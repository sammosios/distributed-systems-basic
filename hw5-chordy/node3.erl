-module(node3).
-export([start/1, start/2]).

-define(Timeout, 3000).
-define(Stabilize, 2000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Store = storage:create(),
    Predecessor = nil,
    Next = nil,
    {ok, {Skey, Spid}} = connect(Id, Peer),
    Ref = monitor(Spid),
    Successor = {Skey, Ref, Spid},
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Next, Store).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Peer ! {notify, {Id, self()}},
            {ok, {Skey, Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n", []),
            {error, timeout}
    end.

node(Id, Predecessor, Successor, Next, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store);
        {notify, New} ->
            % io:format("Node ~p received NOTIFY from ~p~n" ,[Id, New]),
            {Predecessor1, Store1} = notify(New, Id, Predecessor, Store),
            node(Id, Predecessor1, Successor, Next, Store1);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        {status, Pred, Nx} ->
            {Successor1, Next1} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Successor1, Next1, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Next, Merged);
        {'DOWN', Ref, process, _, _} ->
            io:format("DOWN detected by ~p~n", [Id]),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            case Succ of
                {_, _, Spid} when is_pid(Spid) ->
                    stabilize(Succ);
                _ -> ok
            end,
            node(Id, Pred, Succ, Nxt, Store);
        store ->
            io:format("~p~n", [Store]),
            node(Id, Predecessor, Successor, Next, Store);
        status ->
            pretty_print(Id, Predecessor, Successor, Next, Store),
            node(Id, Predecessor, Successor, Next, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Id, Ref, Nodes, T, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Next, Store);
        die ->
            io:format("Node ~p is dead!~n", [Id]),
            exit(killed)
            
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            io:format("Node ~p picked up {~p,~p}~n", [Id, Key, Value]),
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

stabilize(Pred, Next, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            % io:format("Notifying Successor ~p of Node ~p~n", [Skey, Id]),
            Spid ! {notify, {Id, self()}},
            Spid ! {request, self()},
            receive
                {status, _Pred, Next1} ->
                    {Successor, Next1}
                after ?Timeout ->
                    {error, "timeout"}
            end;
        {Id, _} ->
            % io:format("Ring is stable~n", []),
            {Successor, Next};
        {Skey, _, _} ->
            % io:format("Notifying Successor ~p of Node ~p~n", [Skey, Id]),
            Spid ! {notify, {Id, self()}},
            Spid ! {request, self()},
            receive
                {status, _Pred, Next1} ->
                    {Successor, Next1}
                after ?Timeout ->
                    {error, "timeout"}
            end;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {notify, {Id, self()}},
                    Xpid ! {request, self()},
                        receive
                            {status, _Pred, Next1} ->
                                drop(Sref),
                                Ref = monitor(Xpid),
                                {{Xkey, Ref, Xpid}, Next1}
                            after ?Timeout ->
                                {error, "timeout"}
                        end;
                false ->
                    Spid ! {notify, {Id, self()}},
                    Spid ! {request, self()},
                        receive
                            {status, _Pred, Next1} ->
                                {Successor, Next1}
                            after ?Timeout ->
                                {error, "timeout"}
                        end
            end
    end.


% down(Ref, {_, Ref, _}, Successor, Next) ->
%     {nil, Successor, Next};
% down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
%     Nref = monitor(Npid),
%     {Predecessor, {Nkey, Nref, Npid}, nil}.

down(Ref, Predecessor, Successor, Next) ->
    % If predecessor died
    case Predecessor of
        {Pkey, PrefRef, Ppid} when PrefRef =:= Ref ->
            % predecessor died: drop it
            io:format("Predecessor ~p died~n", [Ppid]),
            {nil, Successor, Next};
        _ ->
            % If successor died
            case Successor of
                {Skey, Sref, Spid} when Sref =:= Ref ->
                    io:format("Successor ~p died~n", [Spid]),
                    % Try to promote Next to successor (if available)
                    case Next of
                        {Nkey, Nref, Npid} when is_pid(Npid) ->
                            % Next already has a ref (rare). Use it.
                            {Predecessor, {Nkey, Nref, Npid}, nil};
                        {Nkey, Npid} when is_pid(Npid) ->
                            % Next sent as 2-tuple — create monitor
                            Nref2 = monitor(Npid),
                            {Predecessor, {Nkey, Nref2, Npid}, nil};
                        nil ->
                            % No known next — leave successor nil
                            {Predecessor, nil, nil}
                    end;
                _ ->
                    % Not a monitor we care about
                    {Predecessor, Successor, Next}
            end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, Successor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Successor};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, Successor}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % No predecessor yet → accept N as my predecessor
            % io:format("~p accepts ~p as predecessor~n", [Id, Nkey]),
            Keep = handover(Id, Store, Nkey, Npid),
            Npid ! {status, {Id, self()}},
            Ref = monitor(Npid),
            {{Nkey, Ref, Npid}, Keep};

        {Pkey, Pref, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % N is between P and me 
                    % demonitor previous Pred 
                    drop(Pref),
                    % accept N as new predecessor
                    Ref = monitor(Npid),
                    Keep = handover(Id, Store, Nkey, Npid),
                    Npid ! {status, {Id, self()}},
                    {{Nkey, Ref, Npid}, Keep};
                false ->
                    % Keep old predecessor
                    Npid ! {status, {Pkey, Ppid}},
                    {{Pkey, Pref, Ppid}, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
    erlang:monitor(process, Pid).
drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).

create_probe(Id, Successor) ->
  {_, _, Spid} = Successor,
  Time = erlang:system_time(),
  Spid ! {probe, Id, [{Id, self()}], Time}.

forward_probe(Id, Ref, Nodes, T, Successor) ->
  {_, _, Spid} = Successor,
  Nodes1 = [{Id, self()} | Nodes],
  Spid ! {probe, Ref, Nodes1, T}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(),
  ProbeDuration = Time - T,
  io:format("Probe finished in ~ps~n", [ProbeDuration / 1000000]),
  io:format("Passed through ~p~n", [Nodes]),
  ok.

pretty_print(Id, Predecessor, Successor, Next, Store) ->
    io:format("~n===== Node ~p =====~n", [Id]),
    print_tuple("Predecessor", Predecessor),
    print_tuple("Successor", Successor),
    print_tuple("Next", Next),

    io:format("Store:~n"),
    maps:foreach(
      fun(K, V) ->
          io:format("  ~p => ~p~n", [K, V])
      end,
      Store
    ),
    io:format("====================~n").

print_tuple(Label, {Key, Ref, Pid}) when is_pid(Pid) ->
    io:format("~s: (~p, ~p, ~p)~n", [Label, Key, Ref, Pid]);
print_tuple(Label, undefined) ->
    io:format("~s: undefined~n", [Label]);
print_tuple(Label, nil) ->
    io:format("~s: nil~n", [Label]);
print_tuple(Label, Other) ->
    io:format("~s: ~p~n", [Label, Other]).

