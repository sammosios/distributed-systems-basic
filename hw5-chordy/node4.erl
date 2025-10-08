-module(node4).
-export([start/1, start/2]).

-define(Timeout, 3000).
-define(Stabilize, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Store = storage:create(),
    Replica = storage:create(),
    Predecessor = nil,
    Next = nil,
    {ok, {Skey, Spid}} = connect(Id, Peer),
    Ref = monitor(Spid),
    Successor = {Skey, Ref, Spid},
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Next, Store, Replica).

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

node(Id, Predecessor, Successor, Next, Store, Replica) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {notify, New} ->
            {Predecessor1, Store1} = notify(New, Id, Predecessor, Store, Replica),
            node(Id, Predecessor1, Successor, Next, Store1, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {status, Pred, Nx} ->
            {Successor1, Next1} = stabilize(Pred, Nx, Id, Successor, Store),
            node(Id, Predecessor, Successor1, Next1, Store, Replica);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added, Replica);
        {replicate, NewStore} ->
            node(Id, Predecessor, Successor, Next, Store, NewStore);
        {replicate, Key, Value} ->
            Replica1 = storage:add(Key, Value, Replica),
            node(Id, Predecessor, Successor, Next, Store, Replica1);
        {replica_size_request, Pred} ->
            Pred ! {replica_size, maps:size(Replica)},
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Next, Merged, Replica);
        {'DOWN', Ref, process, _, _} ->
            io:format("DOWN detected by ~p~n", [Id]),
            {Pred, Succ, Nxt, Store1, Replica1} = down(Ref, Predecessor, Successor, Next, Store, Replica),
            case Succ of
                {_, _, Spid} when is_pid(Spid) ->
                    stabilize(Succ);
                _ -> ok
            end,
            node(Id, Pred, Succ, Nxt, Store1, Replica1);
        store ->
            io:format("~p~n", [Store]),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        status ->
            pretty_print(Id, Predecessor, Successor, Next, Store),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        probe ->
            create_probe(Id, maps:size(Store), maps:size(Replica), Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {probe, Ref, Nodes, T} ->
            forward_probe(Id, maps:size(Store), maps:size(Replica), Ref, Nodes, T, Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        die ->
            io:format("Node ~p is dead!~n", [Id]),
            exit(killed)
            
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            io:format("Node ~p picked up {~p,~p}~n", [Id, Key, Value]),
            Added = storage:add(Key, Value, Store),
            Spid ! {replicate, Key, Value},
            Added;
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

stabilize(Pred, Next, Id, Successor, Store) ->
    {Skey, Sref, Spid} = Successor,
    Spid ! {replica_size_request, self()},
    LocalStoreSize = maps:size(Store),
    receive
        {replica_size, ReplicaSize} ->
            case ReplicaSize of 
                LocalStoreSize ->
                    % io:format("Replica is synced.~n"),
                    ok; 
                _ ->
                    Spid ! {replicate, Store}
            end;
        _ ->
            {error}
    end,
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

down(Ref, Predecessor, Successor, Next, Store, Replica) ->
    % If predecessor died
    case Predecessor of
        {_Pkey, PrefRef, Ppid} when PrefRef =:= Ref ->
            % predecessor died: drop it
            io:format("Predecessor ~p died~n", [Ppid]),
            drop(Ref),
            % take over their records
            Store1 = storage:merge(Replica, Store),
            Replica1 = storage:create(),
            {_, _, Spid} = Successor,
            Spid ! {replicate, Store1}, 
            {nil, Successor, Next, Store1, Replica1};
        _ ->
            % If successor died
            case Successor of
                {_Skey, Sref, Spid} when Sref =:= Ref ->
                    io:format("Successor ~p died~n", [Spid]),
                    % Try to promote Next to successor (if available)
                    case Next of
                        % {Nkey, Nref, Npid} when is_pid(Npid) ->
                        %     % Next already has a ref (rare). Use it.
                        %     {Predecessor, {Nkey, Nref, Npid}, nil};
                        {Nkey, _, Npid} when is_pid(Npid) ->
                            % Next sent as 2-tuple — create monitor
                            Nref2 = monitor(Npid),
                            Npid ! {replicate, Store},
                            {Predecessor, {Nkey, Nref2, Npid}, nil, Store, Replica};
                        nil ->
                            % No known next — leave successor nil
                            {Predecessor, nil, nil, Store, Replica}
                    end;
                _ ->
                    % Not a monitor we care about
                    {Predecessor, Successor, Next, Store, Replica}
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

notify({Nkey, Npid}, Id, Predecessor, Store, _Replica) ->
    case Predecessor of
        nil ->
            % No predecessor yet → accept N as my predecessor
            % io:format("~p accepts ~p as predecessor~n", [Id, Nkey]),
            Keep = handover(Id, Store, Nkey, Npid),
            Npid ! {status, {Id, self()}},
            % Npid ! {replica_request, self()},
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
                    % Npid ! {replica_request, self()},
                    {{Nkey, Ref, Npid}, Keep};
                false ->
                    % Keep old predecessor
                    Npid ! {status, {Pkey, Ppid}},
                    {{Pkey, Pref, Ppid}, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.


monitor(Pid) ->
    erlang:monitor(process, Pid).
drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).

create_probe(Id, StoreSize, ReplicaSize, Successor) ->
  {_, _, Spid} = Successor,
  Time = erlang:system_time(),
  Spid ! {probe, Id, [{Id, StoreSize, ReplicaSize, self()}], Time}.

forward_probe(Id, StoreSize, ReplicaSize, Ref, Nodes, T, Successor) ->
  {_, _, Spid} = Successor,
  Nodes1 = [{Id, StoreSize, ReplicaSize, self()} | Nodes],
  Spid ! {probe, Ref, Nodes1, T}.

remove_probe(T, Nodes) ->
    Time = erlang:system_time(),
    ProbeDuration = Time - T,
    % Calculate total store size
    TotalItems = lists:sum([StoreSize || {_, StoreSize, _, _} <- Nodes]),
    TotalItemsInReplicas = lists:sum([ReplicaSize || {_, _, ReplicaSize, _} <- Nodes]),
    io:format("Probe finished in ~.3fs~n", [ProbeDuration / 1000000]),
    io:format("Nodes visited (~p total):~n", [length(Nodes)]),
    io:format("~s~n", [string:copies("-", 80)]),
    lists:foreach(
      fun({Id, StoreSize, ReplicaSize, Pid}) ->
          io:format("ID: ~-15w | Store size: ~-6w | Replica size: ~-6w | PID: ~p~n",
                    [Id, StoreSize, ReplicaSize, Pid])
      end,
      Nodes),
    io:format("~s~n", [string:copies("-", 80)]),
    io:format("Total items stored across all nodes: ~p~n", [TotalItems]),
    io:format("Total items stored across all replicas: ~p~n", [TotalItemsInReplicas]),
    ok.


pretty_print(Id, Predecessor, Successor, Next, Store) ->
    io:format("~n===== Node ~p =====~n", [Id]),
    print_tuple("Predecessor", Predecessor),
    print_tuple("Successor", Successor),
    print_tuple("Next", Next),

    io:format("Store Size: ~p~n" , [maps:size(Store)]),
    % io:format("Store:~n"),
    % maps:foreach(
    %   fun(K, V) ->
    %       io:format("  ~p => ~p~n", [K, V])
    %   end,
    %   Store
    % ),
    io:format("====================~n").

print_tuple(Label, {Key, Ref, Pid}) when is_pid(Pid) ->
    io:format("~s: (~p, ~p, ~p)~n", [Label, Key, Ref, Pid]);
print_tuple(Label, undefined) ->
    io:format("~s: undefined~n", [Label]);
print_tuple(Label, nil) ->
    io:format("~s: nil~n", [Label]);
print_tuple(Label, Other) ->
    io:format("~s: ~p~n", [Label, Other]).

