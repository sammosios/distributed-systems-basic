-module(node2).
-export([start/1, start/2]).

-define(Timeout, 5000).
-define(Stabilize, 3000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Store = storage:create(),
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Store).

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

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            % io:format("Node ~p received NOTIFY from ~p~n" ,[Id, New]),
            {Predecessor1, Store1} = notify(New, Id, Predecessor, Store),
            node(Id, Predecessor1, Successor, Store1);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            Successor1 = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Successor1, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);
        store ->
            io:format("~p~n", [Store]),
            node(Id, Predecessor, Successor, Store);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Id, Ref, Nodes, T, Successor),
            node(Id, Predecessor, Successor, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store)
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            io:format("Notifying Successor ~p of Node ~p~n", [Skey, Id]),
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            % io:format("Ring is stable~n", []),
            Successor;
        {Skey, _} ->
            io:format("Notifying Successor ~p of Node ~p~n", [Skey, Id]),
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {notify, {Id, self()}},
                    {Xkey, Xpid};
                false ->
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % No predecessor yet → accept N as my predecessor
            io:format("~p accepts ~p as predecessor~n", [Id, Nkey]),
            Keep = handover(Id, Store, Nkey, Npid),
            Npid ! {status, {Id, self()}},
            {{Nkey, Npid}, Keep};

        {Pkey, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % N is between P and me → accept N as new predecessor
                    io:format("~p replaces ~p with ~p as predecessor~n", [Id, Pkey, Nkey]),
                    Keep = handover(Id, Store, Nkey, Npid),
                    Npid ! {status, {Id, self()}},
                    {{Nkey, Npid}, Keep};
                false ->
                    % Keep old predecessor
                    Npid ! {status, {Pkey, Ppid}},
                    {{Pkey, Ppid}, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Time = erlang:system_time(),
  Spid ! {probe, Id, [{Id, self()}], Time}.

forward_probe(Id, Ref, Nodes, T, Successor) ->
  {_, Spid} = Successor,
  Nodes1 = [{Id, self()} | Nodes],
  Spid ! {probe, Ref, Nodes1, T}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(),
  ProbeDuration = Time - T,
  io:format("Probe finished in ~p~n", [ProbeDuration]),
  io:format("Passed through ~p~n", [Nodes]),
  ok.