-module(node1).
-export([start/1, start/2]).

-define(Timeout, 5000).
-define(Stabilize, 3000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            % io:format("Node ~p received NOTIFY from ~p~n" ,[Id, New]),
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Id, Ref, Nodes, T, Successor),
            node(Id, Predecessor, Successor);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor)
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            io:format("Notifying Successor ~p of Node ~p~n", [Skey, Id]),
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            % io:format("No action needed.~n", []),
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            % No predecessor yet → accept N as my predecessor
            io:format("~p accepts ~p as predecessor~n", [Id, Nkey]),
            Npid ! {status, {Id, self()}},
            {Nkey, Npid};

        {Pkey, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % N is between P and me → accept N as new predecessor
                    io:format("~p replaces ~p with ~p as predecessor~n", [Id, Pkey, Nkey]),
                    Npid ! {status, {Id, self()}},
                    {Nkey, Npid};
                false ->
                    % Keep old predecessor
                    Npid ! {status, {Pkey, Ppid}},
                    {Pkey, Ppid}
            end
    end.


create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Time = erlang:system_time(),
  Spid ! {probe, Id, [Id], Time}.

forward_probe(Id, Ref, Nodes, T, Successor) ->
  {_, Spid} = Successor,
  Nodes1 = [Id | Nodes],
  Spid ! {probe, Ref, Nodes1, T}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(),
  ProbeDuration = Time - T,
  io:format("Probe finished in ~p~n", [ProbeDuration]),
  io:format("Passed through ~p~n", [Nodes]),
  ok.