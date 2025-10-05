-module(node).
-export([start/1, start/2]).

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
      Peer ! {notify, {Id, self()}};
    after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
  end.


stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      io:format("Notifying Successor ~p of Node ~p~n" ,[Skey, Id])
      Spid ! {notify, {Id, self()}};
    {Id, _} ->
      io:format("No action needed.")
    {Skey, _} ->
      io:format("Notifying Successor ~p of Node ~p~n" ,[Skey, Id])
      Spid ! {notify, {Id, self()}};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {notify, {Id, self()}}; 
        false ->
          Spid ! {notify, {Id, self()}}
      end
end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize ->
  stabilize(Successor),
  node(Id, Predecessor, Successor);

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
      Npid ! {status, {Nkey, Npid}};
    {Pkey, Ppid} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Npid ! {status, {Nkey, Npid}};
        false ->
          Npid ! {status, {Pkey, Ppid}};
  end
end.