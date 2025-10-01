-module(gms4).
-export([start/1, init/3, start/2, init/4]).
-export([leader/5, slave/7]).
-export([drop/0]).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader | Slaves], Group} ->
      io:format("WORKER ~p: received initial view from LEADER ~p~n", [Id, Leader]),
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      Last = {view, N, [Leader | Slaves], Group},
      slave(Id, Master, Leader, N+1, Last, Slaves, Group)
  after 3000 ->
      io:format("WORKER ~p: timeout waiting for initial view~n", [Id]),
      {error, timeout}
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("LEADER ~p broadcasting ~p - ~p to ~p~n", [Id, N, Msg, Slaves]),
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group);
    {join, Wrk, Peer} ->
      % io:format("LEADER ~p received join from ~p via ~p~n", [Id, Wrk, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      % io:format("SLAVE ~p FORWARDING join from ~p to LEADER~p~n", [Id, Wrk, Leader]),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      Master ! Msg,
      Last1 = {msg, N, Msg},
      slave(Id, Master, Leader, N+1, Last1, Slaves, Group);
    {msg, I, _} when I > N + 1 ->
      io:format("SLAVE ~p: is out of sync! requesting state...~n", [Id]),
      Leader ! {mcast, {state_request, self()}},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->
      io:format("SLAVE ~p: ignoring old message with N=~p~n", [Id, I]),
      % ignore old messages
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, N, [Leader | Slaves2], Group2} ->
      Master ! {view, Group2},
      Last1 = {view, N, [Leader | Slaves2], Group2},
      slave(Id, Master, Leader, N+1, Last1, Slaves2, Group2);
    {view, I, _} when I < N ->
      io:format("SLAVE ~p: ignoring old view with N=~p~n", [Id, I]),
      % ignore old views
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    % Case where new leader sends view before worker detects failure
    % of old leader
    {view, [NotTheLeader, Slaves1], Group2} ->
      % refuse to handle a view with a different leader
      io:format("SLAVE ~p: received view with different leader ~p, ignoring~n", [Id, NotTheLeader]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      % io:format("SLAVE ~p: LEADER ~p DOWN, starting election~n", [Id, Leader]),
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> maybe_send(Id, Msg, Node), crash(Id) end, Nodes).


maybe_send(Id, Msg, Node) ->
    case drop() of
        true ->
            io:format("leader ~p: message to ~p dropped~n", [Id, Node]);
        false ->
            Node ! Msg
    end.

drop() ->
  Rate = 200,
  case random:uniform(Rate) of
    Rate -> true;
    _ -> false
end.


crash(Id) ->
  Rate = 1000,
  case random:uniform(Rate) of
    Rate ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
end.

election(Id, Master, N, Last, Slaves, Group) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("WORKER ~p has assumed the role of LEADER~n", [Id]),
      bcast(Id, {view, N, Rest, Group}, Rest),
      bcast(Id, Last, Rest),
      Master ! {view, Group},
      leader(Id, Master, N, Rest, Group);
    [Leader|Rest] ->
      % io:format("WORKER ~p: new LEADER is ~p~n", [Id, Leader]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
end.