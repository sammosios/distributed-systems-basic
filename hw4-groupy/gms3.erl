-module(gms3).
-export([start/1, init/3, start/2, init/4]).
-export([leader/5, slave/7, election/6]).

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
    {view, [Leader | Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, 0, "", Slaves, Group)
  after 3000 ->
      {error, timeout}
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      N1 = N+1,
      bcast(Id, {msg, N1, Msg}, Slaves),
      io:format("LEADER ~p broadcasting ~p to ~p~n", [Id, Msg, Slaves]),
      Master ! Msg,
      leader(Id, Master, N1, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("LEADER ~p received join from ~p via ~p~n", [Id, Wrk, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      N1 = N+1,
      bcast(Id, {view, N1, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N1, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("SLAVE ~p FORWARDING join from ~p to LEADER~p~n", [Id, Wrk, Leader]),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      Master ! Msg,
      Last1 = Msg,
      N1 = N+1,
      slave(Id, Master, Leader, N1, Last1, Slaves, Group);
    {msg, I, _} when I < N ->
      %% duplicate message, ignore
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, [Leader | Slaves2], Group2} ->
      Master ! {view, Group2},
      Last1 = {view, [Leader | Slaves2], Group2},
      N1 = N+1,
      slave(Id, Master, Leader, N1, Last1, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("SLAVE ~p: LEADER ~p DOWN, starting election~n", [Id, Leader]),
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

bcast(Id, Msg, Nodes) ->
  crash(Id),
  lists:foreach(fun(Node) -> Node ! Msg  end, Nodes).

crash(Id) ->
  Aargh = 100,
  case random:uniform(Aargh) of
    Aargh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("WORKER ~p has assumed the role of LEADER~n", [Id]),
      bcast(Id, Last, Rest),
      Master ! {view, Group},
      leader(Id, Master, N, Rest, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
end.