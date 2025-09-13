-module(routy).
-export([start/2, stop/1]).

start(Reg, Name) ->
  io:format("routy:start(~p, ~p)~n", [Reg, Name]),
  Pid = spawn(fun() -> init(Name) end),
  register(Reg, Pid),
  Pid.

stop(Node) ->
  io:format("routy:stop(~p)~n", [Node]),
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(intf:list(Intf), Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      io:format("routy: ~p received {add, ~p, ~p}~n", [Name, Node, Pid]),
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      N1 = N + 1,
      {Table1, Map1} = updateAndBroadcast(Name, N1, Intf1, Map),
      router(Name, N1, Hist, Intf1, Table1, Map1);

    {remove, Node} ->
      io:format("routy: ~p received {remove, ~p}~n", [Name, Node]),
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      N1 = N + 1,
      {Table1, Map1} = updateAndBroadcast(Name, N1, Intf1, Map),
      router(Name, N1, Hist, Intf1, Table1, Map1);

    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
      {new, Hist1} ->
        % special case, updateAndBroadcast is not suitable here
        Map1 = map:update(Node, Links, Map),
        Table1 = dijkstra:table(intf:list(Intf), Map1),
        intf:broadcast({links, Node, R, Links}, Intf),
        router(Name, R, Hist1, Intf, Table1, Map1);
      old ->
        router(Name, N, Hist, Intf, Table, Map)
      end;

    {route, Name, From, Message} ->
      io:format("~w: received message ~p from ~p~n", [Name, Message, From]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~p)~n", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              gateway_not_found
          end;
        notfound ->
          dijkstra_failed
      end,
      router(Name, N, Hist, Intf, Table, Map);  

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w: DOWN received from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      N1 = N + 1,
      {Table1, Map1} = updateAndBroadcast(Name, N1, Intf1, Map),
      router(Name, N1, Hist, Intf1, Table1, Map1);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    status ->
      prettyPrint(Name, N, Hist, Intf, Table, Map),
      router(Name, N, Hist, Intf, Table, Map);

    stop ->
      io:format("routy: ~p received stop~n", [Name]),
      ok
  end.

updateAndBroadcast(Node, N, Intf, Map) ->
  Table1 = dijkstra:table(intf:list(Intf), Map1),
  Map1 = map:update(Node, intf:list(Intf), Map),
  Message = {links, Node, N, intf:list(Intf)},
  intf:broadcast(Message, Intf),
  {Table1, Map1}.


prettyPrint(Name, N, Hist, Intf, Table, Map) ->
    io:format("\n***\tStatus Report for ~p\t***~n", [Name]),
    io:format("N=~p, \nHist=~p, \nIntf=~p~n", 
                [N, Hist, Intf]),
      io:format("Routing Table:~n"),
      lists:foreach(
        fun({Node, Gateway}) ->
                io:format("  ~p \t-> ~p~n", [Node, Gateway])
        end,
        Table
      ),
      io:format("Map:~n"),
      io:format("~p~n", [map:all_nodes(Map)]),
      io:format("~n*******************************************~n~n").
