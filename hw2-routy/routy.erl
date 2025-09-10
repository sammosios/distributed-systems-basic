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
  io:format("router: ~p, N=~p, history=~p, intf=~p, table=~p routes, map=~p nodes~n",
            [Name, N, length(Hist), intf:list(Intf), length(Table), map:all_nodes(Map)]),
  receive
    {add, Node, Pid} ->
      io:format("routy: ~p received {add, ~p, ~p}~n", [Name, Node, Pid]),
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      io:format("routy: ~p received {remove, ~p}~n", [Name, Node]),
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {route, Name, _From, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
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
      io:format("~w: DOWN recived from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
      io:format("routy: ~p received {status, ~p}~n", [Name, From]),
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    
    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
      {new, Hist1} ->
        intf:broadcast({links, Node, R, Links}, Intf),
        NewMap = map:update(Node, Links, Map),
        router(Name, N, Hist1, Intf, Table, NewMap);
      old ->
        router(Name, N, Hist, Intf, Table, Map)
      end;

    update ->
      io:format("routy: ~p received update~n", [Name]),
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
      io:format("routy: ~p received broadcast~n", [Name]),
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      NewMap = map:update(Name, intf:list(Intf), Map),
      router(Name, N+1, Hist, Intf, Table, NewMap);

    stop ->
      io:format("routy: ~p received stop~n", [Name]),
      ok
  end.
