-module(dijkstra).
-export([entry/2, table/2, route/2, iterate/3, replace/4, update/4]).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false -> 
            0;
        {Node, Distance, _Gateway} -> 
            Distance
    end.


replace(Node, N, Gateway, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false -> 
            Sorted;
        _ -> 
            NewSorted = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
            lists:keysort(2, NewSorted)
    end.

update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 -> 
            Sorted;
        Distance -> 
            if N < Distance -> 
                replace(Node, N, Gateway, Sorted);
               true -> 
                Sorted
            end
    end.

iterate(Sorted, Map, Table) ->
    case Sorted of
        [] ->
            Table;

        [{_Node, inf, _Gateway} | _] ->
            % First entry is infinity => everything left is unreachable
            Table;

        [{Node, Distance, Gateway} | Rest] ->
            case lists:keyfind(Node, 1, Table) of
                {Node, _, _} ->
                    iterate(Rest, Map, Table);
                false ->
                    Neighbors = map:reachable(Node, Map), % [neighbor1, neighbor2], ...]
                    NewSorted =
                        lists:foldl(
                          fun(Neighbor, Acc) ->
                                  NewDistance = Distance + 1,
                                  % Use the current node's gateway as the first hop
                                  NextHop = Gateway,  
                                  update(Neighbor, NewDistance, NextHop, Acc)
                          end,
                          Rest,
                          Neighbors
                        ),
                    

                    NewTable =
                        case lists:keyfind(Node, 1, Table) of
                            false -> [{Node, Gateway} | Table];
                            _     -> lists:keyreplace(Node, Table, {Node, Gateway})
                        end,
                    iterate(NewSorted, Map, NewTable)
            end
    end.

table(Gateways, Map) ->
    Nodes = map:all_nodes(Map),
    InitialSorted = 
        lists:map(
            fun(Node) ->
                    case lists:member(Node, Gateways) of
                        true -> {Node, 0, Node};  % Distance 0, Gateway is itself
                        false ->    {Node, inf, undefined}  % Distance infinity, no gateway
                    end
            end,
            Nodes 
        ),
    Sorted = lists:keysort(2, InitialSorted),
    iterate(Sorted, Map, []).

route(Node, Table) ->
    TableNodes = [N || {N, _} <- Table],
    io:format("dijkstra:route(~p, Table=~p)~n", [Node, TableNodes]),
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} -> 
            {ok, Gateway};
        false -> 
            notfound
    end.