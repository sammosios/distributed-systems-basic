-module(map).
-export([new/0, all_nodes/1, reachable/2, update/3]).

%% Create an empty map
new() ->
    [].

%% Add or update a node with its links
update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
        false ->
            [{Node, Links} | Map];
        _ ->
            lists:keyreplace(Node, 1, Map, {Node, Links})
    end.

%% Return neighbors of a node, or [] if not found
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, Neighbors} -> Neighbors;
        false -> []
    end.

%% Return all nodes in the map
all_nodes(Map) ->
    % 1. Create a function that takes a tuple,
    %    and returns a new list with the Key prepended to the ValueList.
    %% e.g. {a, [b, c]} -> [a, b, c]
    ExtractNodes = fun({Key, ValueList}) ->
                        [Key | ValueList] 
                   end,
    
    % 2. Use lists:flatmap to apply that function to every tuple
    %    in the RouterMap list. This creates a single, flattened list.
    FlattenedNodes = lists:flatmap(ExtractNodes, Map),
    
    % 3. Finally, use lists:usort to remove duplicates from the flattened list.
    lists:usort(FlattenedNodes).
