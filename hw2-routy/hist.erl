-module(hist).
-export([new/1, update/3]).

%% ignore(Name)
new(Name) ->
    [{Name, inf}].

update(Node, N, History) ->
    Nodes = [Nod || {Nod, _} <- History],
    case lists:keyfind(Node, 1, History) of
        false -> 
            {new, [{Node, N} | History]};
        {Node, OldN} -> 
            if N > OldN -> 
                {new, lists:keyreplace(Node, 1, History, {Node, N})};
               true -> 
                old
            end
    end.