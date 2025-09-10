-module(hist).
-export([new/1, update/3]).
-export([test/0]).

%% ignore(Name)
new(Name) ->
    [{Name, inf}].

update(Node, N, History) ->
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


%% Example test cases
test() ->
    H0 = [],                          %% start with empty history
    %% First message from london
    {new, H1} = update(london, 1, H0),
    %% Another new message from london
    {new, H2} = update(london, 2, H1),
    %% Old message from london
    old = update(london, 1, H2),

    %% New node: paris
    {new, H3} = update(paris, 5, H2),
    old = update(paris, 4, H3),

    %% Using new/1 to force a node always old
    Hx = new(tokyo),
    old = update(tokyo, 1, Hx),
    old = update(tokyo, 999, Hx),

    io:format("All tests passed.~n").