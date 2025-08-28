-module(wait).
-export([hello/0]).

hello() ->
    receive
        {Message, From} ->
            io:format("Whoa! Just received: ~p from ~p~n", [Message, From]),
            From ! "Thank you for your message",
            hello()
    end.
