-module(key).
-export([generate/0, between/3]).


generate() ->
  rand:uniform(1000000000).

between(Key, From, To) ->
    case From == To of
        true ->
            true;  % full circle
        false when From < To ->
            Key > From andalso Key =< To;
        false when From > To ->  % wrap-around case
            Key > From orelse Key =< To
    end.


    