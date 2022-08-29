-module(record_wildcard_pass).

-export([f/0, g/0, h/1]).

-record(rec, {apa  = 1         :: integer()
             ,bepa = false     :: boolean()
             ,cepa = undefined :: atom()
             }).

f() ->
    #rec{ apa = 1
        , _   = true }.

-spec g() -> #rec{}.
g() ->
    #rec{ apa = 1
        , _   = true }.

%% wildcard in pattern matching
-spec h(#rec{}) -> boolean().
h(#rec{apa = 1, _ = true}) ->
    true;
h(_) ->
    false.
