-module(singleton).

-export([sing/1, atom/1]).

-spec sing(0|1) -> 10..12.
sing(0) ->
    10;
sing(1) ->
    12.

-spec atom(a) -> b.
atom(a) ->
    b.
