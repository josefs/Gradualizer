-module(int).

-compile([export_all]).

-spec f(int()) -> int().
f(N) ->
    N.

-spec g(int()) -> int().
g(1) ->
    2.
