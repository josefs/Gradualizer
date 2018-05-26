-module(int).

-compile([export_all]).

-spec f(integer()) -> integer().
f(N) ->
    N.

-spec g(integer()) -> integer().
g(1) ->
    2.
