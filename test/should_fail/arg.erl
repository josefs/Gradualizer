-module(arg).

-compile([export_all]).

-spec f(integer()) -> integer().
f(N) ->
    N.

-spec g(bool()) -> bool().
g(N) ->
    f(N).
