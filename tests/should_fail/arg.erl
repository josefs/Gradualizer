-module(arg).

-compile([export_all]).

-spec f(int()) -> int().
f(N) ->
    N.

-spec g(bool()) -> bool().
g(N) ->
    f(N).
