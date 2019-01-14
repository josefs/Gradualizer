-module(arg).

-export([g/1]).

-spec f(integer()) -> integer().
f(N) ->
    N.

-spec g(bool()) -> bool().
g(N) ->
    f(N).
