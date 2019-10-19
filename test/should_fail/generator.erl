-module(generator).

-export([f/1, g/1]).

-spec f(integer()) -> list().
f(N) ->
    [ X || X <- N].

-spec g(integer()) -> list().
g(N) ->
    L = [ X || X <- N],
    L.
