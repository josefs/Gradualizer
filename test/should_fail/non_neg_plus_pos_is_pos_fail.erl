-module(non_neg_plus_pos_is_pos_fail).

-export([f/1, g/1]).

-spec f(neg_integer()) -> pos_integer().
f(N) ->
    h(N + 1).

-spec g(neg_integer()) -> pos_integer().
g(N) ->
    h(1 + N).

-spec h(pos_integer()) -> pos_integer().
h(P) -> P.
