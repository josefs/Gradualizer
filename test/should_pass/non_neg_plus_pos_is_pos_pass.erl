-module(non_neg_plus_pos_is_pos_pass).

-export([f/1]).

-spec f(non_neg_integer()) -> pos_integer().
f(N) ->
    h(N + 1).

-spec h(pos_integer()) -> pos_integer().
h(P) -> P.