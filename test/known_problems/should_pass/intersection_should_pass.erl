-module(intersection_should_pass).

-export([f/1,
         g/1,
         j/1]).

-spec f(a|b) -> a|b.
f(V) -> h(V).

-spec g(a|b) -> a|b.
g(V) ->
    A = h(V),
    A.

-spec h(a) -> a;
       (b) -> b.
h(V) -> V.

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.

-spec j({a, b} | {d, e}) -> {a, b} | {d, e}.
j({V, U}) -> i(V, U).
