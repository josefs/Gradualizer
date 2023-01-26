-module(intersection).

-export([i/2,
         j/1]).

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.

-spec j({a, b} | {d, e}) -> {a, b} | {d, e}.
j({V, U}) -> i(V, U).
