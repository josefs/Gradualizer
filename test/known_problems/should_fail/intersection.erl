-module(intersection).
-export([j/1]).

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.

-spec j({d, b} | {a, e}) -> {a, b} | {d, e}.
j({V, U}) -> i(V, U).
