-module(intersection).
-export([i/2]).

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.
