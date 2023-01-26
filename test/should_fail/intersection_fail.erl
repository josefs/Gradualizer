-module(intersection_fail).

-export([k/1]).

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.

-spec k({a, e} | {d, b}) -> {a, e} | {d, b}.
k({V, U}) -> i(V, U).
