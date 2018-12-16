-module(tuple_union_refinement).
-export([fail_1/1]).

%% refine({a, b | c} | {b, c}, {b, c}) -> {a, b | c}.
-spec fail_1({a, b | c} | {b, c}) -> {a, b}.
fail_1({b, c}) -> {a, b};
fail_1(Tuple) -> Tuple.
