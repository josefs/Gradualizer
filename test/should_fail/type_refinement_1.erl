-module(type_refinement_1).
-export([multi_pat_fail_1/2]).

-spec multi_pat_fail_1(a|b, a|b) -> {b, b}.
multi_pat_fail_1(a, a) -> {b, b};
multi_pat_fail_1(A, B) -> {A, B}. %% Not only {b, b} here
