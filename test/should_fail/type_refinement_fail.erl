-module(type_refinement_fail).

-export([imprecision_prevents_refinement/2,
         multi_pat_fail_1/2]).

-spec imprecision_prevents_refinement(float(), a|b) -> b.
imprecision_prevents_refinement(3.14, a) -> b;
imprecision_prevents_refinement(_, X) -> X.

-spec multi_pat_fail_1(a|b, a|b) -> {b, b}.
multi_pat_fail_1(a, a) -> {b, b};
multi_pat_fail_1(A, B) -> {A, B}. %% Not only {b, b} here
