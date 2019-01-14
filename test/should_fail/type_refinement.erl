-module(type_refinement).
-export([guard_prevents_refinement/2, imprecision_prevents_refinement/2,
         multi_pat_fail_1/2]).

-spec guard_prevents_refinement(1..2, boolean()) -> 2.
guard_prevents_refinement(N, Guard) ->
    case N of
        1 when Guard -> 2;
        M            -> M  %% 1 cannot be eliminated
    end.

-spec imprecision_prevents_refinement(float(), a|b) -> b.
imprecision_prevents_refinement(3.14, a) -> b;
imprecision_prevents_refinement(_, X) -> X.

-spec multi_pat_fail_1(a|b, a|b) -> {b, b}.
multi_pat_fail_1(a, a) -> {b, b};
multi_pat_fail_1(A, B) -> {A, B}. %% Not only {b, b} here
