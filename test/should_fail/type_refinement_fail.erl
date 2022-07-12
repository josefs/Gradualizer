-module(type_refinement_fail).
-export([guard_prevents_refinement/2, imprecision_prevents_refinement/2,
         multi_pat_fail_1/2,
         guard_prevents_refinement2/1,
         pattern_prevents_refinement/2]).

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

-spec guard_prevents_refinement2(erlang:timestamp()) -> ok.
guard_prevents_refinement2(X) when is_integer(X), X rem 7 == 0 -> ok;
guard_prevents_refinement2(infinity) -> ok. % can still be an integer

-spec pattern_prevents_refinement(erlang:timestamp(), any()) -> atom().
pattern_prevents_refinement(X, X)    when is_integer(X) -> ok;
pattern_prevents_refinement(X, {_Y}) when is_integer(X) -> ok;
pattern_prevents_refinement(Inf, _) -> Inf. % Inf can still be an integer
