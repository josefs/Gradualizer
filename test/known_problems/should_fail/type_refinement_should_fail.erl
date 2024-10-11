-module(type_refinement_should_fail).

-export([guard_prevents_refinement/2,
         guard_prevents_refinement2/1,
         pattern_prevents_refinement/2]).

-spec guard_prevents_refinement(1..2, boolean()) -> 2.
guard_prevents_refinement(N, Guard) ->
    case N of
        1 when Guard -> 2;
        M            -> M  %% 1 cannot be eliminated
    end.

-spec guard_prevents_refinement2(integer()) -> ok.
guard_prevents_refinement2(X) when is_integer(X), X rem 7 == 0 -> ok;
guard_prevents_refinement2(X) -> ok. % X can still be an integer

-spec pattern_prevents_refinement(integer(), any()) -> atom().
pattern_prevents_refinement(X, X)    when is_integer(X) -> ok;
pattern_prevents_refinement(X, {_Y}) when is_integer(X) -> ok;
pattern_prevents_refinement(Inf, _) -> Inf. % Inf can still be an integer
