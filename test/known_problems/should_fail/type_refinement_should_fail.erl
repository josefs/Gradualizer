-module(type_refinement_should_fail).

-export([pattern_prevents_refinement/2]).

-spec guard_prevents_refinement(1..2, boolean()) -> 2.
guard_prevents_refinement(N, Guard) ->
    case N of
        1 when Guard -> 2;
        M            -> M  %% 1 cannot be eliminated
    end.

-spec guard_prevents_refinement2(erlang:timestamp()) -> ok.
guard_prevents_refinement2(X) when is_integer(X), X rem 7 == 0 -> ok;
guard_prevents_refinement2(infinity) -> ok. % can still be an integer

-spec pattern_prevents_refinement(erlang:timestamp(), any()) -> atom().
pattern_prevents_refinement(X, X)    when is_integer(X) -> ok;
pattern_prevents_refinement(X, {_Y}) when is_integer(X) -> ok;
pattern_prevents_refinement(Inf, _) -> Inf. % Inf can still be an integer
