-module(type_refinement_2).
-export([guard_prevents_refinement/2]).

-spec guard_prevents_refinement(1..2, boolean()) -> 2.
guard_prevents_refinement(N, Guard) ->
    case N of
        1 when Guard -> 2;
        M            -> M  %% 1 cannot be eliminated
    end.
