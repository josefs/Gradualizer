-module(poly_pass_infer).

-export([
    all_positive/1
]).

-gradualizer([solve_constraints]).

-spec all_positive([integer()]) -> {true, [integer()]} | false.
all_positive(List) ->
    F = fun
        (Num, {true, Positive}) ->
            case Num > 0 of
                true -> {true, [Num | Positive]};
                false -> false
            end;
        (_Num, false) -> false
    end,
    lists:foldl(F, {true, []}, List).
