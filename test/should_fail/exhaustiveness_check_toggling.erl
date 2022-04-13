-module(exhaustiveness_check_toggling).

-export([f/1]).

-type adt() :: {case_a, string()}
             | {case_b, integer()}
             | case_c.

-spec f(adt()) -> atom().
f(ADT) ->
    case ADT of
        {case_a, List} ->
            case List of
                %% This pattern should disable exhaustiveness checking for the inner case expr.
                [$a | _] ->
                    nonexhaustive_list_pattern;
                [] ->
                    ok
            end;
        {case_b, _} ->
            ok
        %% But here, in the outer case expr, case_c is not matched on.
        %% It should be reported as not exhausted by the outer case expr.
    end.
