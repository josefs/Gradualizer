-module(exhaustiveness_variable_rebinding_in_case_of).

-compile([export_all, nowarn_export_all]).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).

%% Currently results in:
%%   Nonexhaustive patterns:
%%   Example values which are not covered: -1
-spec neg_integer(integer()) -> ok.
neg_integer(Number) ->
    case Number of
        0 ->
            ok;
        Number ->
            ok
    end.
