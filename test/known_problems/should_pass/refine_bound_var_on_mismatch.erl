-module(refine_bound_var_on_mismatch).

%% Note: Here we're refining an already bound variable

-export([refined_var_not_matching_itself/1,
         refine_bound_var_by_pattern_mismatch/1]).

%% Current error: Var is expected to have type y | z but has type x | y | z
-spec refined_var_not_matching_itself(x | y | z) -> ok.
refined_var_not_matching_itself(Var) ->
    case Var of
        x   -> ok;
        Var -> ok
    end.

%% Current error: Var is expected to have type ok but it has type ok | nok
-spec refine_bound_var_by_pattern_mismatch(ok | nok) -> ok.
refine_bound_var_by_pattern_mismatch(Var) ->
    case Var of
        nok -> ok;
        _   -> Var
    end.
