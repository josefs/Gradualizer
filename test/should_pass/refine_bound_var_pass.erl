-module(refine_bound_var_pass).

%% Note: Here we're refining an already bound variable

-export([refined_var_not_matching_itself/1]).

-spec refined_var_not_matching_itself(x | y | z) -> ok.
refined_var_not_matching_itself(Var) ->
    case Var of
        x   -> ok;
        Var -> ok
    end.
