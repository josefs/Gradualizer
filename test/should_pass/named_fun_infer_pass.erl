-module(named_fun_infer_pass).

-gradualizer(infer).
-export([atom_sum/1]).

%% Documents expected behaviour that the type of parameters are not inferred
%% when infer is on in do_type_check_expr.
-spec atom_sum([atom()]) -> integer().
atom_sum(Atoms) ->
    F = fun Sum(Acc, [Int | Rest]) ->
                Sum(Acc + Int, Rest);
            Sum(Acc, []) ->
                Acc
        end,
    F(Atoms, 0).
