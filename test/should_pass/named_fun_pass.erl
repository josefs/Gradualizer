-module(named_fun_pass).

-export([fac/1, sum/1]).

-spec fac(integer()) -> integer().
fac(I) ->
    F = fun Fac(0) ->
                1;
            Fac(N) ->
                N*Fac(N-1)
        end,
    F(I).

%% Documents expected behaviour that named_fun gets type any() when
%% infer is off in do_type_check_expr.
-spec sum([integer()]) -> integer().
sum(Ints) ->
    F = fun Sum(Acc, [Int | Rest]) ->
                Sum(Acc + Int, Rest);
            Sum(Acc, []) ->
                Acc
        end,
    F(Ints).
