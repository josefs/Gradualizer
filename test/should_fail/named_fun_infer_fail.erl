-module(named_fun_infer_fail).

-gradualizer(infer).
-export([bar/0, sum/1]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() ->
    A = fun F(0) -> 0; F(X) -> F(X - 1) end,
    foo(A).

-spec sum([integer()]) -> integer().
sum(Ints) ->
    F = fun Sum(Acc, [Int | Rest]) ->
                Sum(Acc + Int, Rest);
            Sum(Acc, []) ->
                Acc
        end,
    F(Ints).
