-module(named_fun_fail).

-export([bar/0, baz/1, sum/1]).

-spec foo(integer()) -> integer().
foo(N) -> N.

bar() -> foo(fun F(0) -> 0; F(X) -> F(X - 1) end).

-spec baz(integer()) -> boolean().
baz(I) ->
    O = I({}),
    O.

-spec sum([integer()]) -> integer().
sum(Ints) ->
    F = fun Sum(Acc, [Int | Rest]) ->
                Sum(Acc + Int, Rest);
            Sum(Acc, []) ->
                Acc
        end,
    F(Ints).
