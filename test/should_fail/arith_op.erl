-module(arith_op).
-export([failplus/1, faildivvar/1, faildivlit/1, failpositivedivision/1,
         faildivprecise/1, failplusprecise/2, failminusprecisepos/2,
         failminusnonneg/2, failminuspreciseneg/2, failplusarg/2,
         faildivarg/2, failplusarg2/2, faildivarg2/2,
         floatdiv/2]).

-spec failplus(_) -> tuple().
failplus(X) -> X + X.

-spec faildivvar(_) -> boolean().
faildivvar(X) -> X div X.

-spec faildivlit(boolean()) -> any() | boolean().
faildivlit(X) -> X div 2.

-spec failpositivedivision(integer()) -> neg_integer().
failpositivedivision(X) -> X / X.

-spec faildivprecise(1..10) -> 1..10 | atom.
faildivprecise(X) -> X div X.

-spec failplusprecise(5, 2 | 4) -> 7 | 9.
failplusprecise(X, Y) -> X + Y.

-spec failminusprecisepos(pos_integer(), neg_integer()) -> pos_integer().
failminusprecisepos(X, Y) -> X - Y.

-spec failminusnonneg(non_neg_integer(), neg_integer()) -> non_neg_integer().
failminusnonneg(X, Y) -> X - Y.

-spec failminuspreciseneg(neg_integer(), non_neg_integer()) -> neg_integer().
failminuspreciseneg(X, Y) -> X - Y.

-spec failplusarg(one, integer()) -> integer().
failplusarg(X, Y) -> X + Y.

-spec faildivarg(integer(), zero) -> integer().
faildivarg(X, Y) -> X div Y.

-spec failplusarg2(one, integer()) -> integer().
failplusarg2(X, Y) ->
    A = X + Y,
    A.

-spec faildivarg2(integer(), zero) -> integer().
faildivarg2(X, Y) ->
    A = X div Y,
    A.

-spec floatdiv(integer(), integer()) -> integer().
floatdiv(X, Y) ->
    A = X / Y,
    A.
