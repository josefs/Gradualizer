-module(arith_op).

-export([failplus/1,
         faildivvar/1,
         int_error/2]).

-spec failplus(_) -> tuple().
failplus(X) -> X + X.

-spec faildivvar(_) -> boolean().
faildivvar(X) -> X div X.

-spec int_error(any(), float()) -> integer().
int_error(X, Y) ->
    A = X div Y,
    A.
