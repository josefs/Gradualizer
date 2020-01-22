-module(arith_op).
-export([int_error/2]).

-spec int_error(any(), float()) -> integer().
int_error(X, Y) ->
    A = X div Y,
    A.
