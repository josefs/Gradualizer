-module(int_op).
-export([divfloatany/2, plusfloatany/2]).

-spec divfloatany(float(), any()) -> integer() | any().
divfloatany(X, Y) -> X div Y.

-spec plusfloatany(float(), any()) -> integer().
plusfloatany(X, Y) ->
    A = X + Y,
    A.
