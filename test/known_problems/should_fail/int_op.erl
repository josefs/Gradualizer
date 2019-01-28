-module(int_op).
-export([plusfloatany/2, divfloatany/2]).

-spec plusfloatany(float(), any()) -> integer().
plusfloatany(X, Y) ->
    A = X + Y,
    A.

-spec divfloatany(float(), any()) -> integer() | any().
divfloatany(X, Y) -> X div Y.

