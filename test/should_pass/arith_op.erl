-module(arith_op).
-export([plusintint/2, plusfloatfloat/2,
         plusintfloat/2, plusfloatint/2,
         plusany/2, plusany2/2, plusany3/2, plusany4/2]).

-spec plusintint(integer(), integer()) -> integer().
plusintint(X, Y) -> X + Y.

-spec plusfloatfloat(float(), float()) -> float().
plusfloatfloat(X, Y) -> X + Y.

-spec plusintfloat(integer(), float()) -> float().
plusintfloat(X, Y) ->
    A = X + Y,
    A.

-spec plusfloatint(float(), integer()) -> float().
plusfloatint(X, Y) ->
    A = X + Y,
    A.

-spec plusany(integer(), any()) -> number().
plusany(X, Y) -> X + Y.

-spec plusany2(integer(), any()) -> integer().
plusany2(X, Y) -> Y + X.

-spec plusany3(integer(), any()) -> integer().
plusany3(X, Y) ->
    A = Y + X,
    A.

-spec plusany4(integer(), any()) -> integer().
plusany4(X, Y) ->
    A = X + Y,
    A.
