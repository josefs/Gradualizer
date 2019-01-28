-module(int_op).
-export([plusany/2, plusany2/2, plusany3/2,
         divany/2, divany2/2, divany3/2, divany4/2]).

-spec plusany(integer(), any()) -> number().
plusany(X, Y) -> X + Y.

-spec plusany2(integer(), any()) -> integer().
plusany2(X, Y) -> Y + X.

-spec plusany3(integer(), any()) -> integer().
plusany3(X, Y) ->
    A = Y + X,
    A.

-spec divany(integer(), any()) -> integer().
divany(X, Y) -> X div Y.

-spec divany2(integer(), any()) -> integer().
divany2(X, Y) -> Y div X.

-spec divany3(integer(), any()) -> integer().
divany3(X, Y) ->
    A = X div Y,
    A.

-spec divany4(integer(), any()) -> integer().
divany4(X, Y) ->
    A = Y div X,
    A.
