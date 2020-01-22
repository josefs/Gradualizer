-module(list_op).
-export([append_right_error/1,
         append_left_error/1,
         append/0,
         subtract/1,
         subtract/2,
         subtract/0,
         subtract2/1,
         subtract2/2]).

-spec append_right_error(integer()) -> _ | integer().
append_right_error(X) -> [1] ++ X.

-spec append_left_error(integer()) -> _ | integer().
append_left_error(X) -> X ++ [1].

%% FIXME checking this function used to crash
%% now it returns the following error
%% "The operator '++' is expected to have type _TyVar-* which is too precise to be statically checked"
append() ->
    lists:foldl(fun(X, A) -> A ++ [X] end, [], []).

-spec subtract([a]) -> {ok, [a]}.
subtract(Xs) -> Xs -- Xs.

-spec subtract([a, ...], [a]) -> [a, ...].
subtract(Xs, Ys) -> Xs -- Ys.

-spec subtract2([a]) -> {ok, [a]}.
subtract2(Xs) ->
    A = Xs -- Xs,
    A.

-spec subtract2([a, ...], [a]) -> [a, ...].
subtract2(Xs, Ys) ->
    A = Xs -- Ys,
    A.

%% FIXME checking this function used to crash
%% now it returns the following error
%% "The operator '--' is expected to have type _TyVar-* which is too precise to be statically checked"
subtract() ->
    lists:foldl(fun(X, A) -> A -- [X] end, [], []).
