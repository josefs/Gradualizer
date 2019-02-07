-module(list_op).
-export([append/1,
         append/0,
         subtract/1,
         subtract/2,
         subtract/0]).

-spec append(integer()) -> _ | integer().
append(X) -> [1] ++ X.

%% FIXME checking this function used to crash
%% now it returns the following error
%% "The operator '++' is expected to have type _TyVar-* which is too precise to be statically checked"
append() ->
    lists:foldl(fun(X, A) -> A ++ [X] end, [], []).

-spec subtract([a]) -> {ok, [a]}.
subtract(Xs) -> Xs -- Xs.

-spec subtract([a, ...], [a]) -> [a, ...].
subtract(Xs, Ys) -> Xs -- Ys.

%% FIXME checking this function used to crash
%% now it returns the following error
%% "The operator '--' is expected to have type _TyVar-* which is too precise to be statically checked"
subtract() ->
    lists:foldl(fun(X, A) -> A -- [X] end, [], []).
