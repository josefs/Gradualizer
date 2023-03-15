-module(list_op_pass).

-export([append/0,
         subtract/0]).

%% Checking this function used to crash.
%% Now that we approximate type vars for list ops with any, it passes.
append() ->
    lists:foldl(fun(X, A) -> A ++ [X] end, [], []).

%% Checking this function used to crash.
%% Now that we approximate type vars for list ops with any, it passes.
subtract() ->
    lists:foldl(fun(X, A) -> A -- [X] end, [], []).
