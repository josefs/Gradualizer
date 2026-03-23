-module(zip_generators_fail).

%% Each exported function should produce exactly one type error.

-export([zip_not_list/1, zip_wrong_return/0]).

%% Zip generator from non-list value
-spec zip_not_list(integer()) -> [{integer(), integer()}].
zip_not_list(N) ->
    [{X, Y} || X <- [1, 2, 3] && Y <- N].

%% Zip generator result type mismatch
-spec zip_wrong_return() -> integer().
zip_wrong_return() ->
    [{X, Y} || X <- [1, 2] && Y <- [a, b]].
