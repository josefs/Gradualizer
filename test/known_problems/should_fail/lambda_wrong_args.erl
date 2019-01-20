-module(lambda_wrong_args).

-export([f/1]).

%% The argument type of F should be inferred to be the same
%% as the argument type of integer_to_binary/1, ie integer().
-spec f(list()) -> binary().
f(Int) ->
    F = fun(I) -> integer_to_binary(I) end,
    F(Int).
