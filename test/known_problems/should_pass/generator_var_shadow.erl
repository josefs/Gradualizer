-module(generator_var_shadow).
-export([f/2]).

-spec f(boolean(), [integer()]) -> [integer()].
f(X, Xs) ->
    [X || X <- Xs].
