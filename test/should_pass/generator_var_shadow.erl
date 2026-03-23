-module(generator_var_shadow).

-compile([export_all, nowarn_export_all]).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).

-spec f(boolean(), [integer()]) -> [integer()].
f(X, Xs) ->
    [X || X <- Xs].
