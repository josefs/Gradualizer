-module(branch).

-compile([export_all]).

-spec c(boolean()) -> int().
c(X) ->
    X.
