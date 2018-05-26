-module(branch).

-compile([export_all]).

-spec c(boolean()) -> integer().
c(X) ->
    X.
