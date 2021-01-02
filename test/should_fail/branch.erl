-module(branch).

-compile([export_all, nowarn_export_all]).

-spec c(boolean()) -> integer().
c(X) ->
    X.
