-module(not_included).

-compile([export_all]).

-spec c(boolean()) -> integer().
c(X) ->
    X.
