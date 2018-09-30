-module(rebar3_example).

-compile([export_all]).

-spec c(boolean()) -> boolean().
c(X) ->
    X.
