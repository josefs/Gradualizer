-module(catch_expr_fail).

-export([foo/1]).

-spec foo(nok) -> ok.
foo(X) ->
    A = (catch X),
    A.
