-module(catch_expr_pass).

-export([foo/1]).

-spec foo(ok) -> ok.
foo(X) ->
    A = (catch X),
    A.
