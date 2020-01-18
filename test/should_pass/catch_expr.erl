-module(catch_expr).

-export([foo/1]).

-spec foo(ok) -> ok.
foo(X) ->
    A = (catch X),
    A.
