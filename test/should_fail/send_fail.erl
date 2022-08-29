-module(send_fail).

-export([foo/2, bar/2]).

-spec foo(any(), ok) -> nok.
foo(X, Y) ->  X ! Y.

-spec bar(any(), ok) -> nok.
bar(X, Y) ->
    A = X ! Y,
    A.
