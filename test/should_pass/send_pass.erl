-module(send_pass).

-export([foo/2, bar/2]).

-spec foo(any(), ok) -> ok.
foo(X, Y) ->  X ! Y.

-spec bar(any(), ok) -> ok.
bar(X, Y) ->
    A = X ! Y,
    A.
