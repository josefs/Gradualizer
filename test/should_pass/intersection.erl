-module(intersection).

-compile([export_all]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

g() ->
    f(true).

-spec h() -> boolean().
h() ->
    f(false).
