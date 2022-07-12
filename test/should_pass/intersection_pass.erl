-module(intersection_pass).

-compile([export_all, nowarn_export_all]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

g() ->
    f(true).

-spec h() -> boolean().
h() ->
    f(false).
