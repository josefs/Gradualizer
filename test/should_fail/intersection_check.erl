-module(intersection_check).

-compile([export_all]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

-spec h() -> {}.
h() ->
    f(false).
