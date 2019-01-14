-module(intersection_check).

-export([h/0]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

-spec h() -> {}.
h() ->
    f(false).
