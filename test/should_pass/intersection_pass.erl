-module(intersection_pass).

-export([f/1, g/0, h/0, num/1]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

g() ->
    f(true).

-spec h() -> boolean().
h() ->
    f(false).

-spec num(1) -> one;
         (2) -> two.
num(1) -> one;
num(2) -> two.
