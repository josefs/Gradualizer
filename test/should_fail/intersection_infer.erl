-module(intersection_infer).

-export([g/0]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

g() ->
    f({}).
