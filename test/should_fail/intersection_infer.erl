-module(intersection_infer).

-compile([export_all]).

-spec f(integer()) -> integer();
       (boolean()) -> boolean().
f(X) ->
    X.

g() ->
    f({}).
