-module(call_intersection_function_with_union_arg_should_pass).

-export([f/1,
         g/2]).

-type t() :: t1 | t2.
-type u() :: u1 | u2.

-spec f(t()) -> ok.
f(T) ->
    f_(T).

-spec f_(t1) -> ok;
        (t2) -> ok.
f_(t1) -> ok;
f_(t2) -> ok.

-spec g(t(), u()) -> ok.
g(T, U) ->
    g_(T, U).

-spec g_(t1, u1) -> ok;
        (t2, u2) -> ok.
g_(t1, u1) -> ok;
g_(t2, u2) -> ok.
