-module(call_intersection_function_with_union_arg_should_pass).

-export([g/2]).

-type t() :: t1 | t2.
-type u() :: u1 | u2.

-spec g(t(), u()) -> ok.
g(T, U) ->
    g_(T, U).

-spec g_(t1, u1) -> ok;
        (t2, u2) -> ok.
g_(t1, u1) -> ok;
g_(t2, u2) -> ok.
