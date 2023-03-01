-module(call_intersection_function_with_union_arg_fail).

-export([i1/2,
         i2/2]).

-type t() :: t1 | t2.
-type u() :: u1 | u2.

-spec i1(t1, u2) -> two.
i1(T, U) ->
    j(T, U).

-spec i2(t(), u()) -> one | two.
i2(T, U) ->
    j(T, U).

-spec j(t1, u1) -> one;
       (t2, u2) -> two.
j(t1, u1) -> one;
j(t2, u2) -> two.
