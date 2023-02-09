-module(call_intersection_function_with_union_arg_pass).

-export([f1/1,
         f2/1,
         g1/1,
         g2/1,
         i1/2,
         k1/2,
         k2/2]).

-spec f1(a) -> a.
f1(V) ->
    h(V).

-spec f2(a | b) -> a | b.
f2(V) ->
    h(V).

-spec g1(a) -> a.
g1(V) ->
    A = h(V),
    A.

-spec g2(a | b) -> a | b.
g2(V) ->
    A = h(V),
    A.

-spec h(a) -> a;
       (b) -> b.
h(V) -> V.

-type t() :: t1 | t2.
-type u() :: u1 | u2.

-spec i1(t2, u2) -> two.
i1(T, U) ->
    j(T, U).

-spec j(t1, u1) -> one;
       (t2, u2) -> two.
j(t1, u1) -> one;
j(t2, u2) -> two.

-spec k1(_, _) -> any().
k1(T, U) ->
    j(T, U).

%% intentionally no spec
k2(T, U) ->
    j(T, U).
