-module(call_intersection_function_with_union_arg_should_pass).

-export([f/1]).

-type t() :: t1 | t2.

-spec f(t()) -> ok.
f(T) ->
    helper(T).

-spec helper(t1) -> ok;
            (t2) -> ok.
helper(t1) -> ok;
helper(t2) -> ok.
