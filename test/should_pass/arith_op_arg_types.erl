-module(arith_op_arg_types).

-export([arg1/0, arg2/0]).

%% The issue is that arith_op_arg_types/2 for '-' and non_neg_integer() as return type is too
%% restricitve.
%% "The integer is expected to have type pos_integer() but it has type 0"
-spec arg1() -> non_neg_integer().
arg1() ->
    0 - 0.

%% "The integer is expected to have type 1 but it has type 2"
-spec arg2() -> non_neg_integer().
arg2() ->
    3 - 2.
