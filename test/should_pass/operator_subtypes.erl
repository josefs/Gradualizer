-module(operator_subtypes).

-export([arith_op1/2,
         arith_op2/1,
         float_op1/2,
         logic_op1/2,
         logic_op2/3,
         rel_op1/2,
         rel_subtype_left/2,
         rel_subtype_right/2,
         list_op1/2,
         list_op2/2,
         list_op3/2,
         list_op5/2,
         list_op8/0,
         unary_op1/2,
         unary_op2/1,
         unary_op4/1,
         unary_op5/1,
         unary_op6/1,
         unary_op7/1,
         unary_op8/1]).

%% Arithmetic operations

-spec arith_op1(integer(), integer()) -> integer() | error.
arith_op1(_, 0) -> error;
arith_op1(X, Y) -> X div Y.

-spec arith_op2(integer()) -> boolean() | _.
arith_op2(X) -> X + X.

-spec float_op1(float(), float()) -> number().
float_op1(X, Y) -> X * Y.

%% Logic operations

-spec logic_op1(boolean(), false) -> boolean() | not_boolean.
logic_op1(X, Y) -> X and Y or true.

-spec logic_op2(boolean(), a, boolean() | b) -> {boolean() | a, boolean() | b}.
logic_op2(X, Y, Z) -> {X andalso Y, X orelse Z}.

%% Relational operations

-spec rel_op1(term(), {foo, integer(), atom()}) -> boolean() | other.
rel_op1(other, _) -> other;
rel_op1(X, Y) -> X == Y.

-spec rel_subtype_left(1..10, integer()) -> boolean().
rel_subtype_left(X, Y) -> X == Y.

-spec rel_subtype_right(integer(), 1..10) -> boolean().
rel_subtype_right(X, Y) -> X == Y.

%% List operations

-spec list_op1([integer()], [integer()]) -> [integer() | error] | error.
list_op1(Xs, []) -> [error | Xs];
list_op1([], _)  -> error;
list_op1(Xs, Ys) -> Xs ++ Ys.

-spec list_op2([], []) -> [].
list_op2(Xs, Ys) -> Xs ++ Ys.

-spec list_op3([number()], [integer()]) -> [number()].
list_op3(Xs, Ys) -> Xs -- Ys.

-spec list_op5([a, ...], [a, ...]) -> [a, ...].
list_op5(Xs, Ys) -> Xs ++ Ys.

-spec list_op8() -> _ | integer().
list_op8() -> [1] ++ [2].

%% Unary operators

-spec unary_op1(boolean(), false) -> {false | error, boolean(), true}.
unary_op1(X, Y) -> {not true, not X, not Y}.

-spec unary_op2(0..10) -> integer().
unary_op2(X) -> - (bnot X).

-spec unary_op4(float()) -> number().
unary_op4(X) -> -X.

-spec unary_op5(0..10) -> integer().
unary_op5(X) ->
    A = bnot X,
    A.

-spec unary_op6(pos_integer()) -> neg_integer().
unary_op6(X) ->
    A = -X,
    A.

-spec unary_op7(neg_integer()) -> pos_integer().
unary_op7(X) ->
    A = -X,
    A.

-spec unary_op8(0 | neg_integer()) -> non_neg_integer().
unary_op8(X) ->
    A = -X,
    A.
