
-module(operator_subtypes).

-compile([export_all, nowarn_export_all]).

%% Arithmetic operations

-spec arith_op1(integer(), integer()) -> integer() | error.
arith_op1(_, 0) -> error;
arith_op1(X, Y) -> X div Y.

-spec arith_op2(integer()) -> boolean() | _.
arith_op2(X) -> X + X.

-spec float_op1(float(), float()) -> number().
float_op1(X, Y) -> X * Y.

-spec float_op2(integer(), float()) -> float().
float_op2(X, Y) -> X / Y.

-spec nonneg_op1(0..100, non_neg_integer()) -> integer() | error.
nonneg_op1(X, Y) -> X + Y * X div Y rem X band Y bor X bxor Y bsl X bsr Y.

-type word16() :: 0..65535.

%% Logic operations

-spec logic_op1(boolean(), false) -> boolean() | not_boolean.
logic_op1(X, Y) -> X and Y or true.

-spec logic_op2(boolean(), a, boolean() | b) -> {boolean() | a, boolean() | b}.
logic_op2(X, Y, Z) -> {X andalso Y, X orelse Z}.

%% Relational operations

-spec rel_op1(term(), {foo, integer(), atom()}) -> boolean() | other.
rel_op1(other, _) -> other;
rel_op1(X, Y) -> X == Y.

%% List operations

-spec list_op1([integer()], [integer()]) -> [integer() | error] | error.
list_op1(Xs, []) -> [error | Xs];
list_op1([], _)  -> error;
list_op1(Xs, Ys) -> Xs ++ Ys.

-spec list_op2([], []) -> [].
list_op2(Xs, Ys) -> Xs ++ Ys.

-spec list_op3([number()], [integer()]) -> [number()].
list_op3(Xs, Ys) -> Xs -- Ys.

-spec list_op4([], [tuple()]) -> [].
list_op4(Xs, Ys) -> Xs -- Ys.

-spec list_op5([a, ...], [a, ...]) -> [a, ...].
list_op5(Xs, Ys) -> Xs ++ Ys.

-spec list_op6([integer()], maybe_improper_list(integer(), tl)) -> maybe_improper_list(integer(), tl | 5).
list_op6(Xs, Ys) -> Xs ++ Ys.

-spec list_op7([integer(), ...], nonempty_improper_list(integer(), tl)) -> nonempty_improper_list(integer(), tl).
list_op7(Xs, Ys) -> Xs ++ Ys.

-spec list_op8() -> _ | integer().
list_op8() -> [1] ++ [2].

%% Unary operators

-spec unary_op1(boolean(), false) -> {false | error, boolean(), true}.
unary_op1(X, Y) -> {not true, not X, not Y}.

-spec unary_op2(0..10) -> integer().
unary_op2(X) -> - (bnot X).

-spec unary_op3(0..10) -> 1..11.
unary_op3(X) -> - (bnot X).

-spec unary_op4(float()) -> number().
unary_op4(X) -> -X.

