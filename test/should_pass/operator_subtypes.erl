
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

-spec pos_op1(pos_integer(), 1..10) -> pos_integer().
pos_op1(X, Y) -> X * Y + 777 bor Y.

-spec nonneg_op1(0..100, non_neg_integer()) -> non_neg_integer() | error.
nonneg_op1(X, Y) -> X + Y * X div Y rem X band Y bor X bxor Y bsl X bsr Y.

-spec neg_op1(neg_integer(), -10..-5 | -2..-1) -> neg_integer() | 0..10.
neg_op1(X, Y) -> X + Y + (-10).

-type word16() :: 0..65535.

-spec word_op1(non_neg_integer()) -> word16().
word_op1(N) -> N rem 65536.

-spec word_op2(word16(), word16()) -> word16().
word_op2(X, Y) -> X band Y bor 32768 bxor Y.

-spec word_op3(word16(), non_neg_integer(), pos_integer()) -> word16().
word_op3(X, Y, Z) -> (X bsr Y) div Z.

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
