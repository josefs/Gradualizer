-module(operator_subtypes_should_pass).

-export([float_op2/2,
         pos_op1/2,
         nonneg_op1/2,
         neg_op1/2,
         word_op1/1,
         word_op2/2,
         word_op3/3,
         list_op4/2,
         list_op6/2,
         list_op7/2,
         unary_op3/1]).

%% Arithmetic operations

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

%% List operations

-spec list_op4([], [tuple()]) -> [].
list_op4(Xs, Ys) -> Xs -- Ys.

-spec list_op6([integer()], maybe_improper_list(integer(), tl)) -> maybe_improper_list(integer(), tl | 5).
list_op6(Xs, Ys) -> Xs ++ Ys.

-spec list_op7([integer(), ...], nonempty_improper_list(integer(), tl)) -> nonempty_improper_list(integer(), tl).
list_op7(Xs, Ys) -> Xs ++ Ys.

%% Unary operators

-spec unary_op3(0..10) -> 1..11.
unary_op3(X) -> - (bnot X).
