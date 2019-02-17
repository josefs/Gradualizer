-module(operator_subtypes).
-compile([pos_op1/2, nonneg_op1/2, neg_op1/2, word_op1/1, word_op2/2, word_op3/3]).

-type word16() :: 0..65535.

-spec pos_op1(pos_integer(), 1..10) -> pos_integer().
pos_op1(X, Y) -> X * Y + 777 bor Y.

-spec nonneg_op1(0..100, non_neg_integer()) -> non_neg_integer() | error.
nonneg_op1(X, Y) -> X + Y * X div Y rem X band Y bor X bxor Y bsl X bsr Y.

-spec neg_op1(neg_integer(), -10..-5 | -2..-1) -> neg_integer() | 0..10.
neg_op1(X, Y) -> X + Y + (-10).

-spec word_op1(non_neg_integer()) -> word16().
word_op1(N) -> N rem 65536.

-spec word_op2(word16(), word16()) -> word16().
word_op2(X, Y) -> X band Y bor 32768 bxor Y.

-spec word_op3(word16(), non_neg_integer(), pos_integer()) -> word16().
word_op3(X, Y, Z) -> (X bsr Y) div Z.