-module(unary_op).
-export([fail/1, non_number_argument_to_minus/1]).

-spec fail(number()) -> boolean().
fail(X) -> -X.

-spec non_number_argument_to_minus(atom()) -> integer().
non_number_argument_to_minus(A) ->
    A = - A,
    A.
