-module(factorial).

-export([factorial/1]).

%% This tests multiple things:
%%   * Type refinement of the argument. After the first clase,
%%     we have N :: pos_integer().
%%   * Multiplication is closed under pos_integer()
%%   * pos_integer() - 1 :: non_neg_integer()
-spec factorial(non_neg_integer()) -> pos_integer().
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
