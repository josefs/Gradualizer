-module(case_same_var).
-export([test/1]).

-spec test(integer()) -> integer() | zero.
test(I) ->
    case I of
        0 -> zero;
        I -> I
    end.
