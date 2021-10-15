-module(infinite_loop5).

-compile([export_all, nowarn_export_all]).

-type rec1() :: rec1 | {rec1, rec1()} | {rec1, {rec1, rec1()}}.

%% `{_, Z}' doesn't match on atom `rec1', which is a valid value of type `rec1()'.
%% This should fail.
-spec unwrap(rec1()) -> rec1().
unwrap({_, Z}) -> Z.
