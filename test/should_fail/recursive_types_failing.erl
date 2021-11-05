-module(recursive_types_failing).

-export([recursive_param1/1,
         recursive_param2/1,
         recursive_param3/1]).

-type rec1(A) :: A | rec1({A | rec1(A)}).

-spec recursive_param1(rec1(integer())) -> ok.
recursive_param1({qwe, zxc}) -> ok.


-type rec2() :: rec2 | {rec2, rec2()} | {rec2, {rec2, rec2()}}.

%% `{_, Z}' doesn't match on atom `rec2', which is a valid value of type `rec2()'.
%% This should fail.
-spec recursive_param2(rec2()) -> rec2().
recursive_param2({_, Z}) -> Z.


-type rec3(A) :: A | rec3(A).

%% `ok' is not an `integer()' - this should fail.
-spec recursive_param3(rec3(integer())) -> atom().
recursive_param3(ok) -> ok.
