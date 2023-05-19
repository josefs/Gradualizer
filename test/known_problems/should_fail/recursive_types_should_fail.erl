-module(recursive_types_should_fail).

-export([recursive_param1/1,
         recursive_param3/1]).

-type rec1(A) :: A | rec1({A | rec1(A)}).

-spec recursive_param1(rec1(integer())) -> ok.
recursive_param1({qwe, zxc}) -> ok.

-type rec3(A) :: A | rec3(A).

%% `ok' is not an `integer()' - this should fail.
-spec recursive_param3(rec3(integer())) -> atom().
recursive_param3(ok) -> ok.