-module(recursive_types_passing).

-export([recursive_param1/1,
         return_recursive_type/1,
         recursive_param2/1]).


-type apa(A) :: A | apa({A}).

-spec recursive_param1(apa(integer())) -> ok.
recursive_param1(_) -> ok.


-record(rec, {rec :: any()}).
-type rec(A) :: A | #rec{rec :: A | rec(A)}.

-spec return_recursive_type(any()) -> rec(integer()).
return_recursive_type(_) -> 1.


-type rec1(A) :: A | rec1({A | rec1(A)}).

-spec recursive_param2(rec1(integer())) -> ok.
recursive_param2(_) -> ok.
