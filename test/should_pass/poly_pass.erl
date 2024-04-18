-module(poly_pass).

-export([f/1,
         l/0]).

-gradualizer([solve_constraints]).

-spec f([integer(), ...]) -> integer().
f(L) ->
    hd(L).

-type t1() :: {}.
-type t2() :: binary().
-type list_of_unions() :: [t1() | t2()].

-spec l() -> [t1() | t2()].
l() ->
    lists:map(fun helper/1, return_list_of_unions([])).

-spec helper(t1() | t2()) -> t1() | t2().
helper(T) -> T.

-spec return_list_of_unions(list_of_unions()) -> list_of_unions().
return_list_of_unions(_L) -> [].
