-module(poly_pass).

%% See "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.
-export([poly_2/1,
         poly_pass/3]).

%% These examples don't come from the above paper.
-export([f/1,
         l/0]).

-gradualizer([solve_constraints]).

-spec poly_2(fun((A) -> A)) -> {integer(), integer()}.
poly_2(F) -> {F(42), F(84)}.

-spec poly_pass(fun((A) -> A), boolean(), boolean()) -> {boolean(), boolean()}.
poly_pass(F, B1, B2) -> {F(B2), F(B1)}.

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
