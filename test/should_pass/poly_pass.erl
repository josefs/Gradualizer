-module(poly_pass).

%% See "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.
-export([poly_2/1,
         poly_pass/3]).

%% These examples don't come from the above paper.
-export([f/1]).

-gradualizer([solve_constraints]).

-spec poly_2(fun((A) -> A)) -> {integer(), integer()}.
poly_2(F) -> {F(42), F(84)}.

-spec poly_pass(fun((A) -> A), boolean(), boolean()) -> {boolean(), boolean()}.
poly_pass(F, B1, B2) -> {F(B2), F(B1)}.

-spec f([integer(), ...]) -> integer().
f(L) ->
    hd(L).
