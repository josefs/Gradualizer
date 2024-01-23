-module(poly_fail).

%% See "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.
-export([find1/0,
         poly_2/1,
         poly_2b/1,
         poly_fail/3,
         poly_fail_2/3]).

%% These examples don't come from the above paper.
-export([f/1,
         g/1,
         h/1]).

-gradualizer([solve_constraints]).

-spec lookup(T1, [{T1, T2}]) -> (none | T2).
lookup(_, []) -> none;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|KVs]) -> lookup(K, KVs).

-spec find1() -> string().
find1() ->
    %% This should fail type checking.
    "s" = lookup(0, [{0, 1}]).

-spec poly_2(fun((A) -> A)) -> {integer(), boolean()}.
poly_2(F) -> {F(42), F(false)}.

-spec poly_2b(fun((A) -> A)) -> {integer(), integer()}.
poly_2b(F) -> {F(42), F(84)}.

-spec poly_fail(fun((A) -> A), boolean(), integer()) -> {boolean(), integer()}.
poly_fail(F, B, I) -> {F(I), F(B)}.

-spec poly_fail_2(fun((A) -> A), boolean(), boolean()) -> {boolean(), boolean()}.
poly_fail_2(F, B1, B2) -> {F(B2), F(B1)}.

-spec f([integer(), ...]) -> atom().
f(L) ->
    hd(L).

-spec g([integer()]) -> [atom()].
g(L) ->
    lists:map(fun helper/1, L).

-spec helper(integer()) -> integer().
helper(I) -> I * 2.

-spec h(integer()) -> atom().
h(I) ->
    app(fun helper/1, I).

-spec app(fun ((A) -> B), A) -> B.
app(F, A) ->
    F(A).
