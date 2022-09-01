-module(poly_fail).

%% See "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.

-export([find1/0,
         poly_2/1,
         poly_fail/3]).

-spec lookup(T1, [{T1, T2}]) -> (none | T2).
lookup(_, []) -> none;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|KVs])-> lookup(K, KVs).

-spec find1() -> string().
find1() ->
    %% This should fail type checking.
    "s" = lookup(0, [{0, 1}]).

-spec poly_2(fun((A) -> A)) -> {integer(), boolean()}.
poly_2(F) -> {F(42), F(false)}.

-spec poly_fail(fun((A) -> A), boolean(), integer()) -> {boolean(), integer()}.
poly_fail(F, B, I) -> {F(I), F(B)}.
