-module(poly_fail).

%% See "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.

-export([poly_2/1,
         poly_fail/3]).

-spec poly_2(fun((A) -> A)) -> {integer(), boolean()}.
poly_2(F) -> {F(42), F(false)}.

-spec poly_fail(fun((A) -> A), boolean(), integer()) -> {boolean(), integer()}.
poly_fail(F, B, I) -> {F(I), F(B)}.
