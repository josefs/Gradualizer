-module(zip_generators_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 28).

-compile([export_all, nowarn_export_all]).

%% Basic zip generator - two list generators in lockstep
-spec zip_basic() -> [{integer(), atom()}].
zip_basic() ->
    [{X, Y} || X <- [1, 2, 3] && Y <- [a, b, c]].

%% Zip generator with three lists
-spec zip_three() -> [{integer(), atom(), float()}].
zip_three() ->
    [{X, Y, Z} || X <- [1, 2] && Y <- [a, b] && Z <- [1.0, 2.0]].

%% Zip generator with arithmetic
-spec zip_add() -> [integer()].
zip_add() ->
    [X + Y || X <- [1, 2, 3] && Y <- [10, 20, 30]].

%% Zip generator with filter after zip
-spec zip_with_filter() -> [{integer(), atom()}].
zip_with_filter() ->
    [{X, Y} || X <- [1, 2, 3] && Y <- [a, b, c], X > 1].

%% Zip generator in binary comprehension
-spec zip_in_bc() -> binary().
zip_in_bc() ->
    << <<(X + Y)>> || X <- [1, 2, 3] && Y <- [10, 20, 30] >>.

%% Zip generator in map comprehension
-spec zip_in_mc() -> #{integer() => atom()}.
zip_in_mc() ->
    #{X => Y || X <- [1, 2, 3] && Y <- [a, b, c]}.

%% Zip with strict generators
-spec zip_strict() -> [{integer(), atom()}].
zip_strict() ->
    [{X, Y} || X <:- [1, 2, 3] && Y <:- [a, b, c]].

%% Zip with mixed strict and non-strict
-spec zip_mixed() -> [{integer(), atom()}].
zip_mixed() ->
    [{X, Y} || X <- [1, 2, 3] && Y <:- [a, b, c]].

%% Zip scoping: variables from one arm don't leak into another arm's expression.
%% X is boolean from the parameter. The first zip arm rebinds X to integer.
%% The second arm's expression [X] must use the OUTER X (boolean), not the
%% rebound X (integer). So Y :: boolean.
%% With incorrect sequential scoping, Y would be integer() and this would fail.
-spec zip_independent_scope(boolean()) -> [{integer(), boolean()}].
zip_independent_scope(X) ->
    [{X, Y} || X <- [1, 2, 3] && Y <- [X, X, X]].

%% Zip scoping: each arm sees the pre-zip environment.
%% N is an integer parameter. First arm binds X from a list of atoms.
%% Second arm's expression uses N (outer scope), not X.
-spec zip_outer_scope_in_arms(integer()) -> [{atom(), integer()}].
zip_outer_scope_in_arms(N) ->
    [{X, Y} || X <- [a, b, c] && Y <- [N, N + 1, N + 2]].

%% Zip scoping: variable shadowing within a zip arm.
%% The zip body sees the rebound X (integer), not the parameter (atom).
-spec zip_body_sees_rebound(atom()) -> [integer()].
zip_body_sees_rebound(X) ->
    [X || X <- [1, 2, 3] && _Y <- [a, b, c]].

-endif. %% OTP >= 28
-endif. %% OTP_RELEASE
