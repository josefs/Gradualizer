-module(strict_generators_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 28).

-compile([export_all, nowarn_export_all]).

%% Strict list generator in list comprehension
-spec strict_lc_integers() -> [integer()].
strict_lc_integers() ->
    [X * 2 || X <:- [1, 2, 3]].

%% Strict list generator with filter
-spec strict_lc_filter() -> [integer()].
strict_lc_filter() ->
    [X || X <:- [1, 2, 3, 4], X > 2].

%% Strict binary generator in binary comprehension
-spec strict_bc() -> binary().
strict_bc() ->
    << <<X>> || <<X>> <:= <<1,2,3>> >>.

%% Strict list generator in binary comprehension
-spec strict_list_gen_in_bc() -> binary().
strict_list_gen_in_bc() ->
    << <<X>> || X <:- [1, 2, 3] >>.

%% Strict binary generator in list comprehension
-spec strict_bin_gen_in_lc() -> [integer()].
strict_bin_gen_in_lc() ->
    [X || <<X>> <:= <<1,2,3>>].

%% Mixed strict and non-strict generators
-spec mixed_generators() -> [{integer(), integer()}].
mixed_generators() ->
    [{X, Y} || X <- [1, 2], Y <:- [3, 4]].

%% Strict generator with variable shadowing
-spec strict_var_shadow(boolean(), [integer()]) -> [integer()].
strict_var_shadow(X, Xs) ->
    [X || X <:- Xs].

%% Strict generator with any() type
-spec strict_any_list() -> list().
strict_any_list() ->
    [X || X <:- [1, 2, 3]].

%% Strict generator with union of list types
-spec strict_union_list() -> [integer()] | [atom()].
strict_union_list() ->
    [X || X <:- [apa, bepa]].

-endif. %% OTP >= 28
-endif. %% OTP_RELEASE
