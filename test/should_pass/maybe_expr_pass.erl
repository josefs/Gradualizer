-module(maybe_expr_pass).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 25).
-if(?FEATURE_AVAILABLE(maybe_expr)).

-feature(maybe_expr, enable).

-export([check1/0, check2/0]).
-export([infer1/0, infer2/1]).
-export([multiple_matches/1, tuple_destructure/1, var_binding/0]).
-export([no_match_ops/0, nested_maybe/1, else_with_multiple_clauses/1]).

-spec check1() -> integer().
check1() ->
    maybe
        ok ?= ok,
        1
    end.

-spec check2() -> integer().
check2() ->
    maybe
        ok ?= not_ok,
        1
    else
        _ -> 2
    end.

-spec infer1() -> integer().
infer1() ->
    R = maybe
        ok ?= ok,
        1
    end,
    R.

-spec infer2(string()) -> {ok, string()} | error.
infer2(Val) ->
    R = maybe
        "ok" ?= Val,
        {ok, Val}
    else
        _ -> error
    end,
    R.

%% Multiple ?= operators in sequence
-spec multiple_matches(#{name => string(), age => integer()}) ->
    {string(), integer()} | undefined.
multiple_matches(Map) ->
    maybe
        {ok, Name} ?= maps:find(name, Map),
        {ok, Age} ?= maps:find(age, Map),
        {Name, Age}
    else
        error -> undefined
    end.

%% Tuple pattern destructuring with ?=
-spec tuple_destructure({ok, integer()} | {error, string()}) ->
    integer() | {error, string()}.
tuple_destructure(Input) ->
    maybe
        {ok, Val} ?= Input,
        Val + 1
    end.

%% Variable bindings flow between expressions in body
-spec var_binding() -> integer().
var_binding() ->
    maybe
        X = 1,
        ok ?= ok,
        Y = 2,
        X + Y
    end.

%% Maybe with no ?= operators (just a block)
-spec no_match_ops() -> integer().
no_match_ops() ->
    maybe
        X = 1,
        Y = 2,
        X + Y
    end.

%% Nested maybe blocks
-spec nested_maybe(ok | error) ->
    {ok, integer()} | error.
nested_maybe(Input) ->
    maybe
        ok ?= Input,
        maybe
            ok ?= ok,
            {ok, 42}
        end
    else
        _ -> error
    end.

%% Else with multiple clauses
-spec else_with_multiple_clauses({ok, integer()} | {error, atom()} | undefined) ->
    integer() | {error, atom()} | 0.
else_with_multiple_clauses(Input) ->
    maybe
        {ok, Val} ?= Input,
        Val
    else
        {error, Reason} -> {error, Reason};
        _ -> 0
    end.

-endif. %% FEATURE_AVAILABLE
-endif. %% OTP >= 25
-endif. %% OTP_RELEASE
