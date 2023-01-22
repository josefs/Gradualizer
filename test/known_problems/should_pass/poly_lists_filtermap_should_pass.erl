-module(poly_lists_filtermap_should_pass).

-gradualizer([solve_constraints]).

-export([f/1]).

%% This case is based on gradualizer_db:get_beam_map/0.
%% We could override the spec of lists:filtermap/2 as follows to fix this:
%%
%%   -spec filtermap(Fun, List1) -> List2 when
%%         Fun :: fun((Elem) -> boolean() | {true, Value}),
%%         List1 :: [Elem],
%%         List2 :: [Value].
%%
%% But then it would not reflect that List2 :: [Elem] if we only return boolean() from Fun.
%%
%% In general, this shows that constraints on Elem are not registered properly.
%% It's too constrained, which leads to false positives.

-spec f([string()]) -> map().
f(Strings) ->
    KVs = lists:filtermap(fun (Elem) -> {true, {will_be_key, Elem}} end, Strings),
    maps:from_list(KVs).
