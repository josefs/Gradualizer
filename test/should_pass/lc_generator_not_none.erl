-module(lc_generator_not_none).

%% Compare with:
%% - test/known_problems/should_pass/lc_cannot_glb_different_variants.erl
%% - test/should_fail/lc_generator_should_fail.erl

-export([g/1]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(t()) -> [t()].
g({a, d} = T) ->
    [T];
g({a, Ts}) ->
    [ T || T <- Ts ].
