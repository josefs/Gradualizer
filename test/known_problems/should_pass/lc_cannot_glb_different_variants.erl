-module(lc_cannot_glb_different_variants).

%% Compare with:
%% - test/should_fail/lc_generator_should_fail.erl
%% - test/should_pass/lc_generator_not_none.erl

-export([g/2]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(integer(), t()) -> [t()].
g(1, {a, d} = T) ->
    [T];
g(_, {a, Ts}) ->
    [ T || T <- Ts ];
g(_, _) ->
    [].
