-module(lc_generator_should_fail).

%% Compare with:
%% - test/known_problems/should_pass/lc_cannot_glb_different_variants.erl
%% - test/should_pass/lc_generator_not_none.erl

-export([g/1]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(t()) -> [t()].
g({a, Ts}) ->
    %% If we didn't use a list comprehension and just returned an empty list,
    %% this would rightly fail with a nonexhaustiveness warning.
    %[].
    %% If we use a comprehension, glb(d, [t()]) determines that Ts :: none().
    %% This glb() call is done from typechecker:add_type_pat({a, Ts}, {a, d} | {a, [t()]}, Env)
    %% at typechecker.erl:4310
    [ T || T <- Ts ].
