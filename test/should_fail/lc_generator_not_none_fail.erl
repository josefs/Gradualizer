-module(lc_generator_not_none_fail).

%% Compare with test/should_pass/lc_generator_not_none.erl

-export([g/2]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(integer(), t()) -> [t()].
g(1, {a, d} = T) ->
    [T];
g(_, {a, Ts}) ->
    [ T || T <- Ts ];
g(_, _) ->
    %% This clause is redundant, as the previous two exhaust g/2 parameters.
    %% It's correctly reported as such.
    [].
