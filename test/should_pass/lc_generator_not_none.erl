-module(lc_generator_not_none).

%% Compare with test/should_fail/lc_generator_not_none_fail.erl

-export([g/1,
         h/1]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(t()) -> [t()].
g({a, d} = T) ->
    [T];
g({a, Ts}) ->
    [ T || T <- Ts ].

-spec h(t()) -> [t()].
h({a, Ts}) ->
    [ T || T <- Ts ].
