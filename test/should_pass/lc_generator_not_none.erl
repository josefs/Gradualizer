-module(lc_generator_not_none).

-export([g/1]).

-type t() :: {a, d}
           | {a, [t()]}.

-spec g(t()) -> [t()].
g({a, d} = T) ->
    [T];
g({a, Ts}) ->
    [ T || T <- Ts ].
