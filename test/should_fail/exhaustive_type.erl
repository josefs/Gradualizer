-module(exhaustive_type).

-export([allergen_score/1]).

-type allergen() :: eggs
                  | chocolate
                  | pollen
                  | cats.

-spec allergen_score(allergen()) -> integer().
allergen_score(Al) ->
    case Al of
        eggs         ->  1;
        chocolate    -> 32;
        pollen       -> 64
    end.
