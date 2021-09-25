-module(exhaustive_float).

-export([ef/1]).

-type t() :: {int, integer()}
           | {float, float()}.

-spec ef(t()) -> ok.
ef(T) ->
    case T of
        {int, _} -> ok
    end.
