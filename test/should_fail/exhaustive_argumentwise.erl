-module(exhaustive_argumentwise).

-export([f/2]).

-type t() :: ala | ola.

-spec f(t(), any()) -> ok.
f(ala, _) -> ok.
