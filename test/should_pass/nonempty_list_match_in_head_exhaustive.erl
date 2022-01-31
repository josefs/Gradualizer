-module(nonempty_list_match_in_head_exhaustive).

-export([f/1]).

-type t() :: {} | [t()].

-spec f(t()) -> ok.
f({}) -> ok;
f([]) -> ok;
f([_|_]) -> ok.
