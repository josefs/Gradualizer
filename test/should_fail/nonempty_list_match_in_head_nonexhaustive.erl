-module(nonempty_list_match_in_head_nonexhaustive).

-export([f/1]).

-type t() :: {} | [t()].

-spec f(t()) -> ok.
f(_A = [_|_]) -> ok.
