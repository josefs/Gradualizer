-module(nonempty_list_match_in_head_nonexhaustive).

-export([f/1]).

%% For the infinite loop bug to happen all the conditions must be met:
%% - the function matches on a non-empty list
%% - the type is a union of something and a list of itself - if it's not a union,
%%   typechecker doesn't loop
%% - the type is recursive - a list of something else doesn't trigger the infinite loop
%% - the pattern is bound to a variable (doesn't matter if used or unused)

-type t() :: {} | [t()].

-spec f(t()) -> ok.
f(_A = [_|_]) -> ok.
