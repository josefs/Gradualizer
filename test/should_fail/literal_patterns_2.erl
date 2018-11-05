-module(literal_patterns_2).

-export([f/2]).

-spec f(_, string()) -> ok.
f(_, $a) -> ok.
