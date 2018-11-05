-module(literal_patterns_4).

-export([f/1]).

-spec f(integer()) -> ok.
f(1.0) -> ok.
