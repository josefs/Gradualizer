-module(literal_patterns_5).

-export([f/1]).

-spec f(atom) -> ok.
f("foo") -> ok.
