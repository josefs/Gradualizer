-module(literal_patterns_1).

-export([f/1]).

-spec f(not_a_number) -> not_a_number.
f(10 = X) -> X.
