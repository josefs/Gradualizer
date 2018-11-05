-module(literal_patterns_3).

-export([f/1]).

-spec f({ok, 10..20}) -> ok.
f({ok, 21}) -> ok.
