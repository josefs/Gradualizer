-module(literal_patterns).

-export([f/1, f/2, g/1, h/1, i/1]).

-spec f(not_a_number) -> not_a_number.
f(10 = X) -> X.

-spec f(_, string()) -> ok.
f(_, $a) -> ok.

-spec g({ok, 10..20}) -> ok.
g({ok, 21}) -> ok.

-spec h(integer()) -> ok.
h(1.0) -> ok.

-spec i(atom) -> ok.
i("foo") -> ok.
