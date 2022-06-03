-module(binary_exhaustiveness_checking).

-export([f/1, g/1, h/1]).

f(<<>>) -> ok;
f(<<_:8, _/binary>>) -> ok.

-spec g(binary()) -> ok.
g(<<>>) -> ok;
g(<<_:8, _/binary>>) -> ok.

-spec h(binary()) -> ok.
h(<<_:4, _:4, _/binary>>) -> ok.
