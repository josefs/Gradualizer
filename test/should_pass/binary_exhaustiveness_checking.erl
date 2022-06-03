-module(binary_exhaustiveness_checking).

-export([f/1, g/1, h/1, k/1]).

f(<<>>) -> ok;
f(<<_:8, _/binary>>) -> ok.

-spec g(binary()) -> ok.
g(<<_:8, _/binary>>) -> ok;
g(<<>>) -> ok.

-spec h(binary()) -> ok.
h(<<_:8, _/binary>>) -> ok;
h(<<>>) -> ok.

-spec k(binary()) -> ok.
k(<<_:4, _:4, _/binary>>) -> ok.
