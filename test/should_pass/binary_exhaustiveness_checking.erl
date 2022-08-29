-module(binary_exhaustiveness_checking).

-gradualizer([infer]).

-export([f/1, g/1, h/1, k/1, l/1]).

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

%% This case should pass, since we should detect that <<V:8, _/binary>> is a complex pattern for
%% which we cannot guarantee it exhausts the binary() type (see `is_bin_pat_exhaustive')
%% and therefore not try to do exhaustiveness checking.
-spec l(binary()) -> ok.
l(B) ->
    V = $A,
    case B of
        <<>> ->
            ok;
        <<V:8, _/binary>> ->
            ok
    end.
