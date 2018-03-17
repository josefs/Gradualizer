-module(tuple).

-compile([export_all]).

-spec tup({any(),any(),any()}) -> any().
tup({A,B,C}) ->
    A.

tup2(A,B) ->
    {A,B}.
