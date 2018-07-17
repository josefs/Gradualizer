-module(tuple).

-compile([export_all]).

-spec tup({any(),any(),any()}) -> any().
tup({A,B,C}) ->
    A.

tup2(A,B) ->
    {A,B}.

-type tup() :: {atom(), integer()}.
-spec tup3(atom(), integer()) -> tup().
tup3(A,B) ->
    {A,B}.
