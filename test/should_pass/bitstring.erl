-module(bitstring).

-compile([export_all]).

-spec bin1(binary()) -> any().
bin1(<<A:4, B:4, _/binary>>) ->
    A+B.

-spec bin2(float()) -> binary().
bin2(A) ->
    <<A:32/float-little>>.

bin3(A) ->
    <<A:3>>.

bin4(A,B) ->
    <<A,B>>.
