-module(bitstring).

-compile([export_all]).

%% TODO: Normalize types in pattern matching so we can handle
%% binary() as a synonym for <<_:0, _:_*8>> in patterns.
-spec bin1(<<_:0, _:_*8>>) -> any().
bin1(<<A:4, B:4, _/binary>>) ->
    A+B.

-spec bin2(float()) -> binary().
bin2(A) ->
    <<A:32/float-little>>.

bin3(A) ->
    <<A:3>>.

bin4(A,B) ->
    <<A,B>>.
