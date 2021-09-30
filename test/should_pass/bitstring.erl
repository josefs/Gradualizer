-module(bitstring).

-compile([export_all, nowarn_export_all]).

%% TODO: I'm not sure binary() specs can be that specific...
%-spec bin1(binary()) -> any().
%% It seems they almost can - from https://erlang.org/doc/reference_manual/typespec.html:
%%   Bitstring :: <<>>
%%              | <<_:M>>          %% M is an Integer_Value that evaluates to a positive integer
%%              | <<_:_*N>>        %% N is an Integer_Value that evaluates to a positive integer
%%              | <<_:M, _:_*N>>
%% So a variable length after the first 8 bits is not supported :/
-spec bin1(<<_:8, _:_*10>>) -> any().
bin1(<<A:4, B:4, _/binary>>) ->
    A+B.

-spec bin2(float()) -> binary().
bin2(A) ->
    <<A:32/float-little>>.

bin3(A) ->
    <<A:3>>.

bin4(A,B) ->
    <<A,B>>.

bin5() ->
    <<"abc", 42, "abc"/utf32, "abc"/float, 42/float-little,
      (<<"abc">>):8/bits, (<<"abc">>)/bytes>>.

-spec bin6(any(), any()) -> <<_:_*6>>.
bin6(A, B) ->
    <<0:A/integer-unit:36, 1:B/integer-unit:30>>.
