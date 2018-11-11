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

bin5() ->
    <<"abc", 42, "abc"/utf32, "abc"/float, 42/float-little,
      (<<"abc">>):8/bits, (<<"abc">>)/bytes>>.
