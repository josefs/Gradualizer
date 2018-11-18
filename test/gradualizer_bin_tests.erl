-module(gradualizer_bin_tests).

-include_lib("eunit/include/eunit.hrl").

%% Parse type and expression
t(Str) -> typelib:remove_pos(typelib:parse_type(Str)).
e(Str) -> merl:quote(Str).

-define(_assert_bin_type(T, E),
        {??E, ?_assertEqual(t(??T), gradualizer_bin:compute_type(e(??E)))}).

compute_type_combined_test_() ->
    [
     ?_assert_bin_type(<<_:14, _:_*3>>, <<0:14, N/binary-unit:3>>),
     ?_assert_bin_type(<<_:14, _:_*3>>, <<0:N/integer-unit:3, "ab":7/integer>>)
    ].

compute_type_utf_test_() ->
    [
     ?_assert_bin_type(<<_:24>>,    <<"abc"/utf8>>),
     ?_assert_bin_type(<<_:48>>,    <<"abc"/utf16>>),
     ?_assert_bin_type(<<_:96>>,    <<"abc"/utf32>>),
     ?_assert_bin_type(<<_:_*8>>,   <<X/utf8>>),
     ?_assert_bin_type(<<_:_*16>>,  <<X/utf16>>),
     ?_assert_bin_type(<<_:32>>,    <<X/utf32>>)
    ].

compute_type_test_() ->
    [
     ?_assert_bin_type(<<_:8>>,     <<X>>),
     ?_assert_bin_type(<<_:3>>,     <<X:3>>),
     ?_assert_bin_type(<<_:_*1>>,   <<X:N>>),
     ?_assert_bin_type(<<_:12>>,    <<X:3/unit:4>>),
     ?_assert_bin_type(<<_:_*4>>,   <<X:N/unit:4>>)
    ].

compute_type_bitstring_test_() ->
    [
     ?_assert_bin_type(<<_:8>>,     <<X:1/binary>>),
     ?_assert_bin_type(<<_:42>>,    <<X:7/binary-unit:6>>),
     ?_assert_bin_type(<<_:_*8>>,   <<X/binary>>),
     ?_assert_bin_type(<<_:_*2>>,   <<X/binary-unit:2>>),
     ?_assert_bin_type(<<_:_*8>>,   <<X/bytes>>),
     ?_assert_bin_type(<<_:16>>,    <<X:2/bytes>>),
     ?_assert_bin_type(<<_:_*1>>,   <<X/bitstring>>),
     ?_assert_bin_type(<<_:_*1>>,   <<X/bits>>)
    ].

compute_type_float_test_() ->
    [
     ?_assert_bin_type(<<_:64>>,            <<X/float>>),
     ?_assert_bin_type(<<_:32>>,            <<X:32/float>>),
     ?_assert_bin_type(<<_:32, _:_*32>>,    <<X:S/float>>),
     ?_assert_bin_type(<<_:32, _:_*32>>,    <<X:S/float-unit:16>>),
     ?_assert_bin_type(<<_:64>>,            <<X:S/float-unit:64>>)
    ].

-ifdef(OTP_RELEASE).
%% Run only in OTP 21
compute_type_float_string_test_() ->
    [
     ?_assert_bin_type(<<_:192>>,           <<"abc"/float>>),
     ?_assert_bin_type(<<_:96>>,            <<"abc":32/float>>),
     ?_assert_bin_type(<<_:96, _:_*32>>,    <<"abc":S/float>>)
    ].
-else.
compute_type_float_string_test_() ->
    {"Skipping <<\"str\"/float>> tests in this OTP release", []}.
-endif.
