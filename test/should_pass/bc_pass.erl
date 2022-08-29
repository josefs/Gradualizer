-module(bc_pass).

-compile([export_all, nowarn_export_all]).

bc1() ->
    << <<X:4, Y:4>> || X <- [$a, $b], <<Y:4>> <= <<"xy">> >>.

-spec bc2(binary()) -> binary().
bc2(Bin) ->
    << <<X:4, Y:4>> || X <- binary_to_list(<<"ab">>), <<Y:4>> <= Bin >>.

-spec bc3(any()) -> binary().
bc3(Bin) ->
    << <<X:4, Y:4>> || X <- [$a, $b], <<Y:4>> <= Bin >>.

bc4() ->
    << (list_to_binary(X)) || X <- ["apa", "bepa"], is_list(X) >>.

bc5() ->
    << (ipv4_to_binary(IP)) || IP <- [{192, 168, 0, 1}, {127, 0, 0, 1}] >>.

bc6() ->
    << (unusually_sized_bitstring(X)) || <<X>> <= <<"abc">> >>.

bc7(X) ->
    << (union_of_bitstrings(P)) || P <- [is_integer(X), is_float(X)] >>.

-spec bc8(number()) -> <<_:42, _:_*16>>.
bc8(X) ->
    << (union_of_bitstrings(P)) || P <- [is_integer(X), is_float(X)] >>.

bc9(N) ->
    << <<B/bitstring>> || B <- union_of_lists(N) >>.

bc10(Str) ->
    << <<B:2/bytes>> || <<B:4/bytes>> <= list_to_binary(Str) >>.

-spec bc11(integer()) -> bitstring().
bc11(N) ->
    << <<B/bitstring>> || B <- union_of_lists(N), N > 0 >>.

%% Returns fixed size binary
-spec ipv4_to_binary({byte(), byte(), byte(), byte()}) -> <<_:32>>.
ipv4_to_binary({A, B, C, D}) ->
    <<A, B, C, D>>.

%% Returns a bitstring of a fixed size plus a multiple of some size
-spec unusually_sized_bitstring(integer()) -> <<_:17, _:_*3>>.
unusually_sized_bitstring($a) -> <<1:17, 3:3>>;
unusually_sized_bitstring($b) -> <<2:17, 3:6>>;
unusually_sized_bitstring(N)  -> <<3:17, N:9>>.

%% Returns a union of bitstring types
-spec union_of_bitstrings(boolean()) -> <<_:42>> | <<_:_*16>>.
union_of_bitstrings(true) -> <<0:42>>;
union_of_bitstrings(false) -> <<"xyz"/utf16-little>>.

%% Returns a union of lists of some bitstring type
-spec union_of_lists(integer()) -> [<<_:_*3>>] | [<<_:_*16>>].
union_of_lists(N) when N > 42 -> [<<>>, <<N/utf16>>];
union_of_lists(N)             -> [<<42:3>>, <<N:9>>].

-spec integer_default(binary()) -> non_neg_integer().
integer_default(B) ->
    <<A>> = B,
    A.

-spec integer_unsigned(binary()) -> non_neg_integer().
integer_unsigned(B) ->
    <<A/unsigned>> = B,
    A.

-spec integer_signed(binary()) -> integer().
integer_signed(B) ->
    <<A/signed>> = B,
    A.

-spec expr_vs_pat_default() -> non_neg_integer().
expr_vs_pat_default() ->
    <<A>> = <<-1>>,
    A.

-spec expr_vs_pat_unsigned() -> non_neg_integer().
expr_vs_pat_unsigned() ->
    <<A/unsigned>> = <<-1>>,
    A.

-spec expr_vs_pat_signed() -> integer().
expr_vs_pat_signed() ->
    <<A/signed>> = <<-1>>,
    A.

