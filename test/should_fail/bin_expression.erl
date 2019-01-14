-module(bin_expression).
-export([bin_1/0, bin_2/2]).

-spec bin_1() -> binary().
bin_1() ->
    <<1:1>>.

-spec bin_2(any(), any()) -> <<_:_*6>>.
bin_2(A, B) ->
    <<0:A/integer-unit:27, 1:B/integer-unit:30>>.