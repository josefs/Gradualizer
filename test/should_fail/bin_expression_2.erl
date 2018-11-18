-module(bin_expression_2).

-spec bin_2(any(), any()) -> <<_:_*6>>.
bin_2(A, B) ->
    <<0:A/integer-unit:27, 1:B/integer-unit:30>>.
