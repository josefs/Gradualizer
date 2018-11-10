-module(bc).

bc1() ->
    << <<X:4, Y:4>> || X <- [$a, $b], <<Y:4>> <= <<"xy">> >>.

-spec bc2(binary()) -> binary().
bc2(Bin) ->
    << <<X:4, Y:4>> || X <- [$a, $b], <<Y:4>> <= Bin >>.

-spec bc3(any()) -> binary().
bc3(Bin) ->
    << <<X:4, Y:4>> || X <- [$a, $b], <<Y:4>> <= Bin >>.

