-module(binary_in_union).
-export([
         iodata_binary/0,
         nested_bitstrings/0
        ]).

-spec iodata_binary() -> iodata().
iodata_binary() ->
    <<<<"A">> || _ <- lists:seq(1, 10)>>.

-type nested() :: string() | bitstring() | binary().
-spec nested_bitstrings() -> nested() | boolean() | bitstring().
nested_bitstrings() ->
    << <<1:N>> || N <- lists:seq(1, 12)>>.
