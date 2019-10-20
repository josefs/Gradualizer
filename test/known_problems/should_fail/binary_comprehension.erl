-module(binary_comprehension).
-compile([debug_info]).
-export([
         bitstring_match/0
        ]).

-spec bitstring_match() -> <<_:7, _:_*3>> | string().
bitstring_match() ->
    << <<1:N>> || N <- lists:seq(7, 12)>>.
