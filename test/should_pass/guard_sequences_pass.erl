-module(guard_sequences_pass).

-export([foo/2]).

-spec foo(List1, List2) -> List2 when
      List1 :: [Int],
      List2 :: [Int|{Int,Int}],
      Int :: integer().
foo([], Acc) ->
    Acc;
foo([H | T], []) ->
    foo(T, [H]);
foo([H | T], [HA | TA]) when is_integer(HA), H == HA + 1 ->
    foo(T, [{HA, H} | TA]);
foo([H | T], [{HAB, HAE} | TA]) when H == HAE + 1 ->
    foo(T, [{HAB, H} | TA]);
foo([H | T], Acc) ->
    foo(T, [H | Acc]).
