-module(varbind_in_lc).

-compile([export_all, nowarn_export_all]).

-spec is_true(true) -> true.
is_true(true) -> true.

%% Don't forget variable bindings in lc guard.

lc1() ->
    [ X || X = true, X == true ].

lc2() ->
    [ X || X = true, is_true(X) ].

bc1() ->
    << <<0>> || X = true, X == true >>.

bc2() ->
    << <<0>> || X = true, is_true(X) >>.

