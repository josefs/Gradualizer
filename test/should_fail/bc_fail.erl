-module(bc_fail).
-export([f/0, non_bin_expr/0, integer_signed_wrong/1]).

-spec f() -> binary().
f() ->
    << X || <<X/integer>> <= <<"abc">> >>.

non_bin_expr() ->
    << (list_to_integer(X)) || X <- ["42"] >>.

-spec integer_signed_wrong(binary()) -> non_neg_integer().
integer_signed_wrong(B) ->
    <<A/signed>> = B,
    A.

