-module(bc).
-export([f/0, non_bin_expr/0]).

-spec f() -> binary().
f() ->
    << X || <<X/integer>> <= <<"abc">> >>.

non_bin_expr() ->
    << (list_to_integer(X)) || X <- ["42"] >>.
