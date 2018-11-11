-module(bc_nonbin_expr).

f() ->
    << (list_to_integer(X)) || X <- ["42"] >>.
