-module(module_info).

-compile([export_all, nowarn_export_all]).

-spec nullary_direct() -> [{atom(), atom() | list() | binary()}].
nullary_direct() ->
    erlang:module_info().

-spec nullary_var() -> [{atom(), any()}].
nullary_var() ->
    I = erlang:module_info(),
    I.

-spec unary_direct() -> binary().
unary_direct() ->
    erlang:module_info(md5).

-spec unary_var() -> atom().
unary_var() ->
    I = erlang:module_info(module),
    I.