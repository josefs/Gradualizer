%%% @doc Test cases for `add_type_pat/4'
-module(type_pattern).

-compile([export_all, nowarn_export_all]).

-type mychar() :: char().

-spec f([mychar()]) -> any().
f("foo") ->
    ok.
