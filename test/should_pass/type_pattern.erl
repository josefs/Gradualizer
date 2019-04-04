%%% @doc Test cases for `add_type_pat/4'
-module(type_pattern).

-type mychar() :: char().

%% The user type `mychar()' inside a list is not normalized when
%% `add_types_pats/4' is called, but postponed later within
%% `subtype(string(), [mychar()], TEnv)'. There was a typo that
%% specifically in case of a string pattern VEnv was passed instead of
%% TEnv.
-spec f([mychar()]) -> any().
f("foo") ->
    ok.
