-module(type_variable).

-compile([export_all, nowarn_export_all]).

-spec fff() -> Result when Result :: integer().
fff() -> 0.

ggg() -> -fff().

%% Use string:words/1 since its type spec in OTP is
%% -spec words(String) -> Count when
%%       String :: string(),
%%       Count :: pos_integer().

ggg_ext() -> -string:words("a b c d").

ggg_fun1() -> -(fun fff/0)().
ggg_fun2() -> F = fun fff/0, -F().

ggg_mfa() ->
    F = fun string:words/1,
    -F("a b c d").

%% Result should be pos_integer() in the end.
-spec ff() -> Result when
    Result       :: integer() | Intermediate,
    Result       :: apple | Other,
    Intermediate :: boolean() | Other,
    Other        :: pos_integer().
ff() -> 1.

-spec gg() -> neg_integer().
gg() -> -ff().
