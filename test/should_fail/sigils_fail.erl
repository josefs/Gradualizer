-module(sigils_fail).

%% Each exported function should produce exactly one type error.

-export([sigil_wrong_type/0]).

%% ~"..." produces a binary, not an integer
-spec sigil_wrong_type() -> integer().
sigil_wrong_type() ->
    ~"hello".
