-module(int).

-compile([export_all, nowarn_export_all]).

-spec f(integer()) -> integer().
f(N) ->
    N.

%% test singleton char, char ranges, and the merging of them
-spec char_range() -> $a..$d | $b..$y | $z.
char_range() ->
    $c.
