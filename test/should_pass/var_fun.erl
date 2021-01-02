-module(var_fun).

-compile([export_all, nowarn_export_all]).

f() ->
    lists:map(fun (F) -> F(5) end, [ fun ?MODULE:F/1 || F <- [apa, bepa]]).

-spec g() -> fun((integer()) -> integer()).
g() ->
    F = apa,
    fun ?MODULE:F/1.

h() ->
    M = ?MODULE,
    F = apa,
    A = 1,
    fun M:F/A.

apa(X) ->
    X * 2.

bepa(X) ->
    X + 2.
