-module(list).

-compile([export_all]).

-spec f([integer()]) -> integer().

f([]) ->
    0;
f([A|As]) ->
    A + f(As).
