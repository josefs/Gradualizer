-module(list_exhaustiveness_checking_regressions2).

-export([i/1, j/1]).

-spec i([atom()]) -> ok.
i([]) -> ok;
i([Cs]) -> ok;
i([C1, C2 | Cs]) -> ok.

-spec j([atom()]) -> integer().
j([]) -> 1;
j([C1, C2 | _]) -> 2;
j([Cs]) -> 3.
