-module(stuff_as_top).

-compile([export_all, nowarn_export_all]).

-record(rec, {field :: gradualizer:top()}).

-spec tuple_as_top() -> gradualizer:top().
tuple_as_top() ->
    {x, y, z}.

-spec tuple_pat_as_top(gradualizer:top()) -> any().
tuple_pat_as_top({X, Y}) ->
    {Y, X}.

-spec cons_as_top() -> gradualizer:top().
cons_as_top() -> [x, y].

-spec cons_pat_as_top(gradualizer:top()) -> any().
cons_pat_as_top([X|_]) -> X.

-spec fun_as_top() -> gradualizer:top().
fun_as_top() -> fun (X) -> X + 1 end.

-spec record_as_top() -> gradualizer:top().
record_as_top() -> #rec{}.

-spec record_pat_as_top(gradualizer:top()) -> any().
record_pat_as_top(#rec{}) -> ok.
