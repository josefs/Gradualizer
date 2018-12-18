-module(stuff_as_term).

-compile([export_all, nowarn_export_all]).

-record(rec, {field :: term()}).

-spec tuple_as_term() -> term().
tuple_as_term() ->
    {x, y, z}.

-spec tuple_pat_as_term(term()) -> any().
tuple_pat_as_term({X, Y}) ->
    {Y, X}.

-spec cons_as_term() -> term().
cons_as_term() -> [x, y].

-spec cons_pat_as_term(term()) -> any().
cons_pat_as_term([X|Xs]) -> X.

-spec fun_as_term() -> term().
fun_as_term() -> fun (X) -> X + 1 end.

-spec record_as_term() -> term().
record_as_term() -> #rec{}.

-spec record_pat_as_term(term()) -> any().
record_pat_as_term(#rec{}) -> ok.
