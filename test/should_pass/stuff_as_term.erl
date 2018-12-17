-module(stuff_as_term).

-compile([export_all, nowarn_export_all]).

-spec tuple_as_term() -> term().
tuple_as_term() ->
    {x, y, z}.

-spec tuple_pat_as_term(term()) -> any().
tuple_pat_as_term({X, Y}) ->
    {Y, X}.
