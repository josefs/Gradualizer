-module(bounded_funs).

-export([calls/0, funs/0]).

-spec calls() -> gradualizer:top().
calls() ->
    V1 = g(myatom),
    V2 = ets:lookup_element(myatom, asd, 2),
    V3 = prepend(myatom, [asd]),
    {V1, V2, V3}.

funs() ->
    F1 = fun g/1,
    F2 = fun ets:lookup_element/3,
    F3 = fun prepend/2,
    {F1, F2, F3}.

-spec g(Atom) -> any() when Atom :: atom().
g(A) ->
    A.

%% Constraint with free var E
-spec prepend(Elem, [E]) -> [E] when Elem :: E.
prepend(Elem, L) ->
    [Elem|L].
