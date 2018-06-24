-module(bounded_funs).

-export([f/0, h/0, funs/0]).

-spec f() -> term().
f() ->
    g(myatom).

-spec g(Atom) -> any() when Atom :: atom().
g(A) ->
    A.

h() ->
    ets:lookup_element(myatom, asd, 2).

funs() ->
    F1 = fun g/1,
    F2 = fun ets:lookup_element/3,
    {F1, F2}.
