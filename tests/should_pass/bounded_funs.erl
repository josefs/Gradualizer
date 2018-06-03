-module(bounded_funs).

-export([f/0, h/0]).

-spec f() -> term().
f() ->
    g(myatom).

-spec g(Atom) -> any() when Atom :: atom().
g(A) ->
    A.

h() ->
    ets:lookup_element(myatom, asd, 2).
