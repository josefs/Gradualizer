-module(annotated_types_fail).

-export([f/1, g/0, g/1, syntax/0]).

-include_lib("gradualizer/include/gradualizer.hrl").

-spec f(integer()) -> any().
f(N) ->
    ?annotate_type(N, pos_integer()).

-spec g() -> non_neg_integer().
g() ->
    receive {int, N} -> ?annotate_type(N, integer()) end.

g(X) ->
    N = ?annotate_type(X, integer()),
    atom_or_die(N).

syntax() ->
    '::'(banana, "not a type").

-spec atom_or_die(atom()) -> ok.
atom_or_die(A) when is_atom(A) -> ok.
