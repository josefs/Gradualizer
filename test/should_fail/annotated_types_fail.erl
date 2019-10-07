-module(annotated_types_fail).

-export([downcast_using_annotation/1,
         type_error_after_annotation/0, type_error_after_annotation/1,
         incompatible_cast/0, incompatible_cast/1,
         syntax_error_1/0, syntax_error_2/0]).

-include("../../include/gradualizer.hrl").

-spec downcast_using_annotation(integer()) -> any().
downcast_using_annotation(N) ->
    ?annotate_type(N, pos_integer()).

-spec type_error_after_annotation() -> non_neg_integer().
type_error_after_annotation() ->
    receive {int, N} -> ?annotate_type(N, integer()) end.

type_error_after_annotation(X) ->
    N = ?annotate_type(X, integer()),
    atom_or_die(N).

-spec incompatible_cast() -> atom().
incompatible_cast() ->
    Atom = ?assert_type({"yyy", list_to_atom("zzzz")}, atom()),
    Atom.

-spec incompatible_cast(integer()) -> atom().
incompatible_cast(N) ->
    ?assert_type(N, atom()).

syntax_error_1() ->
    '::'(banana, "not a type").

-spec syntax_error_2() -> atom().
syntax_error_2() ->
    ':::'(banana, "not a type").

%% Used by the tests functions
-spec atom_or_die(atom()) -> ok.
atom_or_die(A) when is_atom(A) -> ok.
