-module(fun_guard_info).
-export([
         guard_is_atom/1,
         remote_guard/1,
         fun_correct_arity/1,
         fun_unspecified_arity/1,
         fun_different_variables/2
        ]).

-spec guard_is_atom(atom() | list()) -> list().
guard_is_atom(Name) when is_atom(Name) ->
    erlang:atom_to_list(Name);
guard_is_atom(_Name) ->
    [].

-spec remote_guard(atom() | list()) -> list().
remote_guard(Name) when erlang:is_atom(Name) ->
    erlang:atom_to_list(Name);
remote_guard(_Name) ->
    [].

-spec fun_correct_arity(any()) -> boolean().
fun_correct_arity(Fun) when is_function(Fun, 2) ->
    Fun(0, 1);
fun_correct_arity(_Fun) -> true.

-spec fun_unspecified_arity(any()) -> boolean().
fun_unspecified_arity(Fun) when is_function(Fun) ->
    Fun(0,1);
fun_unspecified_arity(_Fun) -> true.

-spec fun_different_variables(any(), any()) -> any().
fun_different_variables(A,B)
  when is_atom(A), is_integer(B);
       is_integer(A), is_atom(B) ->
    ok.
