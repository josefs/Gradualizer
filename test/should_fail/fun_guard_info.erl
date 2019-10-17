-module(fun_guard_info).
-export([
         fun_wrong_arity/1,
         guard_unreachable_branch/1,
         at_least_one_of_them_is_an_atom/2,
         refined_types_makes_it_fail/2
        ]).

-spec fun_wrong_arity(any()) -> boolean().
fun_wrong_arity(Fun) when is_function(Fun, 2) ->
    Fun(0);
fun_wrong_arity(_Fun) -> true.

-spec guard_unreachable_branch(atom() | list()) -> list().
guard_unreachable_branch(Name)
  when is_atom(Name), is_list(Name) ->
    erlang:atom_to_list(Name);
guard_unreachable_branch(_Name) ->
    [].

% We know here that for sure at least one of them is an atom,
% so this operation has no chance to pass.
-spec at_least_one_of_them_is_an_atom(any(), any()) -> any().
at_least_one_of_them_is_an_atom(A,B)
  when is_atom(A), is_integer(B);
       is_integer(A), is_atom(B) ->
    A + B.

% Here we know that one is an atom and the other is an integer,
% or similarly, that A can be either an atom or an integer.
% Hence the operation `atom_to_list` is not ensured safe.
-spec refined_types_makes_it_fail(any(), any()) -> any().
refined_types_makes_it_fail(A,B)
  when is_atom(A), is_integer(B);
       is_integer(A), is_atom(B) ->
    erlang:atom_to_list(A).
