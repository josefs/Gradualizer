-module(fun_guard_info).
-export([
         fun_wrong_arity/1,
         guard_unreachable_branch/1
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
