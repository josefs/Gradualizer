-module(variable_binding_leaks).

-export([case_clauses/1]).

-spec case_clauses(foo | integer()) -> integer().
case_clauses(X) ->
    case X of
        Y = foo -> length(atom_to_list(Y));
        Y       -> Y + 1  % a different Y
    end.
