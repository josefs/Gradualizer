-module(poly_map_type_var_should_fail).

-export([map_type_var/1]).

-gradualizer([solve_constraints]).

-spec map_type_var(nonempty_list(#{atom() => integer()} | atom())) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.
