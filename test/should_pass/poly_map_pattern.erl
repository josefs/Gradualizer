-module(poly_map_pattern).

-gradualizer([solve_constraints]).

-export([map_type_var/1]).

-spec map_type_var(nonempty_list(#{atom() => integer()})) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.
