-module(map_pattern_should_pass).

-export([map_type_var/1]).

-spec map_type_var(nonempty_list(#{atom() => integer()} | atom())) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.
