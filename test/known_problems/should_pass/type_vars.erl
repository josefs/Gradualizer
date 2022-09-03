-module(type_vars).

-export([foo/1,
         map_type_var/1]).

-gradualizer([solve_constraints]).

-spec foo([{integer(), integer()}]) -> [{integer(), integer()}].
foo(Pairs) ->
    pair_sort(Pairs).

-spec pair_sort([A]) -> [A] when A :: {gradualizer:top(), gradualizer:top()}.
pair_sort(Ps) -> Ps.

-spec map_type_var(nonempty_list(#{atom() => integer()} | atom())) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int;
        _ ->
            0
    end.
