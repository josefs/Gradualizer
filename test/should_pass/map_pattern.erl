-module(map_pattern).

-export([f/1,
         key_subtype/1,
         map_union/1,
         any_map/1,
         map_term/1,
         map_type_var/1,
         map_pattern_no_spec/1]).

-gradualizer([solve_constraints]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> boolean().
f(#{bepa := Bepa}) ->
    Bepa.

-spec key_subtype(#{atom() => integer()}) -> integer().
key_subtype(#{banana := N}) ->
    N.

-spec map_union(#{a => atom()} | #{b := boolean()}) -> atom().
map_union(#{a := A}) -> A;
map_union(#{b := B}) -> B.

-spec any_map(map()) -> ok.
any_map(#{apa := _}) -> ok.

-spec map_term(gradualizer:top()) -> any().
map_term(#{}) ->
    ok.

-spec map_type_var(nonempty_list(#{atom() => integer()})) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.

map_pattern_no_spec(#{} = _Map) -> ok.
