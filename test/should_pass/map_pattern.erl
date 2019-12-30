-module(map_pattern).

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

-spec map_type_var(nonempty_list(#{atom() => integer()} | atom())) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.

