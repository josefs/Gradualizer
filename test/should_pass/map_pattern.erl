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
