-module(map_pattern).
-export([f/1, badkey/1, map_term/1]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> string().
f(#{bepa := Bepa}) ->
    Bepa.

-spec badkey(#{apa => atom()}) -> ok.
badkey(#{bepa := _Bepa}) -> ok.

-spec map_term(term()) -> any().
map_term(#{k := V}) ->
    %% at this point V :: term()
    atom_to_list(V).
