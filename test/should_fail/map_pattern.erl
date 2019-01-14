-module(map_pattern).
-export([f/1, badkey/1]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> string().
f(#{bepa := Bepa}) ->
    Bepa.

-spec badkey(#{apa => atom()}) -> ok.
badkey(#{bepa := _Bepa}) -> ok.
