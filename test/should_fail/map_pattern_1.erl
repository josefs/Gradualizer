-module(map_pattern_1).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> string().
f(#{bepa := Bepa}) ->
    Bepa.
