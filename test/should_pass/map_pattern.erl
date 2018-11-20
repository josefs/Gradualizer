-module(map_pattern).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> boolean().
f(#{bepa := Bepa}) ->
    Bepa.
