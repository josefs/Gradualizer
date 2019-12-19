-module(map_creation).
-export([f/0, g/0]).

-spec f() -> #{apa := bepa}.
f() -> #{apa => bepa}.

-type typed_map() :: #{field_a := binary()}.

-spec g() -> typed_map().
g() ->
    #{field_a => <<"ala ma kota">>}.
