-module(map_entry).
-export([f/0, g/0]).

-spec f() -> #{bepa := apa}.
f() -> #{apa => bepa}.

-type typed_map() :: #{field_a := atom()}.

-spec g() -> typed_map().
g() ->
    #{field_a => <<"ala ma kota">>}.
