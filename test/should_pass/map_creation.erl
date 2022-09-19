-module(map_creation).
-export([f/0, g/0, h/0]).

-spec f() -> #{apa := bepa}.
f() -> #{apa => bepa}.

-type typed_map() :: #{field_a := binary()}.

-spec g() -> typed_map().
g() ->
    #{field_a => <<"ala ma kota">>}.

-spec h() -> #{inner := #{a := 5}}.
h() -> #{inner => #{a => 5}}.
