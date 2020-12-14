-module(map_entry).

-compile(export_all).

-spec f() -> #{bepa := apa}.
f() -> #{apa => bepa}.

-type typed_map() :: #{field_a := atom()}.

-spec g() -> typed_map().
g() ->
    #{field_a => <<"ala ma kota">>}.

-spec h() -> typed_map().
h() ->
    #{field_a => 42}.

-spec i() -> #{field_a := integer()}.
i() ->
    #{field_a => <<"This is not the integer you are looking for">>}.

-spec j() -> #{field_a := integer()}.
j() ->
    #{field_b => <<"This is not the key you are looking for">>}.