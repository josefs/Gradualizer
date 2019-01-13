-module(records).

-record(r, {f1     :: atom(),
            f2 = 1 :: integer()}).

f() ->
    %% record creation
    R = #r{f2 = 2},
    %% record update
    R2 = R#r{f2 = 3},
    %% record field access
    R2#r.f2.


-spec g(#r{}) -> #r{}.
g(R) ->
    R#r{ f2 = R#r.f2 }.

-spec h() -> #r{}.
h() ->
    #r{ f1 = apa, f2 = 3 }.

i() ->
    #r.f1.

-spec j() -> 3.
j() ->
    #r.f2.

-spec rec_field_subtype(#r{}) -> number().
rec_field_subtype(R) ->
    R#r.f2.

-spec rec_index_subtype() -> number().
rec_index_subtype() ->
    #r.f2.

