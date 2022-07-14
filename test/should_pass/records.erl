-module(records).

-export([f/0, f/1, g/1, h/0, i/0, j/0, k/0, l/0,
         rec_field_subtype/1,
         rec_index_subtype/0,
         record_as_tuple/1,
         nospec_update_bug/1]).

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

-record(test_k, {
    field :: integer() | undefined
}).

-spec k() -> integer() | undefined.
k() ->
    Test = #test_k{},
    Test#test_k.field.

-record(test_l, {
    field = 0 :: integer()
}).

-spec l() -> integer().
l() ->
    Test = #test_l{},
    Test#test_l.field.

-spec rec_field_subtype(#r{}) -> number().
rec_field_subtype(R) ->
    R#r.f2.

-spec rec_index_subtype() -> number().
rec_index_subtype() ->
    #r.f2.

-spec record_as_tuple(#r{}) -> tuple().
record_as_tuple(R) ->
    R.

-record(rec_any, {f}).
f(#rec_any{f = F} = R) -> F.

-record(nospec_update_bug, {
    a :: integer(),
    b :: integer()
}).

nospec_update_bug(Rec) ->
    Rec#nospec_update_bug{
        b = 0
    }.
