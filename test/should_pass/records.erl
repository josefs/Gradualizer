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
