-module(record_union_pass).

-export([f/0,
         g/0,
         h/0]).

-record(r, {}).

-type r() :: #r{}.

-spec f() -> r() | undefined.
f() ->
    #r{}.

-record(s, {}).

-record(t, {field1 :: integer()}).

-type u() :: #r{} | #s{}.

-spec g() -> u() | #t{} | undefined.
g() ->
    #s{}.

-spec h() -> #t{}.
h() ->
    helper(#t{field1 = 1}).

-spec helper(#t{}) -> #t{}.
helper(T) -> T.
