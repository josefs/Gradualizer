-module(record_union_pass).

-export([f/0]).

-record(r, {}).

-type r() :: #r{}.

-spec f() -> r() | undefined.
f() ->
    #r{}.
