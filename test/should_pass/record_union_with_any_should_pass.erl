-module(record_union_with_any_should_pass).

-export([f/1,
         g/0]).

-record(r, {field}).

-spec f(_) -> #r{} | any().
f(R) ->
    R#r{field = val}.

-spec g() -> #r{} | any().
g() ->
    #r{field = val}.
