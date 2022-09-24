-module(spec_and_fun_clause_intersection_should_pass).

-export([f/1]).

-type t() :: {tag, any()}.

-spec f(t()) -> string();
       ([t()]) -> string().
f(Types) when is_list(Types) -> lists:map(fun f/1, Types);
f({tag, _}) -> "".
