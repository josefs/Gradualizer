-module(spec_and_fun_clause_intersection_pass).

-export([foo/1, g/1]).

-spec foo(bar) -> bar;
         (list()) -> baz | qux.
foo(bar) -> bar;
foo([]) -> baz;
foo([_|_]) -> qux.

-type t() :: {tag, any()}.

-spec g(t()) -> string();
       ([t()]) -> string().
g(Types) when is_list(Types) -> lists:map(fun g/1, Types);
g({tag, _}) -> "".
