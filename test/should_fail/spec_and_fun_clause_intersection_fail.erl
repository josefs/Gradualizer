-module(spec_and_fun_clause_intersection_fail).

-export([f/1, g/1, i/1]).

-type t() :: {tag, integer()}.

-spec f([t()]) -> string();
       (t()) -> string().
f(asd) -> "";
f({tag, _}) -> "".

-spec g([t()]) -> string();
       (t()) -> string().
g([] = _Types) -> "";
g({tag, _}) -> "".

-spec i([t()]) -> [string()];
       (t()) -> string().
i([_|_] = Types) -> lists:map(fun i/1, Types);
i({tag, _}) -> "".
