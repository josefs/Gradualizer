-module(spec_and_fun_clause_intersection_fail).

-export([f/1, g/1]).

-type t() :: {tag, integer()}.


%% Order of spec clauses and function clauses does not match - should fail!
%% Strictly speaking, this does not follow the official Erlang docs to the letter,
%% but supporting out of order matching of function clauses vs spec clauses
%% is an implementation nightmare.
-spec f(t()) -> string();
       ([t()]) -> string().
f([] = _Types) -> [];
f([_|_] = Types) -> lists:map(fun f/1, Types);
f({tag, _}) -> "".

-spec g([t()]) -> string();
       (t()) -> string().
g([] = _Types) -> "";
g({tag, _}) -> "".
