-module(spec_and_fun_clause_intersection_fail).

-export([g/1]).

-type t() :: {tag, any()}.

%% Order of spec clauses and function clauses does not match - should fail!
%% Strictly speaking, this does not follow the official Erlang docs to the letter,
%% but supporting out of order matching of function clauses vs spec clauses
%% is an implementation nightmare.
-spec g(t()) -> string();
       ([t()]) -> string().
g([] = _Types) -> [];
g([_|_] = Types) -> lists:map(fun g/1, Types);
g({tag, _}) -> "".
