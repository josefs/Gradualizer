-module(spec_and_fun_clause_intersection_pass).

-export([f/1, g/1, g1/1, g2/1, h/1]).

-spec f(bar) -> bar;
       (list()) -> baz | qux.
f(bar) -> bar;
f([]) -> baz;
f([_|_]) -> qux.

-type t() :: {tag, integer()}.

%% Order of spec clauses matches the order of patterns in function clauses - this should pass.
-spec g([t()]) -> string();
       (t()) -> string().
g([] = _Types) -> [];
g([_|_] = Types) -> lists:map(fun g/1, Types);
g({tag, _}) -> "".

%% Order of spec clauses doesn't match the order of patterns
%% in function clauses - this should pass, too.
-spec g1(t()) -> string();
        ([t()]) -> string().
g1([] = _Types) -> [];
g1([_|_] = Types) -> lists:map(fun g1/1, Types);
g1({tag, _}) -> "".

%% Order of spec clauses doesn't match the order of patterns
%% in function clauses and function clause patterns are mixed - will that pass?
-spec g2(t()) -> string();
        ([t()]) -> string().
g2([] = _Types) -> [];
g2({tag, _}) -> "";
g2([_|_] = Types) -> lists:map(fun g2/1, Types).

-spec h(t()) -> string();
       ([t()]) -> string().
h(Types) when is_list(Types) -> lists:map(fun h/1, Types);
h({tag, _}) -> "".
