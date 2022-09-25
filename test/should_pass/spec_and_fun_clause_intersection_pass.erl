-module(spec_and_fun_clause_intersection_pass).

-export([f/1, g/1, h/1, i/1]).

-spec f(bar) -> bar;
       (list()) -> baz | qux.
f(bar) -> bar;
f([]) -> baz;
f([_|_]) -> qux.

-type t() :: {tag, any()}.

%% Order of spec clauses matches the order of patterns in function clauses - this should pass.
-spec g([t()]) -> string();
       (t()) -> string().
g([] = _Types) -> [];
g([_|_] = Types) -> lists:map(fun g/1, Types);
g({tag, _}) -> "".

%% This wouldn't work if we took guards into account when matching clauses to specs.
%% It only works because any pattern matches a var in `add_type_pat()'.
%% It's a bit of a coincidence that it does - it might stop in the future,
%% so let's deem it "non-official".
-spec h(t()) -> string();
       ([t()]) -> string().
h(Types) when is_list(Types) -> lists:map(fun h/1, Types);
h({tag, _}) -> "".

%% Should this still warn about nonexhaustive patterns? Currently it doesn't.
-spec i([t()]) -> string();
       (t()) -> string().
i([_|_] = Types) -> lists:map(fun i/1, Types);
i({tag, _}) -> "".
