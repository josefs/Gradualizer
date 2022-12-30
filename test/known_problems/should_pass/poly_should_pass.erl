-module(poly_should_pass).

-gradualizer([solve_constraints]).

-export([find1/0,
         f/1]).

-spec lookup(T1, [{T1, T2}]) -> (none | T2).
lookup(_, []) -> none;
lookup(K, [{K, V}|_]) -> V;
lookup(K, [_|KVs]) -> lookup(K, KVs).

-spec find1() -> string().
find1() ->
    case lookup(0, [{0, "s"}]) of
        none -> "default";
        V -> V
    end.

-type t1() :: {_, _, _, _}.
-type t2() :: {_, _, _}.

-spec f([t1() | t2()]) -> ok.
f(Fields) ->
    lists:flatmap(fun
                      ({_, _, _, _}) -> [t1];
                      ({_, _, _}) -> [t2]
                  end, Fields),
    ok.
