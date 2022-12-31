-module(poly_union_lower_bound_pass).

-gradualizer([solve_constraints]).

-export([f/1,
         g/1,
         i/1]).

-type t1() :: {_, _, _, _}.
-type t2() :: {_, _, _}.

-spec f([t1() | t2()]) -> ok.
f(Fields) ->
    lists:flatmap(fun
                      ({_, _, _, _}) -> [t1];
                      ({_, _, _}) -> [t2]
                  end, Fields),
    ok.

-spec g([t1() | t2()]) -> ok.
g(Fields) ->
    lists:foldl(fun
                    ({_, _, _, _}, Acc) -> [t1 | Acc];
                    (_, Acc) -> [default | Acc]
                end, [], Fields),
    ok.

-spec i([binary() | integer()]) -> [integer()].
i(L) ->
    lists:map(fun takes_a_union/1, L).

-spec takes_a_union(binary() | integer()) -> integer().
takes_a_union(B) when is_binary(B) -> binary_to_integer(B);
takes_a_union(I) when is_integer(I) -> I.
