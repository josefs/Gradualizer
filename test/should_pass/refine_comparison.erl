-module(refine_comparison).

-compile([export_all, nowarn_export_all]).

-spec comp1(integer()) -> pos_integer() | neg_integer().
comp1(N) when 0 < N -> N;
comp1(N) when 0 > N -> N;
comp1(_) -> 1.

-spec comp2(2..10) -> 2..5 | 8..10.
comp2(N) when N =< 5 -> N;
comp2(N) when N >= 8 -> N;
comp2(_) -> 10.

-spec comp3(1..3) -> 1 | 3.
comp3(N) when N /= 2 -> N;
comp3(_) -> 1.

-spec comp4(number()) -> 1.
comp4(N) when N =:= 1 -> N;
comp4(_) -> 1.

-spec compatom1(a | b | c) -> a | b | ok.
compatom1(X) when X == a -> X;
compatom1(X) when X =:= b -> X;
compatom1(_) -> ok.

-spec compatom2(a | b | c | pid()) -> a | c | pid().
compatom2(X) when X =/= b -> X;
compatom2(_) -> a.

-spec compatom3(a | b | c | pid()) -> a | c | pid().
compatom3(X) when X /= b -> X;
compatom3(_) -> a.

-type my_map() :: #{value := integer() | nil}.

-spec comp_map_value1(my_map()) -> integer().
comp_map_value1(State) when map_get(value, State) /= nil ->
    Val = map_get(value, State),
    Val + 1;
comp_map_value1(#{value := nil}) -> 0.

-spec comp_map_value2(my_map()) -> integer().
comp_map_value2(#{value := Val}) when Val /= nil ->
    Val + 1;
comp_map_value2(#{value := nil}) -> 0.
