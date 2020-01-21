-module(refine_comparison).

-export([comp1/1, comp2/1, comp3/1, compatom1/1]).

-spec comp1(integer()) -> pos_integer() | neg_integer().
comp1(N) when N > 0 -> N;
comp1(N) when N < 0 -> N;
comp1(_) -> 1.

-spec comp2(2..10) -> 2..5 | 8..10.
comp2(N) when N =< 5 -> N;
comp2(N) when N >= 8 -> N;
comp2(_) -> 10.

-spec comp3(1..3) -> 1 | 3.
comp4(N) when N /= 2 -> N;
comp4(_) -> 1.

-spec compatom1(a | b | c) -> a | b | ok.
compatom1(X) when X == a -> X;
compatom1(X) when X =:= b -> X;
compatom1(_) -> ok.

-spec compatom2(a | b | c | pid()) -> a | c | pid().
compatom1(X) when X =/= b -> X;
compatom1(_) -> a.
