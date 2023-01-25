-module(map_infer_pass).

-export([not_good/1,
         kaboom1/0,
         kaboom2/0]).

-gradualizer([infer]).

-spec not_good(#{good | bad := integer()}) -> integer().
not_good(#{good := _}) -> 0;
not_good(#{bad := _}) -> 1.

-spec kaboom1() -> integer().
kaboom1() ->
    M = #{bad => 0},
    not_good(M).

-spec kaboom2() -> integer().
kaboom2() ->
    not_good(#{bad => 0}).
