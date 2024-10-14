-module(return_fun_should_pass).

-export([return_fun_intersection/0,
        return_fun_union_intersection/0]).

-spec return_fun_intersection() -> fun((any()) -> integer()).
return_fun_intersection() -> fun number/1.

-spec return_fun_union_intersection()
                                   -> fun((atom()) -> atom()) |
                                      fun((1..3) -> integer()).
return_fun_union_intersection() -> fun number/1.

-spec number(integer()) -> integer();
            (float()) -> float().
number(N) -> N.
