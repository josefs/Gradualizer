-module(fun_subtyping).

-export([return_fun_intersection/0]).

-spec return_fun_intersection() -> fun((number()) -> number()).
return_fun_intersection() -> fun number/1.

-spec number(integer()) -> integer();
            (float()) -> float().
number(N) -> N.
