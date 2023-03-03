-module(module_info_higher_arity).

-export([two_plus_four/0, module_info/2]).

-spec module_info(number(), number()) -> number().
module_info(A, B) -> A + B.

-spec two_plus_four() -> number().
two_plus_four() -> module_info_higher_arity:module_info(2, 4).