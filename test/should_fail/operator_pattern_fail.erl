-module(operator_pattern_fail).
-export([n/1, p/1]).

-spec n(non_neg_integer()) -> {}.
n(1-2) -> {}.

-spec p(pos_integer()) -> {}.
p(1-1) -> {}.
