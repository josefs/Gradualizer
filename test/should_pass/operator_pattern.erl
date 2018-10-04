-module(operator_pattern).

-compile([export_all]).

-spec f(float()) -> {}.
f(1.0+1.0)   -> {};
f(12.5-20.3) -> {};
f(1.2*1.3)   -> {};
f(1.5/0.5)   -> {};
f(4/3)       -> {}.

-spec n(non_neg_integer()) -> {}.
n(1+1)   -> {};
n(13-12) -> {};
n(10*3)  -> {};
n(-3*-4) -> {}.

-spec i(integer()) -> {}.
i(1+1)  -> {};
i(1-5)  -> {};
i(3*-5) -> {}.

-spec p(pos_integer()) -> {}.
p(1+1) -> {};
p(2-1) -> {};
p(2*2) -> {}.
