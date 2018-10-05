-module(record_var).

-record(rec, {apa}).

-spec f(A) -> A when A :: #rec{}.
f(#rec{}) -> #rec{}.
