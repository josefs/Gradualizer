-module(pattern_record_fail).

-compile([export_all, nowarn_export_all]).

-record(r1, {
    f
}).

-record(r2, {
    f
}).

-spec bad_match1(#r1{} | other) -> #r1{} | not_r1.
bad_match1(R = #r2{}) -> R;
bad_match1(_) -> not_r1.

-spec bad_match2(#r1{} | #r2{}) -> #r1{} | not_r1.
bad_match2(R = #r2{}) -> R;
bad_match2(_) -> not_r1.

-spec fail(#r1{} | #r2{}) -> integer().
fail(R = #r2{f = F}) -> R#r1.f + F;
fail(_) -> 0.

