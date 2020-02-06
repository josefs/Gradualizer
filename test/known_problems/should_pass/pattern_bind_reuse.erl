-module(pattern_bind_reuse).

-compile(export_all).

-spec test(integer() | undefined, integer()) -> integer().
test(I, I) -> I + I;
test(_, I) -> I.
