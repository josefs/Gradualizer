-module(map).

-compile([export_all, nowarn_export_all]).

-spec unknown_map1(#{a => b}) -> map().
unknown_map1(M) -> M.

-spec unknown_map2(map()) -> #{a => b}.
unknown_map2(M) -> M.

-spec mandatory_ok(#{a := b}) -> #{a => b}.
mandatory_ok(M) -> M.

-spec union_value(#{a => b} | #{a => c}) -> #{a => b | c}.
union_value(M) -> M.
