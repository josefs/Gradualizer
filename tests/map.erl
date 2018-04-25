-module(map).

-spec unknown_map1(#{a => b}) => map().
unknown_map1(M) -> M.

-spec unknown_map1(map()) -> #{a => b}.
unknown_map2(M) -> M.

-spec mandatory_ok(#{a := b}) -> #{a => b}.
mandatory_ok(M) -> M.

-spec mandatory_fail(#{a => b}) -> #{a := b}.
mandatory_fail(M) -> M.
