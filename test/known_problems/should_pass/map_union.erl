-module(map_union).

-compile(export_all).

-spec union_value2(#{a => b | c}) -> #{a => b} | #{a => c}.
union_value2(M) -> M.
