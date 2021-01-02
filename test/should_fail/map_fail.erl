-module(map_fail).

-compile([export_all, nowarn_export_all]).

-spec mandatory_fail(#{a => b}) -> #{a := b}.
mandatory_fail(M) -> M.
