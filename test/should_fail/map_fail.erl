-module(map_fail).

-compile([export_all, nowarn_export_all]).

-spec mandatory_fail(#{a => b}) -> #{a := b}.
mandatory_fail(M) -> M.

-spec mandatory_fail2(#{a => b}) -> #{b := a}.
mandatory_fail2(M) -> M.

-spec mandatory_fail3(#{a := b, c := d}) -> #{b := a}.
mandatory_fail3(#{a := B, c := D} = M) -> M#{b => D, a => B}.