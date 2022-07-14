-module(shortcut_ops_fail).

-compile([export_all, nowarn_export_all]).

-spec andalso_too_precise1(true, number()) -> number().
andalso_too_precise1(True, N) -> True andalso N.

-spec andalso_too_precise2(false, number()) -> false.
andalso_too_precise2(False, N) -> False andalso N.

-spec orelse_too_precise1(false, number()) -> number().
orelse_too_precise1(False, N) -> False orelse N.

-spec orelse_too_precise2(true, number()) -> true.
orelse_too_precise2(True, N) -> True orelse N.

