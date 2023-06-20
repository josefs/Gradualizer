-module(shortcut_ops_should_pass).

-compile([export_all, nowarn_export_all]).

-spec andalso_infer_and_check1(boolean(), number()) -> false | number().
andalso_infer_and_check1(B, N) ->
    X = B andalso N,
    X.

-spec andalso_infer_and_check2(boolean(), false | number()) -> false | number().
andalso_infer_and_check2(B, N) ->
    X = B andalso N,
    X.

-spec orelse_infer_and_check1(boolean(), number()) -> true | number().
orelse_infer_and_check1(B, N) ->
    X = B orelse N,
    X.

-spec orelse_infer_and_check2(boolean(), true | number()) -> true | number().
orelse_infer_and_check2(B, N) ->
    X = B orelse N,
    X.