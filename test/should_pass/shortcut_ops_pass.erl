-module(shortcut_ops_pass).

-compile([export_all, nowarn_export_all]).

-spec andalso_check1(boolean(), number()) -> false | number().
andalso_check1(B, N) -> B andalso N.

-spec andalso_check2(boolean(), false | number()) -> false | number().
andalso_check2(B, N) -> B andalso N.

-spec andalso_check3(boolean(), boolean()) -> boolean() | number().
andalso_check3(B, X) -> B andalso X.

-spec orelse_check1(boolean(), number()) -> true | number().
orelse_check1(B, N) -> B orelse N.

-spec orelse_check2(boolean(), true | number()) -> true | number().
orelse_check2(B, N) -> B orelse N.

-spec orelse_check3(boolean(), boolean()) -> boolean() | number().
orelse_check3(B, X) -> B orelse X.

-spec andalso_infer1(boolean(), number()) -> _.
andalso_infer1(B, N) -> B andalso N.

-spec andalso_infer2(boolean(), false | number()) -> _.
andalso_infer2(B, N) -> B andalso N.

-spec andalso_infer3(boolean(), boolean()) -> _.
andalso_infer3(B, X) -> B andalso X.

-spec orelse_infer1(boolean(), number()) -> _.
orelse_infer1(B, N) -> B orelse N.

-spec orelse_infer2(boolean(), true | number()) -> _.
orelse_infer2(B, N) -> B orelse N.

-spec orelse_infer3(boolean(), boolean()) -> _.
orelse_infer3(B, X) -> B orelse X.

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

-spec is_false_number(false | number()) -> ok.
is_false_number(_) -> ok.

-spec is_true_number(true | number()) -> ok.
is_true_number(_) -> ok.

-spec is_bool(boolean()) -> ok.
is_bool(_) -> ok.

-spec check_inferred(boolean(), number()) -> _.
check_inferred(B, N) ->
    And1 = andalso_infer1(B, N),
    ok   = is_false_number(And1),
    And2 = andalso_infer2(B, N),
    ok   = is_false_number(And2),
    And3 = andalso_infer3(B, B),
    ok   = is_bool(And3),
    Or1  = orelse_infer1(B, N),
    ok   = is_true_number(Or1),
    Or2  = orelse_infer2(B, N),
    ok   = is_true_number(Or2),
    Or3  = orelse_infer3(B, B),
    ok   = is_bool(Or3),
    ok.

