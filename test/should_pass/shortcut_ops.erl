-module(shortcut_ops).

-compile(export_all).

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

