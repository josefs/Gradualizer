-module(pattern_record).

-compile(export_all).

-record(test, {n :: integer()}).
-spec test42(#test{n :: 42} | bananas) -> #test{n :: 42} | not_test.
test42(#test{} = Test) -> Test;
test42(_) -> not_test.
