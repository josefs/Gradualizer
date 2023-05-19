-module(recursive_types_failing).

-export([recursive_param2/1]).

-type rec2() :: rec2 | {rec2, rec2()} | {rec2, {rec2, rec2()}}.

%% `{_, Z}' doesn't match on atom `rec2', which is a valid value of type `rec2()'.
%% This should fail.
-spec recursive_param2(rec2()) -> rec2().
recursive_param2({_, Z}) -> Z.