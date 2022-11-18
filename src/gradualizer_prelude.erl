%% @private
-module(gradualizer_prelude).

-compile({parse_transform, gradualizer_prelude_parse_trans}).

-export([get_modules_and_forms/0]).

-spec get_modules_and_forms() -> [{module(), gradualizer_file_utils:abstract_forms()}].
get_modules_and_forms() ->
    error(undef). % function body replaced by the parse transform
