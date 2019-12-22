-module(map_as_argument_update).
-compile(export_all).

%% See https://github.com/josefs/Gradualizer/pull/233 for the bug report,
%% and https://github.com/josefs/Gradualizer/pull/237 for the fix.

-spec t(list()) -> map().
t(Rows) ->
    lists:foldl(fun(Id, Map) ->
                        Map#{id := Id}
                end, #{id => undefined}, Rows).
