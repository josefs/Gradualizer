-module(preludes).

-compile([export_all, nowarn_export_all]).

%% use lists:sort (which only accepts list() not all term()) to test
%% that V* is any()
app_get_env() ->
    {ok, V1} = application:get_env(param),
    lists:sort(V1),
    {ok, V2} = application:get_env(app, param),
    lists:sort(V2),
    V3 = application:get_env(app, param, default),
    lists:sort(V3).
