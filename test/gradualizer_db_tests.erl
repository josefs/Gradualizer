-module(gradualizer_db_tests).

-include_lib("eunit/include/eunit.hrl").

import_module_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [?_assertMatch(not_found, gradualizer_db:import_module(hello)),
      ?_assertMatch(ok, gradualizer_db:import_module(lists))
     ]}.

%%
%% Helper functions
%%

setup_app() ->
    {ok, Apps} = application:ensure_all_started(gradualizer),
    Apps.

cleanup_app(Apps) ->
    [ok = application:stop(App) || App <- Apps],
    ok.
