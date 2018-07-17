-module(gradualizer_db_tests).

-include_lib("eunit/include/eunit.hrl").

import_module_test() ->
    ?assertMatch(not_found, gradualizer_db:import_module(hello)),
    ?assertMatch(ok, gradualizer_db:import_module(lists)).
