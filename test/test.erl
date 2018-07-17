-module(test).

-include_lib("eunit/include/eunit.hrl").

should_pass_test_() ->
    %% user_types.erl references remote_types.erl
    %% it is not in the sourcemap of the DB so let's import it manually
    gradualizer_db:import_files(["test/should_pass/user_types.erl"]),
    run_tests_in("test/should_pass", ok).

should_fail_test_() ->
    run_tests_in("test/should_fail", nok).

run_tests_in(Dir, ExpectedRes) ->
    Files = filelib:wildcard(filename:join(Dir, "*.erl")),

    [{filename:basename(Dir) ++ ": " ++ filename:basename(File),
      fun() ->
              ?assert(ExpectedRes =:= gradualizer:type_check_file(File))
      end}
     || File <- Files].
