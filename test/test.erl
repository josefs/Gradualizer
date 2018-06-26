-module(test).

-include_lib("eunit/include/eunit.hrl").

should_pass_test_() ->
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
