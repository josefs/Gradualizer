-module(test).

-include_lib("eunit/include/eunit.hrl").

should_pass_test_() ->
    run_tests_in("test/should_pass", ok).

should_fail_test_() ->
    run_tests_in("test/should_fail", nok).

run_tests_in(Dir, ExpectedRes) ->
    {ok, Files} = file:list_dir(Dir),

    [{filename:basename(Dir) ++ ": " ++ File,
      fun() ->
              FullFile = filename:join(Dir, File),
              ?assert(ExpectedRes =:= typechecker:type_check_file(FullFile))
      end}
     || File <- Files].
