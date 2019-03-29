-module(test).

-include_lib("eunit/include/eunit.hrl").

should_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [{generator, fun gen_should_pass/0},
      {generator, fun gen_should_fail/0},
      {generator, fun gen_known_problem_should_pass/0},
      {generator, fun gen_known_problem_should_fail/0}
     ]}.

gen_should_pass() ->
    {setup,
     fun() ->
             %% user_types.erl references remote_types.erl
             %% it is not in the sourcemap of the DB so let's import it manually
             gradualizer_db:import_erl_files(["test/should_pass/user_types.erl"]),
             %% imported.erl references any.erl
             gradualizer_db:import_erl_files(["test/should_pass/any.erl"])
     end,
     map_erl_files(
       fun(File) ->
               ?_assertEqual(ok, gradualizer:type_check_file(File))
       end, "test/should_pass")
    }.

gen_should_fail() ->
    map_erl_files(
      fun(File) ->
              fun() ->
                      Errors = gradualizer:type_check_file(File, [return_errors]),
                      %% Test that error formatting doesn't crash
                      Opts = [{fmt_location, brief},
                              {fmt_expr_fun, fun erl_prettypr:format/1}],
                      lists:foreach(fun({_, Error}) -> typechecker:handle_type_error(Error, Opts) end, Errors),
                      {ok, Forms} = gradualizer_file_utils:get_forms_from_erl(File),
                      ExpectedErrors = typechecker:number_of_exported_functions(Forms),
                      ?_assertEqual(ExpectedErrors, length(Errors))
              end
      end, "test/should_fail").

% Test succeeds if Gradualizer crashes or if it doesn't type check.
% Doing so makes the test suite notify us whenever a known problem is resolved.
gen_known_problem_should_pass() ->
    map_erl_files(
      fun(File) ->
              ?_assertNotEqual(ok, safe_type_check_file(File))
      end, "test/known_problems/should_pass").

% Test succeeds if Gradualizer crashes or if it does type check.
% Doing so makes the test suite notify us whenever a known problem is resolved.
gen_known_problem_should_fail() ->
    map_erl_files(
      fun(File) ->
              ?_assertNotEqual(nok, safe_type_check_file(File))
      end, "test/known_problems/should_fail").

%%
%% Helper functions
%%

setup_app() ->
    {ok, Apps} = application:ensure_all_started(gradualizer),
    Apps.

cleanup_app(Apps) ->
    [ok = application:stop(App) || App <- Apps],
    ok.

map_erl_files(Fun, Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "*.erl")),
    [{filename:basename(File), Fun(File)} || File <- Files].

safe_type_check_file(File) ->
    try
        gradualizer:type_check_file(File)
    catch
        _:_ -> crash
    end.
