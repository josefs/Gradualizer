-module(known_problems_should_fail_SUITE).

-compile([export_all, nowarn_export_all]).

%% EUnit has some handy macros, so let's use it, too
-include_lib("eunit/include/eunit.hrl").

%% Test server callbacks
-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1, end_per_suite/1]).

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [{group, all_tests}].

groups() ->
    Config = [
              {dynamic_suite_module, ?MODULE},
              {dynamic_suite_test_path, filename:join(code:lib_dir(gradualizer), "test/known_problems/should_fail")},
              {dynamic_test_template, known_problems_should_fail_template}
             ],
    {ok, GeneratedTests} = gradualizer_dynamic_suite:reload(Config),
    [{all_tests, [parallel], GeneratedTests}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(code:lib_dir(gradualizer)),
    Config.

load_prerequisites(AppBase) ->
    %% exhaustive_user_type.erl is referenced by exhaustive_remote_user_type.erl
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_fail/exhaustive_user_type.erl")]),
    ok.

end_per_suite(_Config) ->
    ok = application:stop(gradualizer),
    ok.

known_problems_should_fail_template(_@File) ->
    Result = safe_type_check_file(_@File, [return_errors, {form_check_timeout_ms, 2000}]),
    case Result of
        crash ->
            ok;
        Errors ->
            ErrorsExceptTimeouts = lists:filter(
                                     fun ({_File, {form_check_timeout, _}}) -> false; (_) -> true end,
                                     Errors),
            ?assertEqual(0, length(ErrorsExceptTimeouts))
    end.

safe_type_check_file(File) ->
    safe_type_check_file(File, []).

safe_type_check_file(File, Opts) ->
    try
        gradualizer:type_check_file(File, Opts)
    catch
        _:_ -> crash
    end.
