-module(should_fail_SUITE).

-compile([export_all, nowarn_export_all]).

%% EUnit has some handy macros, so let's use it, too
-include_lib("eunit/include/eunit.hrl").

%% Test server callbacks
-export([suite/0,
         all/0,
         groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [{group, all_tests}].

groups() ->
    Config = [
              {dynamic_suite_module, ?MODULE},
              {dynamic_suite_test_path, filename:join(code:lib_dir(gradualizer), "test/should_fail")},
              {dynamic_test_template, should_fail_template}
             ],
    {ok, GeneratedTests} = gradualizer_dynamic_suite:reload(Config),
    [{all_tests, [parallel], GeneratedTests}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(code:lib_dir(gradualizer)),
    Config.

load_prerequisites(AppBase) ->
    %% user_types.erl is referenced by opaque_fail.erl.
    %% It is not in the sourcemap of the DB so let's import it manually
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/user_types.erl")]),
    %% exhaustive_user_type.erl is referenced by exhaustive_remote_user_type.erl
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_fail/exhaustive_user_type.erl")]),
    ok.

end_per_suite(_Config) ->
    ok = application:stop(gradualizer),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

should_fail_template(_@File) ->
    Errors = gradualizer:type_check_file(_@File, [return_errors, {form_check_timeout_ms, 2000}]),
    Timeouts = [ E || {_File, {form_check_timeout, _}} = E <- Errors],
    ?assertEqual(0, length(Timeouts)),
    %% Test that error formatting doesn't crash
    Opts = [{fmt_location, brief},
            {fmt_expr_fun, fun erl_prettypr:format/1}],
    lists:foreach(fun({_, Error}) -> gradualizer_fmt:handle_type_error(Error, Opts) end, Errors),
    {ok, Forms} = gradualizer_file_utils:get_forms_from_erl(_@File, []),
    ExpectedErrors = typechecker:number_of_exported_functions(Forms),
    ?assertEqual(ExpectedErrors, length(Errors)).
