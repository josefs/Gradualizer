-module(should_pass_SUITE).

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
    Opts = [
            {dynamic_suite_module, ?MODULE},
            {dynamic_suite_test_path, filename:join(code:lib_dir(gradualizer), "test/should_pass")},
            {dynamic_test_template, should_pass_template}
           ],
    {ok, GeneratedTests} = gradualizer_dynamic_suite:reload(Opts),
    [{all_tests, [parallel], GeneratedTests}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(code:lib_dir(gradualizer)),
    Config.

load_prerequisites(AppBase) ->
    %% user_types.erl is referenced by remote_types.erl and opaque.erl.
    %% It is not in the sourcemap of the DB so let's import it manually
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/user_types.erl")]),
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/other_module.erl")]),
    %% imported.erl references any.erl
    gradualizer_db:import_erl_files([filename:join(AppBase, "test/should_pass/any.erl")]),
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

should_pass_template(_@File) ->
    ?assertEqual(ok, gradualizer:type_check_file(_@File, [{form_check_timeout_ms, 2000}])).
