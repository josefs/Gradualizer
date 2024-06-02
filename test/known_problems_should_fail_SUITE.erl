-module(known_problems_should_fail_SUITE).

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

init_per_suite(Config0) ->
    AppBase = code:lib_dir(gradualizer),
    Config = [
              {dynamic_suite_module, ?MODULE},
              {dynamic_suite_test_path, filename:join(AppBase, "test/known_problems/should_fail")},
              {dynamic_test_template, known_problems_should_fail_template}
             ] ++ Config0,
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(AppBase),
    {ok, TestNames} = gradualizer_dynamic_suite:reload(Config),
    case all_tests() of
        TestNames -> ok;
        _ -> ct:fail("Please update all_tests/0 to list all tests")
    end,
    Config.

load_prerequisites(AppBase) ->
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

all() ->
    [{group, all_tests}].

groups() ->
    [{all_tests, [parallel], all_tests()}].

all_tests() ->
    [arith_op,binary_comprehension,case_pattern_should_fail,
     exhaustive_argumentwise,exhaustive_expr,exhaustive_map_variants,
     exhaustive_remote_map_variants,guard_should_fail,infer_any_pattern,
     intersection_with_any_should_fail,intersection_with_unreachable,
     lambda_wrong_args,map_refinement_fancy,poly_lists_map_should_fail,
     poly_should_fail,recursive_types_should_fail,refine_ty_vars,sample].

known_problems_should_fail_template(_@File) ->
    Result = safe_type_check_file(_@File, [return_errors]),
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
