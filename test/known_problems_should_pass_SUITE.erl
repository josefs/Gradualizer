-module(known_problems_should_pass_SUITE).

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
              {dynamic_suite_test_path, filename:join(AppBase, "test/known_problems/should_pass")},
              {dynamic_test_template, known_problems_should_pass_template}
             ] ++ Config0,
    {ok, _} = application:ensure_all_started(gradualizer),
    ok = load_prerequisites(AppBase),
    {ok, TestNames} = gradualizer_dynamic_suite:reload(Config),
    case all_tests() of
        TestNames -> ok;
        _ -> ct:fail("Please update all_tests/0 to list all tests")
    end,
    Config.

load_prerequisites(_AppBase) ->
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
    [arith_op_arg_types,binary_exhaustiveness_checking_should_pass,
     call_intersection_function_with_union_arg_should_pass,elixir_list_first,
     error_in_guard,fun_subtyping,generator_var_shadow,
     inner_union_subtype_of_root_union,intersection_should_pass,
     intersection_with_any,list_concat_op_should_pass,list_tail,
     map_pattern_duplicate_key,maybe_expr,poly_should_pass,poly_type_vars,
     recursive_types,refine_bound_var_on_mismatch,
     refine_bound_var_with_guard_should_pass,refine_comparison_should_pass,
     refine_list_tail,union_fun].

known_problems_should_pass_template(_@File) ->
    {ok, Forms} = gradualizer_file_utils:get_forms_from_erl(_@File, []),
    ExpectedErrors = typechecker:number_of_exported_functions(Forms),
    ReturnedErrors = length(safe_type_check_file(_@File, [return_errors])),
    ?assertEqual(ExpectedErrors, ReturnedErrors).

safe_type_check_file(File) ->
    safe_type_check_file(File, []).

safe_type_check_file(File, Opts) ->
    try
        gradualizer:type_check_file(File, Opts)
    catch
        _:_ -> crash
    end.
