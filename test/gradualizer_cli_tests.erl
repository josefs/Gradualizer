-module(gradualizer_cli_tests).

%% Test for the CLI interface module, the testable part.

-include_lib("eunit/include/eunit.hrl").

help_test() ->
    ?assertEqual(help, gradualizer_cli:handle_args([])),
    ?assertEqual(help, gradualizer_cli:handle_args(["--help"])),
    ?assertEqual(help, gradualizer_cli:handle_args(["--infer", "-h", "other.junk"])).

version_test() ->
    ?assertEqual(version, gradualizer_cli:handle_args(["--version"])).

defaults_single_file_test() ->
    ?assertEqual({ok, ["file.erl"], []},
                 gradualizer_cli:handle_args(["file.erl"])).
defaults_multi_file_test() ->
    ?assertEqual({ok, ["m1.erl", "m2.erl"], [print_file]},
                 gradualizer_cli:handle_args(["m1.erl", "m2.erl"])).
defaults_dir_test() ->
    ?assertEqual({ok, ["test/dir"], [print_file]},
                 gradualizer_cli:handle_args(["test/dir"])).

no_file_test() ->
    ?assertMatch({error, "No files"++_},
                 gradualizer_cli:handle_args(["--infer"])).

invalid_arg_test() ->
    ?assertMatch({error, "Unknown"++_},
                 gradualizer_cli:handle_args(["--invalid-arg", "file.erl"])).

infer_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--infer", "--", "file.erl"]),
    ?assert(proplists:get_bool(infer, Opts)).
no_infer_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--no_infer", "file.erl"]),
    ?assertNot(proplists:get_bool(infer, Opts)).

verbose_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--verbose", "file.erl"]),
    ?assert(proplists:get_bool(verbose, Opts)).

include_multi_wrong_syntax_test() ->
    {ok, Files, Opts} = gradualizer_cli:handle_args(["-I", "inc1", "inc2", "--", "file.erl"]),
    ?assertEqual(["inc1"], proplists:get_all_values(i, Opts)),
    ?assertEqual(["inc2", "--", "file.erl"], Files).

include_multi_test() ->
    {ok, Files, Opts} = gradualizer_cli:handle_args(["-I", "inc1", "-I", "inc2", "file.erl"]),
    ?assertEqual(["inc2", "inc1"], proplists:get_all_values(i, Opts)),
    ?assertEqual(["file.erl"], Files).

include_error_test_() ->
    [?_assertMatch({error, "No files"++_}, gradualizer_cli:handle_args(["-I", "include"])),
     ?_assertMatch({error, "Missing "++_}, gradualizer_cli:handle_args(["-I"]))].

pa_test_() ->
    %% Skipping the successful cases which have side-effects of calling
    %% code:add_pathsa/1 directly, e.g. ["-pa", "dir1", "dir2", "--", "file.erl"]
    [?_assertMatch({error, "No files"++_}, gradualizer_cli:handle_args(["-pa", "ebin"])),
     ?_assertMatch({error, "No files"++_}, gradualizer_cli:handle_args(["--path_add", "ebin", "file.erl"])),
     ?_assertMatch({error, "No files"++_}, gradualizer_cli:handle_args(["-pa", "ebin", "file.erl"])),
     ?_assertMatch({error, "Missing "++_}, gradualizer_cli:handle_args(["-pa", "--", "file.erl"]))].

print_file_true_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--print_file", "file.erl"]),
    ?assertEqual(true, proplists:get_value(print_file, Opts)).
print_file_module_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--print_module", "file.erl"]),
    ?assertEqual(module, proplists:get_value(print_file, Opts)).
print_file_basename_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--print_basename", "file.erl"]),
    ?assertEqual(basename, proplists:get_value(print_file, Opts)).
print_file_false_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--no_print_file", "file.erl"]),
    ?assertEqual(false, proplists:get_value(print_file, Opts)).

stop_on_first_error_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--stop_on_first_error", "file.erl"]),
    ?assert(proplists:get_bool(stop_on_first_error, Opts)).
no_stop_on_first_error_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--no_stop_on_first_error", "file.erl"]),
    ?assertEqual(false, proplists:get_bool(stop_on_first_error, Opts)).
crash_on_error_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--crash_on_error", "file.erl"]),
    ?assertEqual(true, proplists:get_value(crash_on_error, Opts)).
no_crash_on_error_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--no_crash_on_error", "file.erl"]),
    ?assertEqual(false, proplists:get_value(crash_on_error, Opts)).

fmt_location_brief_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--fmt_location", "brief", "file.erl"]),
    ?assertEqual(brief, proplists:get_value(fmt_location, Opts)).
fmt_location_verbose_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--fmt_location", "verbose", "file.erl"]),
    ?assertEqual(verbose, proplists:get_value(fmt_location, Opts)).
fmt_location_none_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--fmt_location", "none", "file.erl"]),
    ?assertEqual(none, proplists:get_value(fmt_location, Opts)).
fmt_location_invalid1_test() ->
    ?assertMatch({error, _}, gradualizer_cli:handle_args(["--fmt_location", "backwards", "file.erl"])).
fmt_location_invalid2_test() ->
    ?assertMatch({error, _}, gradualizer_cli:handle_args(["--fmt_location", "normal", "file.erl"])).

prelude_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--no_prelude", "file.erl"]),
    ?assertEqual(false, proplists:get_value(prelude, Opts)).

specs_override_dir_test() ->
    {ok, _Files, Opts} = gradualizer_cli:handle_args(["--specs_override_dir", "dir", "file.erl"]),
    ?assertEqual("dir", proplists:get_value(specs_override, Opts)).
specs_override_2_dirs_test() ->
    %% Accepts single arg; inconsistent with -pa and -I:
    {ok, Files, Opts} = gradualizer_cli:handle_args(["--specs_override_dir", "d1", "d2", "--", "file.erl"]),
    ?assertEqual(["d1"], proplists:get_all_values(specs_override, Opts)),
    ?assertEqual(["d2", "--", "file.erl"], Files).
specs_override_fail_test() ->
    ?assertMatch({error, _}, gradualizer_cli:handle_args(["--specs_override_dir"])).
