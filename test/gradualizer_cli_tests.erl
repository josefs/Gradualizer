-module(gradualizer_cli_tests).

%% Test for the CLI interface module, the testable part.

-include_lib("eunit/include/eunit.hrl").

help_test() ->
    ?assertEqual(help, gradualizer_cli:handle_args([])),
    ?assertEqual(help, gradualizer_cli:handle_args(["--help"])),
    ?assertEqual(help, gradualizer_cli:handle_args(["-h", "other.junk"])).

help_output_no_halt_test() ->
    %% This gives code coverage to the printing of the help text.
    ?assertEqual(ok, gradualizer_cli:main(["-h"])).

version_test() ->
    ?assertEqual(version, gradualizer_cli:handle_args(["--version"])).

no_file_test() ->
    ?assertMatch({error, "No files"++_},
                 gradualizer_cli:handle_args(["--solve_constraints"])).

invalid_arg_test() ->
    ?assertMatch({error, "Unknown"++_},
                 gradualizer_cli:handle_args(["--invalid-arg", "file.erl"])).

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

color_test() ->
    {ok, _Files, Opts1} = gradualizer_cli:handle_args(["--color", "auto", "file.erl"]),
    ?assertEqual(auto, proplists:get_value(color, Opts1)),
    {ok, _Files, Opts2} = gradualizer_cli:handle_args(["--color", "file.erl"]),
    ?assertEqual(always, proplists:get_value(color, Opts2)),
    {ok, _Files, Opts3} = gradualizer_cli:handle_args(["--no_color", "file.erl"]),
    ?assertEqual(never, proplists:get_value(color, Opts3)).

fancy_test() ->
    {ok, _Files, Opts1} = gradualizer_cli:handle_args(["--fancy", "file.erl"]),
    ?assertEqual(true, proplists:get_value(fancy, Opts1)),
    {ok, _Files, Opts2} = gradualizer_cli:handle_args(["--no_fancy", "file.erl"]),
    ?assertEqual(false, proplists:get_value(fancy, Opts2)).

union_size_limit_test() ->
    {ok, _Files, Opts1} = gradualizer_cli:handle_args(["--union_size_limit", "60", "file.erl"]),
    ?assertEqual(60, proplists:get_value(union_size_limit, Opts1)),
    {ok, _Files, Opts2} = gradualizer_cli:handle_args(["--union_size_limit", "10", "file.erl"]),
    ?assertEqual(10, proplists:get_value(union_size_limit, Opts2)).
