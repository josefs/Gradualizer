-module(gradualizer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(passing, "test/should_pass/any.erl").
-define(failing, "test/should_fail/arg.erl").

global_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [{generator, fun type_check_erl_file/0},
      {generator, fun type_check_erl_files/0},
      {generator, fun type_check_forms/0},
      {generator, fun type_check_beam_file/0},
      {generator, fun type_check_module/0},
      {generator, fun type_check_dir/0},
      {generator, fun not_found/0},
      {generator, fun bad_content/0},
      {generator, fun beam_without_forms/0}
     ]}.

type_check_erl_file() ->
    [?_assertEqual(ok, gradualizer:type_check_file(?passing)),
     ?_assertEqual([], gradualizer:type_check_file(?passing, [return_errors])),
     ?_assertEqual(nok, gradualizer:type_check_file(?failing)),
     ?_assertMatch([_|_], gradualizer:type_check_file(?failing, [return_errors]))
    ].

type_check_erl_files() ->
    [
     ?_assertEqual(ok, gradualizer:type_check_files([?passing, ?passing])),
     ?_assertEqual(nok, gradualizer:type_check_files([?failing, ?failing])),
     ?_assertEqual(nok, gradualizer:type_check_files([?failing, ?failing],
                                                     [stop_on_first_error])),
     ?_assertMatch([_|_], gradualizer:type_check_files([?failing, ?failing], [return_errors])),
     ?_assertMatch([_|_], gradualizer:type_check_files([?failing, ?failing],
                                                       [return_errors, stop_on_first_error])),
     ?_assertEqual([], gradualizer:type_check_files([?passing, ?passing], [return_errors]))
    ].

type_check_forms() ->
    {ok, PassingForms} = epp:parse_file(?passing, []),
    %% Drop the file attribute to check that type_check_forms works without it
    [{attribute, _, file, _} | PassingFormsNoFile] = PassingForms,
    [
     ?_assertEqual(ok, gradualizer:type_check_forms(PassingForms, [])),
     ?_assertEqual(ok, gradualizer:type_check_forms(PassingFormsNoFile, []))
    ].

type_check_beam_file() ->
    Dir = filename:dirname(?FILE), % this differs when /not/ using rebar
    BeamFile = filename:join(Dir, "any.beam"),
    ?_assertEqual(ok, gradualizer:type_check_file(BeamFile)).

type_check_module() ->
    {module, Mod} = code:load_file(any),
    ?_assertEqual(ok, gradualizer:type_check_module(Mod)).

type_check_dir() ->
    ?_assertEqual(nok, gradualizer:type_check_dir("test/dir/")).

not_found() ->
    [
     ?_assertThrow({file_not_found, "test/not_found.erl"},
                   gradualizer:type_check_file("test/not_found.erl")),
     ?_assertThrow({file_not_found, "test/not_found.beam"},
                   gradualizer:type_check_file("test/not_found.beam")),
     ?_assertThrow({beam_not_found, non_existing},
                   gradualizer:type_check_module(not_found)),
     ?_assertThrow({dir_not_found, "test/not_found/"},
                   gradualizer:type_check_dir("test/not_found/")),
     ?_assertThrow({unknown_file_extension, ".bad_ext"},
                   gradualizer:type_check_file("test/not_found.bad_ext")),
     ?_assertThrow({beam_not_found, preloaded},
                   gradualizer:type_check_module(erlang))
    ].

bad_content() ->
    {setup,
     fun() -> file:write_file("test/bad_content.beam", "bad content") end,
     fun(_) -> file:delete("test/bad_content.beam") end,
     ?_assertThrow({forms_error,{not_a_beam_file, 'test/bad_content.beam'}},
                   gradualizer:type_check_file("test/bad_content.beam"))}.

beam_without_forms() ->
    {setup,
     fun() ->
             {ok, any} = compile:file("test/should_pass/any.erl",
                                      [{outdir, "test/should_pass/"}])
     end,
     fun(_) -> file:delete("test/should_pass/any.beam") end,
     ?_assertThrow({forms_not_found, "test/should_pass/any.beam"},
                   gradualizer:type_check_file("test/should_pass/any.beam"))}.


setup_app() ->
     {ok, Apps} = application:ensure_all_started(gradualizer),
     Apps.

 cleanup_app(Apps) ->
     [ok = application:stop(App) || App <- Apps],
     ok.
