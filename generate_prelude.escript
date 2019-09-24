#!/usr/bin/env escript
-mode(compile).
-author('nelson.vides@erlang-solutions.com').

main(_) ->
    try
        Val = create_prelude(),
        io:format("Prelude created: ~p~n", [Val])
    catch C:R:S ->
              io:format("Failed ~p:~p~n~p~n", [C, R, S])
    end.

create_prelude() ->
    PWD = filename:dirname(escript:script_name()),
    Input = PWD ++ "/src/gradualizer_prelude.spec",
    Output = PWD ++ "/src/gradualizer_prelude.erl",
    create_code(Input, Output).

create_code(Input, Output) ->
    {ok, Forms} = parse_input_to_ast(Input),
    GetPreludeImpl = ast_to_string(Forms),
    {ok, Code} = get_final_textfile(GetPreludeImpl),
    file:write_file(Output, Code).

parse_input_to_ast(Input) ->
    EppOpts = [{includes, guess_include_dirs(Input)}],
    epp:parse_file(Input, EppOpts).

ast_to_string(Forms) ->
    erl_parse:anno_to_term(Forms).

guess_include_dirs(File) ->
    Dir = filename:dirname(File),
    case filename:basename(Dir) of
        "src" -> [filename:join(Dir, "../include")];
        _     -> []
    end ++ [code:lib_dir(App, include) || App <- [erts, kernel, stdlib]].

get_final_textfile(GetPreludeImpl) ->
    Code = lists:flatten(
             ["-module(gradualizer_prelude).\n",
              "-export([get_prelude/0]).\n",
              "get_prelude() ->\n",
              lists:flatten(io_lib:format("~p", [GetPreludeImpl])),
              "."
             ]),
    {ok, Code}.
