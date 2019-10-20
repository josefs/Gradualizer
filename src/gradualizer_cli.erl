-module(gradualizer_cli).
-export([main/1, handle_args/1]).

-spec main([string()]) -> ok.
main(Args) ->
    case handle_args(Args) of
        help    -> print_usage();
        version -> print_version();
        {error, Message} ->
            io:format(standard_error, "~s~n", [Message]),
            halt(1);
        {ok, Files, Opts} ->
            case gradualizer:type_check_files(Files, Opts) of
                ok -> ok;
                nok -> halt(1)
            end
    end.

-spec handle_args([string()]) -> help | version | {error, string()} |
                                 {ok, [string()], gradualizer:options()}.
handle_args([]) -> help;
handle_args(Args) ->
    try parse_opts(Args, []) of
        {Rest, Opts} ->
            HasHelp = proplists:get_bool(help, Opts),
            HasVersion = proplists:get_bool(version, Opts),
            if
                HasHelp -> help;
                HasVersion -> version;
                Rest =:= [] -> {error, "No files specified to check (try --)"};
                true ->
                    Opts1 = add_default_print_file_to_opts(Rest, Opts),
                    {ok, Rest, Opts1}
            end
    catch
        error:Message when is_list(Message) ->
            {error, Message}
    end.

%% Adds print_file option if there are more than one file to check and
%% the print_file is not already specified.
-spec add_default_print_file_to_opts(FilesToCheck :: list(),
                                     gradualizer:options()) ->
                                            gradualizer:options().
add_default_print_file_to_opts(Files, Opts) ->
    [print_file || not proplists:is_defined(print_file, Opts),
                   length(Files) > 1 orelse filelib:is_dir(hd(Files))] ++ Opts.

-spec get_ver(atom()) -> string().
get_ver(App) ->
    {_, _, Ver} = lists:keyfind(App, 1, application:loaded_applications()),
    Ver.

print_version() ->
    application:load(gradualizer),
    io:format("Gradualizer v~s~n", [get_ver(gradualizer)]).

print_usage() ->
    io:format("Usage: gradualizer [options] [PATH...]~n"),
    io:format("A type checker for Erlang/Elixir~n~n"),
    io:format("       PATH                      Files or directories to type check~n"),
    io:format("       --                        Signals that no more options will follow. The following~n"),
    io:format("                                 arguments are treated as filenames, even if~n"),
    io:format("                                 they start with hyphens.~n"),
    io:format("  -h,  --help                    display this help and exit~n"),
    io:format("       --infer                   Infer type information from literals and other~n"),
    io:format("                                 language constructs~n"),
    io:format("       --no_infer                Only use type information from function specs~n"),
    io:format("                                  - the default behaviour~n"),
    io:format("       --verbose                 Show what Gradualizer is doing~n"),
    io:format("  -pa, --path_add                Add the specified directory to the beginning of~n"),
    io:format("                                 the code path; see erl -pa             [string]~n"),
    io:format("  -I                             Include path for Erlang source files; see -I in~n"),
    io:format("                                 the manual page erlc(1)~n"),
    io:format("       --print_file              prefix error printouts with the file name~n"),
    io:format("                                  - the default when checking a directory or more~n"),
    io:format("                                    than one file~n"),
    io:format("       --print_basename          prefix error printouts with the file basename~n"),
    io:format("       --print_module            prefix error printouts with the module~n"),
    io:format("       --no_print_file           inverse of --print-file~n"),
    io:format("                                  - the default when checking a single file~n"),
    io:format("       --stop_on_first_error     stop type checking at the first error~n"),
    io:format("       --no_stop_on_first_error  inverse of --stop-on-first-error~n"),
    io:format("                                  - the default behaviour~n"),
    io:format("       --no_prelude              Do not override OTP specs.~n"),
    io:format("       --specs_override_dir      Add specs overrides from the *.specs.erl files in~n"),
    io:format("                                 this directory.~n"),
    io:format("       --fmt_location            How to format location when pretty printing errors~n"),
    io:format("                                 (Column is only available if analyzing from source)~n"),
    io:format("                                 - 'none': no location for easier comparison~n"),
    io:format("                                 - 'brief': for machine processing~n"),
    io:format("                                   (\"LINE:COLUMN:\" before message text)~n"),
    io:format("                                 - 'verbose' (default): for human readers~n"),
    io:format("                                   (\"on line LINE at column COLUMN\" within the message text)~n").


-spec parse_opts([string()], gradualizer:options()) -> {[string()], gradualizer:options()}.
parse_opts([], Opts) ->
    {[], Opts};
parse_opts([A | Args], Opts) ->
    case A of
        "-h"                       -> {[], [help]};
        "--help"                   -> {[], [help]};
        "--infer"                  -> parse_opts(Args, [infer | Opts]);
        "--no_infer"               -> parse_opts(Args, [{infer, false} | Opts]);
        "--verbose"                -> parse_opts(Args, [verbose | Opts]);
        "-pa"                      -> handle_path_add(A, Args, Opts);
        "--path_add"               -> handle_path_add(A, Args, Opts);
        "-I"                       -> handle_include_path(A, Args, Opts);
        "--print_file"             -> parse_opts(Args, [print_file | Opts]);
        "--print_module"           -> parse_opts(Args, [{print_file, module} | Opts]);
        "--print_basename"         -> parse_opts(Args, [{print_file, basename} | Opts]);
        "--no_print_file"          -> parse_opts(Args, [{print_file, false} | Opts]);
        "--stop_on_first_error"    -> parse_opts(Args, [stop_on_first_error | Opts]);
        "--no_stop_on_first_error" -> parse_opts(Args, [{stop_on_first_error, false} | Opts]);
        "--crash_on_error"         -> parse_opts(Args, [crash_on_error | Opts]);
        "--no_crash_on_error"      -> parse_opts(Args, [{crash_on_error, false} | Opts]);
        "--version"                -> {[], [version]};
        "--no_prelude"             -> parse_opts(Args, [{prelude, false}| Opts]);
        "--specs_override_dir"     -> handle_specs_override(A, Args, Opts);
        "--fmt_location"           -> handle_fmt_location(Args, Opts);
        "--"                       -> {Args, Opts};
        "-" ++ _                   -> erlang:error(string:join(["Unknown parameter:", A], " "));
        _                          -> {[A | Args], Opts}
    end.

-spec handle_path_add(string(), [string()], gradualizer:options()) -> {[string()], gradualizer:options()}.
handle_path_add(A, Args, Opts) ->
    {Paths, RestArgs} = lists:splitwith(fun no_start_dash/1, Args),
    case Paths of
        [] ->
            erlang:error(string:join(["Missing argument for", A], " "));
        _ ->
            code:add_pathsa(Paths)
    end,
    parse_opts(RestArgs, Opts).

-spec handle_include_path(string(), [string()], gradualizer:options()) -> {[string()], gradualizer:options()}.
handle_include_path(_, [Dir | Args], Opts) ->
    parse_opts(Args, [{i, Dir} | Opts]);
handle_include_path(A, [], _Opts) ->
    erlang:error(string:join(["Missing argument for", A], " ")).

-spec handle_specs_override(string(), [string()], gradualizer:options()) -> {[string()], gradualizer:options()}.
handle_specs_override(_, [Dir | Args], Opts) ->
    parse_opts(Args, [{specs_override, Dir} | Opts]);
handle_specs_override(A, [], _Opts) ->
    erlang:error(string:join(["Missing argument for", A], " ")).

handle_fmt_location([FmtTypeStr | Args], Opts) ->
    try list_to_existing_atom(FmtTypeStr) of
        FmtType when FmtType =:= none;
                     FmtType =:= brief;
                     FmtType =:= verbose ->
            parse_opts(Args, [{fmt_location, FmtType}|Opts]);
        _ ->
            erlang:error(lists:append(["Bad value for fmt-location: ", FmtTypeStr]))
    catch _:_ ->
            erlang:error(lists:append(["Bad value for fmt-location: ", FmtTypeStr]))
    end.

no_start_dash("-" ++ _) ->
    false;
no_start_dash(_) ->
    true.
