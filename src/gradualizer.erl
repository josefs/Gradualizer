%%% @doc Main external API of the Gradualizer
%%%
%%% The functions `type_check(file|module|dir)' accept the following options:
%%% - `stop_on_first_error': if `true' stop type checking at the first error,
%%%   if `false' continue checking all functions in the given file and all files
%%%   in the given directory.
%%% - `print_file': if `true' prefix error printouts with the file name the
%%%   error is from.
-module(gradualizer).

-export([type_check_file/1,
         type_check_file/2,
         type_check_module/1,
         type_check_module/2,
         type_check_dir/1,
         type_check_dir/2,
         type_check_files/1,
         type_check_files/2
        ]).

-export_type([options/0]).

-type options() :: proplists:proplist().

%% API functions

%% @doc Type check a source or beam file
-spec type_check_file(file:filename()) -> ok | nok.
type_check_file(File) ->
    type_check_file(File, []).

%% @doc Type check a source or beam file
-spec type_check_file(file:filename(), options()) -> ok | nok.
type_check_file(File, Opts) ->
    ParsedFile =
        case filename:extension(File) of
            ".erl" ->
                gradualizer_file_utils:get_forms_from_erl(File);
            ".beam" ->
                gradualizer_file_utils:get_forms_from_beam(File);
            Ext ->
                throw({unknown_file_extension, Ext})
        end,
    case ParsedFile of
        {ok, Forms} ->
            Opts2 = proplists:expand([{print_file, [{print_file, File}]}], Opts),
            typechecker:type_check_forms(Forms, Opts2);
        Error ->
            throw(Error)
    end.


%% @doc Type check a module
-spec type_check_module(module()) -> ok | nok.
type_check_module(Module) ->
    type_check_module(Module, []).

%% @doc Type check a module
-spec type_check_module(module(), options()) -> ok | nok.
type_check_module(Module, Opts) when is_atom(Module) ->
    case code:which(Module) of
        File when is_list(File) ->
            type_check_file(File, Opts);
        Error when is_atom(Error) ->
            throw({beam_not_found, Error})
    end.

%% @doc Type check all source or beam files in a directory.
%% (Option `print_file' is implicitely true)
-spec type_check_dir(file:filename()) -> ok | nok.
type_check_dir(Dir) ->
    type_check_dir(Dir, []).

%% @doc Type check all source or beam files in a directory.
%% (Option `print_file' is implicitely true)
-spec type_check_dir(file:filename(), options()) -> ok | nok.
type_check_dir(Dir, Opts) ->
    case filelib:is_dir(Dir) of
        true ->
            type_check_files(filelib:wildcard(filename:join(Dir, "*.{erl,beam}")), Opts);
        false ->
            throw({dir_not_found, Dir})
    end.

%% @doc Type check a source or beam file
%% (Option `print_file' is implicitely true)
-spec type_check_files([file:filename()]) -> ok | nok.
type_check_files(Files) ->
    type_check_files(Files, []).

%% @doc Type check a source or beam
%% (Option `print_file' is implicitely true)
-spec type_check_files([file:filename()], options()) -> ok | nok.
type_check_files(Files, Opts) ->
    StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
    lists:foldl(
        fun(File, Res) when Res =:= ok;
                            not StopOnFirstError ->
                case type_check_file(File, [print_file|Opts]) of
                    ok -> Res;
                    nok -> nok
                end;
            (_, nok) ->
                nok
        end, ok, Files).
