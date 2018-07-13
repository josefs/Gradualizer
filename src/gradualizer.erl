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
         get_forms_from_beam/1
        ]).

-type options() :: proplists:proplist().

%% API functions

%% @doc Type check a source or beam file
-spec type_check_file(file:filename()) -> ok | nok.
type_check_file(File) ->
    type_check_file(File, []).

%% @doc Type check a source or beam file
-spec type_check_file(file:filename(), options()) -> ok | nok.
type_check_file(File, Opts) ->
    Forms =
        case filename:extension(File) of
            ".erl" ->
                get_forms_from_erl(File);
            ".beam" ->
                get_forms_from_beam(File);
            Ext ->
                throw({unknown_file_extension, Ext})
        end,
    Opts2 = proplists:expand([{print_file, [{print_file, File}]}], Opts),
    typechecker:type_check_forms(Forms, Opts2).


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
            StopOnFirstError = proplists:get_bool(stop_on_first_error, Opts),
            lists:foldl(
              fun(File, Res) when Res =:= ok;
                                  not StopOnFirstError ->
                      case type_check_file(File, [print_file|Opts]) of
                          ok -> Res;
                          nok -> nok
                      end;
                 (_, Error) ->
                      Error
              end, ok, filelib:wildcard(filename:join(Dir, "*.{erl,beam}")));
        false ->
            throw({dir_not_found, Dir})
    end.

%% Helper functions

get_forms_from_erl(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File});
        {error, Reason} ->
            throw({file_open_error, {Reason, File}})
    end.

get_forms_from_beam(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {_Module, [{abstract_code,no_abstract_code}]}} ->
            throw({forms_not_found, File});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({file_not_found, File});
        {error, beam_lib, {file_error, _, Reason}} ->
            throw({file_open_error, {Reason, File}});
        {error, beam_lib, Reason} ->
            throw({forms_error, Reason})
    end.
