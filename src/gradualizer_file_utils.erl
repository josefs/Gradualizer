%% @private
%% @doc Module with utility functions to work with files.

-module(gradualizer_file_utils).

-export([
            get_forms_from_erl/2,
            get_forms_from_beam/1
        ]).

-type abstract_forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].

-type parsed_file_error() :: {file_not_found, file:filename_all()} |
                             {file_open_error, {file:posix() | badarg | system_limit,
                                                file:filename_all()}} |
                             {forms_not_found, file:filename_all()} |
                             {forms_error, Reason :: any()}.

-type parsed_file() :: {ok, abstract_forms()} |
                       parsed_file_error().

-export_type([parsed_file_error/0, abstract_forms/0]).

-spec get_forms_from_erl(file:filename(), IncludePaths :: [file:name()]) ->
    parsed_file() | parsed_file_error().
get_forms_from_erl(File, Includes) ->
    case epp_parse_file(File, Includes) of
        {ok, Forms} ->
            {ok, Forms};
        {error, enoent} ->
            {file_not_found, File};
        {error, Reason} ->
            {file_open_error, {Reason, File}}
    end.

%% @doc Preprocess and parse a file including column numbers in the result
epp_parse_file(File, Includes) ->
    case file:open(File, [read]) of
        {ok, Fd} ->
            try
                StartLocation = {1, 1},
                case epp_open(File, Fd, StartLocation, Includes) of
                    {ok, Epp} ->
                        %% The undocumented `epp:parse_file/1' just
                        %% takes an internal state, and calls the
                        %% documented `epp:parse_erl_forms/1' in a
                        %% loop.
                        Forms = epp:parse_file(Epp),
                        epp:close(Epp),
                        {ok, Forms};
                    Error2 ->
                        Error2
                end
            after
                file:close(Fd)
            end;
        Error1 ->
            Error1
    end.

%% See `priv/extra_specs/epp.specs.erl' for example of handling `erlang:function_exported/3'.
epp_open(File, Fd, StartLocation, Includes) ->
    code:ensure_loaded(epp),
    case erlang:function_exported(epp, open, 5) of
        true ->
            %% epp:open/5 is dropped since OTP-24.
            epp:open(File, Fd, StartLocation, Includes, []);
        false ->
            %% Pre-OTP-24 epp:open/1 doesn't return column numbers.
            epp:open([{name, File},
                      {location, StartLocation},
                      {includes, Includes},
                      {fd, Fd}])
    end.

%% Accepts a filename or the beam code as a binary
-spec get_forms_from_beam(file:filename_all()) -> parsed_file() | parsed_file_error().
get_forms_from_beam(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            {ok, Forms};
        {ok, {_Module, [{abstract_code,no_abstract_code}]}} ->
            {forms_not_found, File};
        {error, beam_lib, {file_error, _, enoent}} ->
            {file_not_found, File};
        {error, beam_lib, {file_error, _, Reason}} ->
            {file_open_error, {Reason, File}};
        {error, beam_lib, Reason} ->
            {forms_error, Reason}
    end.
