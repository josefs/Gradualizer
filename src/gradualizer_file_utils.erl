%%% @doc Module with utility functions to work with files.

-module(gradualizer_file_utils).

-export([
            get_forms_from_erl/1,
            get_forms_from_beam/1
        ]).

-type abstract_forms() :: [erl_parse:abstract_form() | erl_parse:form_info()].

-type parsed_file_error() :: {file_not_found, file:filename()} |
                             {file_open_error, {file:posix() | badarg | system_limit, file:filename()}}.

-type parsed_file() :: {ok, abstract_forms()} |
                       parsed_file_error().

-export_type([parsed_file_error/0, abstract_forms/0]).

-spec get_forms_from_erl(file:filename()) -> parsed_file().
get_forms_from_erl(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            {ok, Forms};
        {error, enoent} ->
            {file_not_found, File};
        {error, Reason} ->
            {file_open_error, {Reason, File}}
    end.

-spec get_forms_from_beam(file:filename()) -> parsed_file().
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
