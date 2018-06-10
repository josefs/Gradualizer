%% @doc Functions operating on types on the Erlang Abstract Form
-module(typelib).

-export([remove_pos/1, annotate_user_types/2, get_module_from_annotation/1,
	 substitute_type_vars/2,
         pp_type/1, debug_type/3, parse_type/1]).

-type type() :: erl_parse:abstract_type().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parsing and pretty printing types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec pp_type(type()) -> string().
pp_type(Type) ->
    %% erl_pp can handle type definitions, so wrap Type in a type definition
    %% and then take the type from that.
    Form = {attribute, 0, type, {t, Type, []}},
    TypeDef = erl_pp:form(Form),
    {match, [S]} = re:run(TypeDef, <<"::\\s*(.*)\\.\\n*">>,
			  [{capture, all_but_first, list}, dotall]),
    S.
    %case erl_anno:file(element(2, Type)) of
    %	undefined -> S;
    %	File      -> S ++ " in " ++ File
    %end.

%% Looks up and prints the type M:N(P1, ..., Pn).
debug_type(M, N, P) ->
    case gradualizer_db:get_type(M, N, P) of
	{ok, T} ->
	    Params = lists:join($,, lists:map(fun pp_type/1, P)),
	    io:format("~w:~w(~s) :: ~s.~n",
		      [M, N, Params, pp_type(T)]);
	not_found ->
	    not_found
    end.

-spec parse_type(string()) -> type().
parse_type(Src) ->
    AttrSrc = "-type t() :: " ++ Src ++ ".",
    {ok, Tokens, _EndLocation} = erl_scan:string(AttrSrc),
    {ok, {attribute, _, type, {t, Type, []}}} = erl_parse:parse_form(Tokens),
    Type.

%% Removes all annotations from type, except filename in two cases: Filename is
%% kept for user-defined types and record types. Filename is used to
%% disambiguate between types with the same name from different modules.
-spec remove_pos(type()) -> type().
remove_pos({Type, _, Value}) when Type == atom; Type == integer; Type == var ->
    {Type, erl_anno:new(0), Value};
remove_pos({nil, _}) ->
    {nil, erl_anno:new(0)};
remove_pos({user_type, Anno, Name, Params}) when is_list(Params) ->
    {user_type, anno_keep_only_filename(Anno), Name,
     lists:map(fun remove_pos/1, Params)};
remove_pos({type, Anno, record, Params = [_Name]}) ->
    {type, anno_keep_only_filename(Anno), record, Params};
remove_pos({type, _, Type, Params}) when is_list(Params) ->
    {type, erl_anno:new(0), Type, lists:map(fun remove_pos/1, Params)};
remove_pos({type, _, Type, any}) when Type == tuple; Type == map ->
    {type, erl_anno:new(0), Type, any};
remove_pos({remote_type, _, [Mod, Name, Params]}) ->
    Params1 = lists:map(fun remove_pos/1, Params),
    {remote_type, erl_anno:new(0), [Mod, Name, Params1]};
remove_pos({ann_type, _, [Var, Type]}) ->
    {ann_type, erl_anno:new(0), [Var, remove_pos(Type)]};
remove_pos({op, _, Op, Type}) ->
    {op, erl_anno:new(0), Op, remove_pos(Type)}.

%% Helper for remove_pos/1. Removes all annotations except filename.
-spec anno_keep_only_filename(erl_anno:anno()) -> erl_anno:anno().
anno_keep_only_filename(Anno) ->
    NewAnno = erl_anno:new(0),
    case erl_anno:file(Anno) of
	undefined -> NewAnno;
	Filename  -> erl_anno:set_file(Filename, NewAnno)
    end.

%% Annotate user-defined types and record types with a file name.
-spec annotate_user_types(module() | file:filename(), type()) -> type().
annotate_user_types(Module, Type) when is_atom(Module) ->
    annotate_user_types(atom_to_list(Module) ++ ".erl", Type);
annotate_user_types(Filename, {user_type, Anno, Name, Params}) ->
    %% Annotate local user-defined type
    {user_type, erl_anno:set_file(Filename, Anno), Name,
     [annotate_user_types(Filename, Param) || Param <- Params]};
annotate_user_types(Filename, {type, Anno, record, RecName = [_]}) ->
    %% Annotate local record type
    {type, erl_anno:set_file(Filename, Anno), record, RecName};
annotate_user_types(Filename, {type, Anno, T, Params}) when is_list(Params) ->
    {type, Anno, T, [annotate_user_types(Filename, Param) || Param <- Params]};
annotate_user_types(Filename, {ann_type, Anno, [Var, Type]}) ->
    {ann_type, Anno, [Var, annotate_user_types(Filename, Type)]};
annotate_user_types(_Filename, Type) ->
    Type.

get_module_from_annotation(Anno) ->
    case erl_anno:file(Anno) of
        File when is_list(File) ->
            {ok, list_to_existing_atom(filename:basename(File, ".erl"))};
        undefined ->
            none
    end.

-spec substitute_type_vars(type(),
                           #{atom() => type()}) -> type().
substitute_type_vars({Tag, L, T, Params}, TVars) when Tag == type orelse
						      Tag == user_type,
						      is_list(Params) ->
    {Tag, L, T, [substitute_type_vars(P, TVars) || P <- Params]};
substitute_type_vars({remote_type, L, [M, T, Params]}, TVars) ->
    {remote_type, L, [M, T, [substitute_type_vars(P, TVars) || P <- Params]]};
substitute_type_vars({var, L, Var}, TVars) ->
    case TVars of
        #{Var := Type} -> Type;
        _              -> {var, L, Var}
    end;
substitute_type_vars(Other = {T, _, _}, _) when T == atom; T == integer ->
    Other.
