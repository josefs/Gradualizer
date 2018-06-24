%%% @doc Module for extracting data from an Erlang parse tree.
-module(absform).

-export([normalize_record_field/1,
         normalize_function_type_list/1,
         function_type_list_to_fun_types/1]).

%% @doc Turns all record fields into typed record fields. Adds default
%% 'undefined' if default value is missing.
normalize_record_field({record_field, L, Name = {atom, _, _}}) ->
    {typed_record_field,
     {record_field, L, Name, {atom, L, undefined}},
     {type, L, any, []}};
normalize_record_field({record_field, L, Name = {atom, _, _}, Default}) ->
    {typed_record_field,
     {record_field, L, Name, Default},
     {type, L, any, []}};
normalize_record_field({typed_record_field,
                        {record_field, L, Name = {atom, _, _}},
                        Type}) ->
    {typed_record_field,
     {record_field, L, Name, {atom, L, undefined}},
     Type};
normalize_record_field({typed_record_field,
                        {record_field, _L, {atom, _, _Name}, _Default},
                        _Type} = Complete) ->
    Complete.

%% @doc Turns all function types into bounded function types. Add default empty
%% constraints if missing.
normalize_function_type_list(FunTypeList) ->
    lists:map(fun normalize_function_type/1, FunTypeList).

normalize_function_type({type, L, 'fun',
                         [{type, _, product, _ArgTypes}, _RetType]} = FunType) ->
    {type, L, bounded_fun, [FunType, _EmptyConst = []]};
normalize_function_type({type, _, 'bounded_fun',
                         [_FunType, _FunConst]} = BoundedFun) ->
    BoundedFun.

%% @doc Convert a function type list (as retrieved from a spec) into a type and
%% constraints. The type of a function as in a spec can be richer (with
%% constraints and multiple clauses) than the type of a fun object in a type
%% declaration. The returned type is a union of fun object types (one for each
%% clause) plus the combination of the constraints separately. This assumes that
%% each type variable is only used in one of the clauses. Useful to preserve
%% rich type info of fun objects like `fun mod:fn/1'.
%% TODO: Warn if same type variable is used in multiple fun spec clauses.
function_type_list_to_fun_types(FunTypeList) ->
    L = element(2, hd(FunTypeList)),
    {FunTypes, Css} =
        lists:unzip(
          [{FunType, constraints:convert(FunConst)}
           || {type, _, bounded_fun, [FunType, FunConst]} <- FunTypeList]),
    {{type, L, union, FunTypes}, constraints:combine(Css)}.
