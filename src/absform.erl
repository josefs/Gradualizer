%%% @doc Module for extracting data from an Erlang parse tree.
-module(absform).

-export([normalize_record_field/1,
         normalize_function_type_list/1]).

-include("gradualizer.hrl").

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
-spec normalize_function_type_list(FunTypeList) -> FunTypeList when
      FunTypeList :: gradualizer_type:af_function_type_list().
normalize_function_type_list(FunTypeList) ->
    ?assert_type(lists:map(fun normalize_function_type/1, FunTypeList), [B, ...]).

-spec normalize_function_type(BoundedFun | Fun) -> BoundedFun when
      BoundedFun :: gradualizer_type:af_constrained_function_type(),
      Fun :: gradualizer_type:af_fun_type().
normalize_function_type({type, L, 'fun', [{type, _, product, _ArgTypes}, _RetType]} = FunType) ->
    {type, L, bounded_fun, [FunType, _EmptyConst = []]};
normalize_function_type({type, _, 'bounded_fun', [_FunType, _FunConst]} = BoundedFun) ->
    BoundedFun.
