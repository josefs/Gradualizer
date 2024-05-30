%% @doc Module for extracting data from an Erlang parse tree.
%% @private
-module(absform).

-export([normalize_record_field/1,
         normalize_function_type_list/1,
         extract_function_from_call/1]).

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

-type bounded_fun() :: gradualizer_type:af_constrained_function_type().

%% @doc Turns all function types into bounded function types. Add default empty
%% constraints if missing.
-spec normalize_function_type_list(gradualizer_type:af_function_type_list()) -> [bounded_fun()].
normalize_function_type_list(FunTypeList) ->
    ?assert_type(lists:map(fun normalize_function_type/1, FunTypeList), nonempty_list()).

-spec normalize_function_type(bounded_fun()) -> bounded_fun();
                             (gradualizer_type:af_fun_type()) -> bounded_fun().
normalize_function_type({type, _, 'bounded_fun', [_FunType, _FunConst]} = BoundedFun) ->
    BoundedFun;
normalize_function_type({type, _, 'bounded_fun', _}) ->
    %% This will never happen if the code in question compiles,
    %% but to avoid a "Nonexhaustive patterns" error we handle it explicitly.
    %% TODO: if the type representation inherited from erl_parse didn't use lists for inner nodes we
    %% wouldn't have to bother with matching empty lists here :(
    erlang:error(unreachable);
normalize_function_type({type, L, 'fun', [{type, _, product, _ArgTypes}, _RetType]} = FunType) ->
    {type, L, bounded_fun, [FunType, _EmptyConst = []]};
normalize_function_type({type, _, 'fun', _}) ->
    %% TODO: same story as for bounded_fun above
    erlang:error(unreachable).

extract_function_from_call({call, Anno, {remote, _, Mod, Fun}, Args}) ->
    {function, Mod, Fun, {integer, Anno, length(Args)}};
extract_function_from_call({call, _Anno, {atom, _, Fun}, Args}) ->
    {function, Fun, length(Args)}.
