-module(gradualizer_type_env).

-export([empty/0,
         create/3,

         module/1,
         types/1,
         records/1]).

-export_type([t/0,
              type_map/0,
              record_map/0]).

-type t() :: #{ module := module(),
                types := type_map(),
                records := record_map() }.
%% Type environment, passed around while comparing compatible subtypes.

-type type_map() :: #{{Ty :: atom(), arity()} => {Params :: [atom()],
                                                  Body :: gradualizer_type:abstract_type()}}.

-type record_map() :: #{Rec :: atom() => [typechecker:typed_record_field()]}.

-spec empty() -> t().
empty() ->
    #{module => undefined,
      types => #{},
      records => #{}}.

-spec create(Module, TypeDefs, RecordDefs) -> t() when
      Module :: module(),
      TypeDefs :: [ {gradualizer_type:type_name(), gradualizer_type:abstract_type(), [atom()]} ],
      RecordDefs :: [ {gradualizer_type:record_name(), [ typechecker:typed_record_field() ]} ].
create(Module, TypeDefs, RecordDefs) ->
    TypeMap =
        maps:from_list([begin
                            Id       = {Name, length(Vars)},
                            Params   = [VarName || {var, _, VarName} <- Vars],
                            {Id, {Params, typelib:remove_pos(Body)}}
                        end || {Name, Body, Vars} <- TypeDefs]),
    RecordMap =
        maps:from_list([{Name, [{typed_record_field, Field, typelib:remove_pos(Type)}
                                || {typed_record_field, Field, Type}
                                       <- lists:map(fun absform:normalize_record_field/1,
                                                    Fields)]}
                         || {Name, Fields} <- RecordDefs]),
    #{module => Module,
      types => TypeMap,
      records => RecordMap}.

-spec module(t()) -> module().
module(#{module := Module}) -> Module.

-spec types(t()) -> type_map().
types(#{types := Types}) -> Types.

-spec records(t()) -> record_map().
records(#{records := Records}) -> Records.
