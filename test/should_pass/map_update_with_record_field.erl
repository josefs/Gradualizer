-module(map_update_with_record_field).

-compile(export_all).

%% When record `r' is defined or included and used directly in the map type definition,
%% then `mapup3' passes typechecking.
%-record(r, {}).
%-type m() :: #{rec := #r{}}.

%% However, when the actual remote type and corresponding remote record is used,
%% then `mapup3' fails to typecheck, although it still should.
-record(r, {}).
-type my_empty_record() :: #r{}.
-type m() :: #{rec := my_empty_record()}.

-spec mapup3(m(), my_empty_record()) -> m().
mapup3(TypedMap, R) ->
    TypedMap#{rec => R}.

-spec map_field_update(#{a := b, c := d}) -> #{a := d, c := b}.
map_field_update(#{a := b, c := d} = M) ->
    M#{a => d, c => b}.

-spec map_field_update_opt(#{a := b, c => d}) -> #{a := d, c => b}.
map_field_update_opt(#{a := b, c := d} = M) ->
    M#{a := d, c := b}.

-spec map_field_update_opt2(#{a := b, c => d}) -> #{a := d, c => b}.
map_field_update_opt2(#{a := b, c := d} = M) ->
    M#{a := d}.

-spec map_field_update_real_fields(map()) ->
    #{a := integer(), b => binary(), c => boolean(), d := atom()}.
map_field_update_real_fields(#{a := <<"Hello">>, d := 42} = Map) ->
    Map#{a => maps:get(d, Map), d => binary_to_atom(maps:get(a, Map))}.

-spec map_field_update_real_fields_2(map()) ->
    #{a := integer(), b => binary(), c => boolean(), d := atom()}.
    map_field_update_real_fields_2(#{a := <<"Hello">>, d := 42} = Map) ->
    Map#{
        a => maps:get(d, Map),
        b => maps:get(a, Map),
        c => false,
        d => binary_to_atom(maps:get(a, Map))
    }.

-spec map_update_with_case(boolean(), map()) -> #{a => 1, b => 1, c => 1}.
map_update_with_case(Bool, Map1) ->
    Map2 = case Bool of
        true ->
            Map1#{a := 1};
        false ->
            Map1#{b := 2}
    end,
    Map2#{c := 1}.

-spec map_tuple_update_with_case(boolean(), map()) -> #{a => 1, b => 1, c => 1}.
map_tuple_update_with_case(Bool, Map1) ->
    {Map2} = case Bool of
        true ->
            {Map1#{a := 1}};
        false ->
            {Map1#{b := 2}}
    end,
    Map2#{c := 1}.