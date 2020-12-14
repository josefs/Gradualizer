-module(map_field_invalid_update).

-compile(export_all).

-spec f(#{a := integer()}) -> #{a := binary()}.
f(#{} = Ctx) ->
    Ctx#{a := not_a_binary}.

-spec g(#{}) -> #{a := binary()}.
g(#{} = Ctx) ->
    Ctx#{b := 42}.

-spec invalid_key(#{some_key := integer()}) -> #{some_key := binary()}.
invalid_key(#{} = Ctx) ->
    Ctx#{b := 42}.

-spec map_update_with_case(boolean(), map()) -> #{a := 1, b => 1, c => 1}.
map_update_with_case(Bool, Map1) ->
    Map2 = case Bool of
        true ->
            Map1#{a := 1};
        false ->
            Map1#{b := 2}
    end,
    Map2#{c := 1}.