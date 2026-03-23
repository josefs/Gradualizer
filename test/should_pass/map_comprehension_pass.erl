-module(map_comprehension_pass).

-compile([export_all, nowarn_export_all]).

%% Basic map comprehension with list generator
-spec mc_from_list() -> #{integer() => integer()}.
mc_from_list() ->
    #{X => X * X || X <- [1, 2, 3]}.

%% Map comprehension with map generator
-spec mc_swap(#{atom() => integer()}) -> #{integer() => atom()}.
mc_swap(M) ->
    #{V => K || K := V <- M}.

%% Map comprehension with map generator, doubling values
-spec mc_double_values(#{atom() => integer()}) -> #{atom() => integer()}.
mc_double_values(M) ->
    #{K => V * 2 || K := V <- M}.

%% Map comprehension with filter
-spec mc_filter() -> #{integer() => integer()}.
mc_filter() ->
    #{X => X * X || X <- [1, 2, 3, 4, 5], X rem 2 =:= 1}.

%% Map generator in list comprehension
-spec keys_from_map(#{atom() => integer()}) -> [atom()].
keys_from_map(M) ->
    [K || K := _V <- M].

%% Map generator in list comprehension, filtering values
-spec keys_with_large_values(#{atom() => integer()}) -> [atom()].
keys_with_large_values(M) ->
    [K || K := V <- M, V > 10].

%% Map comprehension with binary generator
-spec mc_from_binary() -> #{integer() => integer()}.
mc_from_binary() ->
    #{X => X * 2 || <<X>> <= <<1,2,3>>}.

%% Map generator producing values for binary comprehension
-spec bc_from_map(#{atom() => integer()}) -> binary().
bc_from_map(M) ->
    << <<V>> || _K := V <- M >>.

%% Strict map generator
-spec strict_mc(#{atom() => integer()}) -> #{atom() => integer()}.
strict_mc(M) ->
    #{K => V + 1 || K := V <:- M}.

%% Strict map generator in list comprehension
-spec strict_map_gen_lc(#{atom() => integer()}) -> [integer()].
strict_map_gen_lc(M) ->
    [V || _K := V <:- M].

%% Map comprehension with any() map
-spec mc_any_map() -> map().
mc_any_map() ->
    #{K => V || K := V <- maps:new()}.

%% Mixed list and map generators
-spec mixed_generators(#{atom() => integer()}) -> [{integer(), atom()}].
mixed_generators(M) ->
    [{V, K} || K := V <- M].
