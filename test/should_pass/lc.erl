-module(lc).

-compile([export_all, nowarn_export_all]).

bin_gen_1() ->
    [X || <<X>> <= bin_fixed_size()].

-spec bin_gen_2(<<_:16>>) -> [integer()].
bin_gen_2(Bin) ->
    [X || <<X>> <= Bin].

-spec any_list() -> list().
any_list() ->
    [X || X <- [1, 2, 3]].

-spec union_of_lists() -> [integer()] | [atom()].
union_of_lists() ->
    [X || X <- [apa, bepa]].

-spec bin_fixed_size() -> <<_:16>>.
bin_fixed_size() -> <<"xy">>.
