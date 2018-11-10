-module(lc).

bin_gen_1() ->
    [X || <<X>> <= bin_fixed_size()].

-spec bin_gen_2(<<_:16>>) -> [integer()].
bin_gen_2(Bin) ->
    [X || <<X>> <= Bin].

-spec bin_fixed_size() -> <<_:16>>.
bin_fixed_size() -> <<"xy">>.
