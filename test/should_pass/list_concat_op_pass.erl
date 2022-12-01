-module(list_concat_op_pass).

-export([nil_concat_fun_elem_gives_elem/0,
         nonempty1_concat_fun_elem_gives_improper/0,
         nonempty2_concat_fun_elem_gives_improper/0,
         nil_concat_fun_nonempty_gives_proper/0,
         nil_concat_op_nonempty_gives_proper/0,
         nonempty_concat_fun_nonempty_gives_proper/0,
         nonempty_concat_op_nonempty_gives_proper/0,
         proper_concat_fun_proper_gives_proper/0]).

-spec nil_concat_fun_elem_gives_elem() -> b.
nil_concat_fun_elem_gives_elem() ->
    erlang:'++'([], b).

-spec nonempty1_concat_fun_elem_gives_improper() -> nonempty_improper_list(a, b).
nonempty1_concat_fun_elem_gives_improper() ->
    erlang:'++'([a], b).

-spec nonempty2_concat_fun_elem_gives_improper() -> nonempty_improper_list(atom(), c).
nonempty2_concat_fun_elem_gives_improper() ->
    erlang:'++'([a,b], c).

-spec nil_concat_fun_nonempty_gives_proper() -> [atom()].
nil_concat_fun_nonempty_gives_proper() ->
    erlang:'++'([], [a]).

-spec nil_concat_op_nonempty_gives_proper() -> [atom()].
nil_concat_op_nonempty_gives_proper() ->
    [] ++ [a].

-spec nonempty_concat_fun_nonempty_gives_proper() -> [atom()].
nonempty_concat_fun_nonempty_gives_proper() ->
    erlang:'++'([a,b], [c]).

-spec nonempty_concat_op_nonempty_gives_proper() -> [atom()].
nonempty_concat_op_nonempty_gives_proper() ->
    [a,b] ++ [c].

-spec proper_concat_fun_proper_gives_proper() -> [integer()].
proper_concat_fun_proper_gives_proper() ->
    erlang:'++'(generate_list(), generate_list()).

-spec generate_list() -> [integer()].
generate_list() -> lists:seq(1, 2).
