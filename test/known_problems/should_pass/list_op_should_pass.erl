-module(list_op_should_pass).

-export([nil_concat_op_elem_gives_elem/0,
         nil_concat_fun_elem_gives_elem/0,
         nonempty1_concat_op_elem_gives_improper/0,
         nonempty1_concat_fun_elem_gives_improper/0,
         nonempty2_concat_op_elem_gives_improper/0,
         nonempty2_concat_fun_elem_gives_improper/0,
         nil_concat_op_nonempty_gives_proper/0,
         nil_concat_fun_nonempty_gives_proper/0,
         nonempty_concat_op_nonempty_gives_proper/0,
         nonempty_concat_fun_nonempty_gives_proper/0]).

-spec nil_concat_op_elem_gives_elem() -> b.
nil_concat_op_elem_gives_elem() ->
    [] ++ b.

-spec nil_concat_fun_elem_gives_elem() -> b.
nil_concat_fun_elem_gives_elem() ->
    erlang:'++'([], b).

-spec nonempty1_concat_op_elem_gives_improper() -> nonempty_improper_list(a, b).
nonempty1_concat_op_elem_gives_improper() ->
    [a] ++ b.

-spec nonempty1_concat_fun_elem_gives_improper() -> nonempty_improper_list(a, b).
nonempty1_concat_fun_elem_gives_improper() ->
    erlang:'++'([a], b).

-spec nonempty2_concat_op_elem_gives_improper() -> nonempty_improper_list(atom(), c).
nonempty2_concat_op_elem_gives_improper() ->
    [a,b] ++ c.

-spec nonempty2_concat_fun_elem_gives_improper() -> nonempty_improper_list(atom(), c).
nonempty2_concat_fun_elem_gives_improper() ->
    erlang:'++'([a,b], c).

%% See list_op_should_fail.
%% -spec improper_concat_op_elem_gives_badarg() -> none().
%% improper_concat_op_elem_gives_badarg() ->
%%     [a|b] ++ c.

-spec nil_concat_op_nonempty_gives_proper() -> [atom()].
nil_concat_op_nonempty_gives_proper() ->
    [] ++ [a].

-spec nil_concat_fun_nonempty_gives_proper() -> [atom()].
nil_concat_fun_nonempty_gives_proper() ->
    erlang:'++'([], [a]).

-spec nonempty_concat_op_nonempty_gives_proper() -> [atom()].
nonempty_concat_op_nonempty_gives_proper() ->
    [a,b] ++ [c].

-spec nonempty_concat_fun_nonempty_gives_proper() -> [atom()].
nonempty_concat_fun_nonempty_gives_proper() ->
    erlang:'++'([a,b], [c]).

%% See list_op_should_fail.
%% -spec improper_concat_op_nonempty_gives_badarg() -> none().
%% improper_concat_op_nonempty_gives_badarg() ->
%%     [a|b] ++ [c].
