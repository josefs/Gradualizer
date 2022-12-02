-module(list_concat_op_should_pass).

-export([nil_concat_op_elem_gives_elem/0,
         nonempty1_concat_op_elem_gives_improper/0,
         nonempty2_concat_op_elem_gives_improper/0]).

-spec nil_concat_op_elem_gives_elem() -> b.
nil_concat_op_elem_gives_elem() ->
    [] ++ b.

-spec nonempty1_concat_op_elem_gives_improper() -> nonempty_improper_list(a, b).
nonempty1_concat_op_elem_gives_improper() ->
    [a] ++ b.

-spec nonempty2_concat_op_elem_gives_improper() -> nonempty_improper_list(atom(), c).
nonempty2_concat_op_elem_gives_improper() ->
    [a,b] ++ c.

%% See list_op_should_fail.
%% -spec improper_concat_op_elem_gives_badarg() -> none().
%% improper_concat_op_elem_gives_badarg() ->
%%     [a|b] ++ c.

%% See list_op_should_fail.
%% -spec improper_concat_op_nonempty_gives_badarg() -> none().
%% improper_concat_op_nonempty_gives_badarg() ->
%%     [a|b] ++ [c].
