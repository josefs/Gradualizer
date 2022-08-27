-module(list_op_should_pass).

-export([nil_concat_elem_gives_elem/0,
         nonempty_concat_elem_gives_improper1/0, nonempty_concat_elem_gives_improper2/0,
         nil_concat_nonempty_gives_proper/0,
         nonempty_concat_nonempty_gives_proper/0]).

-spec nil_concat_elem_gives_elem() -> b.
nil_concat_elem_gives_elem() ->
    [] ++ b.

-spec nonempty_concat_elem_gives_improper1() -> nonempty_improper_list(a, b).
nonempty_concat_elem_gives_improper1() ->
    [a] ++ b.

-spec nonempty_concat_elem_gives_improper2() -> nonempty_improper_list(atom(), c).
nonempty_concat_elem_gives_improper2() ->
    [a,b] ++ c.

%% See list_op_should_fail.
%% -spec improper_concat_elem_gives_badarg() -> none().
%% improper_concat_elem_gives_badarg() ->
%%     [a|b] ++ c.

-spec nil_concat_nonempty_gives_proper() -> [atom()].
nil_concat_nonempty_gives_proper() ->
    [] ++ [a].

-spec nonempty_concat_nonempty_gives_proper() -> [atom()].
nonempty_concat_nonempty_gives_proper() ->
    [a,b] ++ [c].

%% See list_op_should_fail.
%% -spec improper_concat_nonempty_gives_badarg() -> none().
%% improper_concat_nonempty_gives_badarg() ->
%%     [a|b] ++ [c].
