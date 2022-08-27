-module(list_op_should_fail).

%% See also list_op_should_pass.

-export([improper_concat_op_elem_gives_badarg/0,
         improper_concat_fun_elem_gives_badarg/0,
         improper_concat_op_nonempty_gives_badarg/0,
         improper_concat_fun_nonempty_gives_badarg/0]).

-spec improper_concat_op_elem_gives_badarg() -> list().
improper_concat_op_elem_gives_badarg() ->
    [a|b] ++ c.

-spec improper_concat_fun_elem_gives_badarg() -> list().
improper_concat_fun_elem_gives_badarg() ->
    erlang:'++'([a|b], c).

-spec improper_concat_op_nonempty_gives_badarg() -> list().
improper_concat_op_nonempty_gives_badarg() ->
    [a|b] ++ [c].

-spec improper_concat_fun_nonempty_gives_badarg() -> list().
improper_concat_fun_nonempty_gives_badarg() ->
    erlang:'++'([a|b], [c]).
