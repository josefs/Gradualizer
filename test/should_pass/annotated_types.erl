-module(annotated_types).

-export([f/1, g/0, h/1, i/1, named_record_arg/1]).

-include_lib("gradualizer/include/gradualizer.hrl").

f(Expr) ->
    {call, _, _Name, Args} = Expr,
    Arity = ?assert_type(length(Args), arity()),
    do_stuff_with_arity(Arity).

-spec g() -> non_neg_integer().
g() ->
    receive {age, Age} -> ?annotate_type(Age, non_neg_integer()) end.

-spec h(non_neg_integer()) -> ok.
h(N) ->
    do_stuff_with_arity(?assert_type(N, arity())).

i(X) ->
    do_stuff_with_arity(?annotate_type(X, arity())).

-spec do_stuff_with_arity(arity()) -> ok.
do_stuff_with_arity(_Arity) -> ok.

-record(r, {}).
-type r() :: #r{}.

-spec named_record_arg(R :: r()) -> {ok, R}.
named_record_arg(#r{} = R) -> {ok, R}.
