-module(annotated_types).

-export([f/1]).

-include_lib("gradualizer/include/gradualizer.hrl").

f(Expr) ->
    {call, _, _Name, Args} = Expr,
    Arity = ?assert_type(length(Args), arity()),
    do_stuff_with_arity(Arity).

-spec g() -> non_neg_integer().
g() ->
    receive {age, Age} -> ?annotate_type(Age, non_neg_integer()) end.


-spec do_stuff_with_arity(arity()) -> ok.
do_stuff_with_arity(_Arity) -> ok.
