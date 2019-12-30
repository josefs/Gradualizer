-module(type_vars).

-export([foo/1]).

-spec foo([{integer(), integer()}]) -> [{integer(), integer()}].
foo(Pairs) ->
    pair_sort(Pairs).

-spec pair_sort([A]) -> [A] when A :: {gradualizer:top(), gradualizer:top()}.
pair_sort(Ps) -> Ps.
