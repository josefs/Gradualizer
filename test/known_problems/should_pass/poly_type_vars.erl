-module(poly_type_vars).

-export([foo/1]).

-gradualizer([solve_constraints]).

%% When a spec contains bounded type variables (bound quantification),
%% we currently only substitute these type variables with their bounds,
%% effectively making these functions monomorphic.

-spec foo([{integer(), integer()}]) -> [{integer(), integer()}].
foo(Pairs) ->
    pair_sort(Pairs).

-spec pair_sort([A]) -> [A] when A :: {gradualizer:top(), gradualizer:top()}.
pair_sort(Ps) -> Ps.
