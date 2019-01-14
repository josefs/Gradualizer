-module(list_op).
-export([append/1, subtract/1, subtract/2]).

-spec append(integer()) -> _ | integer().
append(X) -> [1] ++ X.

-spec subtract([a]) -> {ok, [a]}.
subtract(Xs) -> Xs -- Xs.

-spec subtract([a, ...], [a]) -> [a, ...].
subtract(Xs, Ys) -> Xs -- Ys.
