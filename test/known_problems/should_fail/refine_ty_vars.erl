-module(refine_ty_vars).

-export([foo/1]).

%% Since _X gets the type any(), no refinement occurs.
-spec foo([{a|b, a|b}]) -> boolean().
foo(Xs) ->
    lists_any(fun ({a, _X}) -> false;
                  ({a, a})  -> true; %% can never match
                  ({b, _})  -> true
              end,
              Xs).

%% An isolated version of lists:any/2
-spec lists_any(fun((T) -> boolean()), [T]) -> boolean().
lists_any(Pred, [X|Xs]) -> Pred(X) orelse lists_any(Pred, Xs);
lists_any(_Pred, [])    -> false.
