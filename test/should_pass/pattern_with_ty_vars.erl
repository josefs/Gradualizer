-module(pattern_with_ty_vars).

-export([foo/1]).

%% _X is assigned a type variable, e.g. _TyVar-57293823
%% Simply check that fun arg type T is matching {a, _TyVar-57293823}
%% and that the typechecker doesn't crash.
-spec foo([{a|b, a|b}]) -> boolean().
foo(Xs) ->
    lists_any(fun ({a, _X}) -> false;
                  ({b, _Y}) -> true
              end,
              Xs).

%% An isolated version of lists:any/2
-spec lists_any(fun((T) -> boolean()), [T]) -> boolean().
lists_any(Pred, [X|Xs]) -> Pred(X) orelse lists_any(Pred, Xs);
lists_any(_Pred, [])    -> false.
