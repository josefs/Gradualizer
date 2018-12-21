-module(refine_ty_vars).

-export([foo/1]).

%% _X is assigned a type variable, e.g. _TyVar-57293823
%% Since type varialbes are not handled in glb, we get
%% glb(T, {a, _TyVar-57293823}) -> none(), thus no refinement occurs.
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
