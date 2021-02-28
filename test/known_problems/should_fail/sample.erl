% From #318
-module(sample).
-export([test/1]).

-spec test(atom()) -> pid().    
test(X) -> fapply(fun idz/1, X).

-spec idz(Z) -> Z.
idz(Z) -> Z.

-spec fapply(fun((T) -> U), T) -> U.
fapply(F, X) ->
    ZZ = F(X),
    ZZ.
