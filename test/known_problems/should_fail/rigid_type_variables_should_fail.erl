-module(rigid_type_variables_should_fail).

-export([bad_curry/1]).

-spec bad_curry(fun(({A, B}) -> C)) -> fun((A, B) -> C).
bad_curry(F) -> fun (X, _Y) -> F({X, X}) end.
