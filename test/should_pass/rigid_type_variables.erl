-module(rigid_type_variables).

-compile([export_all, nowarn_export_all]).

-spec id(A) -> A.
id(X) -> X.

-spec id_same_var(A) -> A.
id_same_var(A) -> A.

-spec make_tuple(A, B) -> {A, B}.
make_tuple(X, Y) -> {X, Y}.

-spec fst({P, any()}) -> P.
fst({A, _B}) -> A.

-spec compose(fun((A) -> B), fun((B) -> C), A) -> C.
compose(F, G, X) -> G(F(X)).

-spec curry(fun(({A, B}) -> C)) -> fun((A, B) -> C).
curry(F) -> fun (X, Y) -> F({X, Y}) end.

-spec head([X, ...]) -> X.
head([X | _]) -> X.

-spec join([A], [B]) -> [A | B].
join(Xs, Ys) -> Xs ++ Ys.

-spec prepend([A], A) -> [A].
prepend(Xs, Y) -> [Y | Xs].

-spec filter([A], fun((A) -> boolean())) -> [A].
filter([], _F) -> [];
filter([X | Xs], F) ->
    case F(X) of
        true -> [X | filter(Xs, F)];
        false -> filter(Xs, F)
    end.

-spec map([A], fun((A) -> B)) -> [B].
map([], _F) -> [];
map([X | Xs], F) -> [F(X) | map(Xs, F)].

% gets rewritten to (integer()) -> integer()
-spec rewrite_bound(Int) -> Int when Int :: integer().
rewrite_bound(N) -> N + 1.

-type tagged(Value) :: {tag, Value}.
% gets rewritten to (map()) -> {tag, map()}
-spec add_tag(Value) -> tagged(Value) when Value :: map().
add_tag(Value) ->
    {tag, Value}.

% this one passes only due to the lack of support for bounded quantification
% and this behaviour may (or may not) be considered undesirable
-spec add_tag2(Value) -> {tag, Value} when Value :: tuple().
add_tag2(_Value) ->
    {tag, {apple, banana}}.
