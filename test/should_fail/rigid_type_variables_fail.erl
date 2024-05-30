-module(rigid_type_variables_fail).

-compile([export_all, nowarn_export_all]).

-spec bad_id(A) -> A.
bad_id(X) -> {X, X}.

-spec bad_id2(A) -> A.
bad_id2(_X) -> banana.

-spec bad_make_tuple(A, B) -> {A, B}.
bad_make_tuple(X, Y) -> {X, {X, Y}}.

-spec bad_make_tuple2(A, B) -> {A, B}.
bad_make_tuple2(X, _Y) -> {X, X}.

-spec forbidden_match(A) -> {A, boolean()}.
forbidden_match(abc = X) -> {X, true};
forbidden_match(X) -> {X, false}.

-spec forbidden_addition(A) -> A.
forbidden_addition(X) ->
    X + 1,
    X.

-spec bad_compose(fun((A) -> B), fun((B) -> C), A) -> C.
bad_compose(F, G, X) -> F(G(X)).

-spec bad_curry(fun(({A, B}) -> C)) -> fun((A, B) -> C).
bad_curry(F) -> fun (X, _Y) -> F({X, X}) end.

-spec bad_head([X, ...]) -> X.
bad_head([_ | T]) -> T.

-spec bad_prepend([A], A) -> [A].
bad_prepend(Xs, _) -> [Xs | Xs].

-spec bad_filter([A], fun((A) -> boolean())) -> [A].
bad_filter([], _F) -> [];
bad_filter([X | Xs], F) ->
    case F(X) of
        true -> [F | bad_filter(Xs, F)];
        false -> bad_filter(Xs, F)
    end.

-spec bad_map([A], fun((A) -> B)) -> [B].
bad_map([], _F) -> [];
bad_map([X | Xs], F) -> [F([X]) | bad_map(Xs, F)].

% gets rewritten to (integer()) -> integer()
-spec rewrite_bound(atom()) -> Int when Int :: integer().
rewrite_bound(Atom) -> Atom.

-spec bad_id_with_bound(A) -> R when R :: A.
bad_id_with_bound(_X) -> 42.

-type tagged(Value) :: {tag, Value}.
% fails due to {inner_tag, _} not being a subtype of map()
-spec bad_add_tag(Value) -> tagged(Value) when Value :: map().
bad_add_tag(Value) ->
    {tag, {inner_tag, Value}}.

%% All four these poly_2 examples come from the paper
%% "Bidirectional Typing for Erlang", N. Rajendrakumar, A. Bieniusa, 2021, Section 2. Examples.
%% These functions are not well typed according to Gradualizer, as we adopt the standard interpretation
%% of type variables (i.e., for-alls are always at the outer-most scope), and we thus do not support
%% rank-2 polymorphism. In all these examples the type variable A is chosen by the caller (not callee),
%% so it can be anything and the poly_2 functions cannot assume anything about it.
-spec poly_2_different(fun((A) -> A)) -> {integer(), boolean()}.
poly_2_different(F) -> {F(42), F(false)}.

-spec poly_2_same(fun((A) -> A)) -> {integer(), integer()}.
poly_2_same(F) -> {F(42), F(84)}.

-spec poly_2_args_different(fun((A) -> A), boolean(), integer()) -> {boolean(), integer()}.
poly_2_args_different(F, B, I) -> {F(I), F(B)}.

-spec poly_2_args_same(fun((A) -> A), boolean(), boolean()) -> {boolean(), boolean()}.
poly_2_args_same(F, B1, B2) -> {F(B2), F(B1)}.
