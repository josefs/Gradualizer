-module(tuple_union_arg_fail).

-export([j/1]).

-spec i(a, b) -> {a, b};
       (d, e) -> {d, e}.
i(V, U) -> {V, U}.

%% This passes, though it shouldn't, because V is inferred to be d | a,
%% and U is inferred to be b | e.
%% If that was the case, then the call to i/2 could sometimes succeed.
%% Not always, though! So it should already be considered an error.
%%
%% However, due to how the union is structured we know that when V=d, then U=b,
%% and that combination is certain to fail. The same holds for V=a, U=e.
-spec j({d, b} | {a, e}) -> {a, b} | {d, e}.
j({V, U}) -> i(V, U).
