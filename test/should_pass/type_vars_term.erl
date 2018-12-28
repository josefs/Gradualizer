-module(type_vars_term).

-export([f/1]).

%% This fails if we unfold T in id/1 to
%% -spec id(term()) -> term().
-spec f(integer()) -> integer().
f(N) -> id(N).

-spec id(T) -> T when T :: term().
id(X) -> X.
