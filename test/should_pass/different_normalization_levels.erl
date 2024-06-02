-module(different_normalization_levels).

-export([f/1]).

-type t() :: a | b | c.

%% The problem is that the argument type stays the same (as top-level  normalization
%% does not expand it) but the result type gets normalized to `a | b | c'.
%% `t() | a' used not to be a subtype of `a | b | c' because it used to check whether
%% `t()' was a subtype of one of `a' or `b', or `c' (which it wasn't).

%% It surfaced because, for instance,
%%    type() | any()
%% was not a subtype of
%%    type()

-spec f(t() | a) -> t().
f(X) -> X.
