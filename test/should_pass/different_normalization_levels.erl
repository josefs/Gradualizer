-module(different_normalization_levels).

-export([f/1]).

-type t() :: a | b | c.

%% The problem is that the argument type stays the same (as top-level  normalization
%% does not expand it) but the result type gets normalized to `a | b | c',
%% and `t() | a' is not a subtype of `a | b | c' because it currently checks whether
%% `t()' is a subtype of one of `a' or `b', or `c' (which it isn't).

%% It surfaced because, for instance,
%%    type() | any()
%% is not a subtype of
%%    type()

-spec f(t() | a) -> t().
f(X) -> X.
