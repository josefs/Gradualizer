-module(user_type_in_pattern_body).

%% Check that the pattern type is normalized when recursively checking a pattern

-compile([export_all, nowarn_export_all]).

-type t()  :: {}.

%% Cons: The element type t() should be normalized to {}, to check that the head
%% pattern {} has type t().
-spec cons([t()]) -> boolean().
cons([{} | _]) -> true;
cons([])        -> false.

%% Map: Key {} and value {} are of type t()
-spec map(#{t() => t()}) -> boolean().
map(#{{} := {}}) -> true;
map(#{})         -> false.

%% record field type normalized in patterns
-record(r, {a :: t()}).
-spec rec(#r{}) -> ok.
rec(#r{a = {}}) -> ok.
