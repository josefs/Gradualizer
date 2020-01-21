-module(record_field_union_any).

-export([f/1]).

-record(r, {f :: integer()}).

%% Currently fails with error "The variable is expected to have type #r{} but it
%% has type undefined | any()"
-spec f(any() | undefined) -> integer().
f(R) ->
    R#r.f.
