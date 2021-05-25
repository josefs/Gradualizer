-module(exhaustive_user_type).

-compile(debug_info).

-export([simple/1,
         recursive/1,
         mutually_recursive/1,
         generic/1,
         local_opaque/1,
         generic_with_local_opaque/1]).

-export_type([t/0,
              recursive_t/0,
              opaque_t/0]).

-type info() :: integer().

-type t() :: {true, info()}
           | {false, info()}.

-spec simple(t()) -> ok.
simple(T) ->
    case T of
        {true, _} -> ok
    end.

-type recursive_t() :: ok | {r, recursive_t()}.

-spec recursive(recursive_t()) -> ok.
recursive(T) ->
    case T of
        ok -> ok
    end.

-type mutually_recursive1_t() :: ok | {mr1, mutually_recursive2_t()}.
-type mutually_recursive2_t() :: ok | {mr2, mutually_recursive1_t()}.

-spec mutually_recursive(mutually_recursive1_t()) -> ok.
mutually_recursive(T) ->
    case T of
        ok -> ok
    end.

-type g(T) :: ok | {generic, T}.

-spec generic(g(integer())) -> ok.
generic(T) ->
    case T of
        ok -> ok
    end.

-opaque opaque_t() :: left | right.

-spec local_opaque(opaque_t()) -> ok.
local_opaque(T) ->
    case T of
        left -> ok
    end.

-spec generic_with_local_opaque(g(opaque_t())) -> ok.
generic_with_local_opaque(T) ->
    case T of
        ok -> ok
    end.
