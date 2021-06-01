-module(exhaustive_remote_user_type).

-export([local_alias/1,
         local_alias_to_recursive_type/1,
         remote/1,
         generic_with_remote_opaque/1,
         remote_opaque/1,
         remote_record_variants/1]).

-type alias_t() :: exhaustive_user_type:t().
-type recursive_t() :: exhaustive_user_type:recursive_t().

-spec local_alias(alias_t()) -> ok.
local_alias(T) ->
    case T of
        {true, _} -> ok
    end.

-spec local_alias_to_recursive_type(recursive_t()) -> ok.
local_alias_to_recursive_type(T) ->
    case T of
        ok -> ok
    end.

-spec remote(exhaustive_user_type:t()) -> ok.
remote(T) ->
    case T of
        {true, _} -> ok
    end.

-type g(T) :: ok | {generic, T}.

-spec generic_with_remote_opaque(g(exhaustive_user_type:opaque_t())) -> ok.
generic_with_remote_opaque(T) ->
    case T of
        ok -> ok
    end.

-spec remote_opaque(exhaustive_user_type:opaque_t()) -> ok.
remote_opaque(T) ->
    case T of
        left -> ok
    end.

-include("exhaustive_user_type.hrl").

-spec remote_record_variants(exhaustive_user_type:record_sum_t()) -> ok.
remote_record_variants(T) ->
    case T of
        #variant1{} -> ok
    end.
