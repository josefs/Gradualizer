-module(exhaustive_remote_user_type).

-export([local_alias/1,
         remote/1]).

-type alias_t() :: exhaustive_user_type:t().

-spec local_alias(alias_t()) -> ok.
local_alias(T) ->
    case T of
        {true, _} -> ok
    end.

-spec remote(exhaustive_user_type:t()) -> ok.
remote(T) ->
    case T of
        {true, _} -> ok
    end.
