-module(exhaustive_remote_map_variants).

%% This is the remote type counterpart
%% of test/known_problems/should_fail/exhaustive_map_variants.erl
%%
%% See also test/should_fail/exhaustive_user_type.erl
%% and test/should_fail/exhaustive_remote_user_type.erl

-export([remote_map_variants/1]).

-spec remote_map_variants(exhaustive_user_type:map_sum_t()) -> ok.
remote_map_variants(T) ->
    case T of
        #{field_one := _} -> ok
    end.
