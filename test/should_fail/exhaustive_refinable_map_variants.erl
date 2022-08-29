-module(exhaustive_refinable_map_variants).

%% This extends cases from test/should_fail/exhaustive_user_type.erl to variants defined as maps.

-export([map_variants/1]).

-export_type([map_sum_t/0]).

-type map_sum_t() :: #{field_one := integer()}
                   | #{field_two := string()}.

-spec map_variants(map_sum_t()) -> ok.
map_variants(T) ->
    case T of
        #{field_one := _} -> ok
    end.
