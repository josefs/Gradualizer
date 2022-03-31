-module(exhaustive_list_variants).

-export([list_variant_omitted/1,
         match_on_empty_list/1,
         match_on_non_empty_list/1]).

-type list_variant_t() :: {non_list, integer()}
                        | {list, [integer()]}.

-spec list_variant_omitted(list_variant_t()) -> ok.
list_variant_omitted(T) ->
    case T of
        {non_list, _} -> ok
    end.

-spec match_on_empty_list(list_variant_t()) -> ok.
match_on_empty_list(T) ->
    case T of
        {non_list, _} -> ok;
        {list, []} -> ok
    end.

-spec match_on_non_empty_list(list_variant_t()) -> ok.
match_on_non_empty_list(T) ->
    case T of
        {non_list, _} -> ok;
        {list, [_|_]} -> ok
    end.
