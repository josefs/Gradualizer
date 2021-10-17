-module(exhaustive_list_variants).

-export([list_variants/1]).

-type list_variant_t() :: {non_list, integer()}
                        | {list, [integer()]}.

-spec list_variants(list_variant_t()) -> ok.
list_variants(T) ->
    case T of
        {non_list, _} -> ok
    end.
