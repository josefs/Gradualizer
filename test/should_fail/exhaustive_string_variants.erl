-module(exhaustive_string_variants).

-export([string_variants/1]).

-type string_variant_t() :: {non_string, integer()}
                          | {string, string()}.

-spec string_variants(string_variant_t()) -> ok.
string_variants(T) ->
    case T of
        {non_string, _} -> ok
    end.
