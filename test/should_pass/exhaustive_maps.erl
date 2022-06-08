-module(exhaustive_maps).

-export([map_union_empty_pat_is_first/1,
         map_union_empty_pat_is_last/1]).

-spec map_union_empty_pat_is_first(#{a => atom()} | #{b := boolean()}) -> atom().
map_union_empty_pat_is_first(#{}) -> empty;
map_union_empty_pat_is_first(#{a := A}) -> A;
map_union_empty_pat_is_first(#{b := B}) -> B.

-spec map_union_empty_pat_is_last(#{a => atom()} | #{b := boolean()}) -> atom().
map_union_empty_pat_is_last(#{a := A}) -> A;
map_union_empty_pat_is_last(#{b := B}) -> B;
map_union_empty_pat_is_last(#{}) -> empty.
