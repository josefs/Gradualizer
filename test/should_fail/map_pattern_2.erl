-module(map_pattern_2).

-compile([export_all, nowarn_export_all]).

-spec badkey(#{apa => atom()}) -> ok.
badkey(#{bepa := _Bepa}) -> ok.
