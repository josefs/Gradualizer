-module(underscore_catch_all).

-compile([export_all, nowarn_export_all]).

-spec underscore(_) -> _.
underscore(1) ->
    "apa";
underscore(_) ->
    "bepa".
