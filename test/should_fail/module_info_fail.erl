-module(module_info_fail).

-compile([export_all, nowarn_export_all]).

-spec md5() -> atom().
md5() ->
    erlang:module_info(md5).