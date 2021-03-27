%%% @doc Test cases for `add_type_pat/4'
-module(type_pattern).

-compile([export_all, nowarn_export_all]).

-type ok_tuple() :: {ok}.

-spec g([ok_tuple()]) -> list().
g(List) ->
    [ok || {ok} <- List].
