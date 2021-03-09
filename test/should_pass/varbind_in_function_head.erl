-module(varbind_in_function_head).

-compile([export_all, nowarn_export_all]).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).

a({Key, Value, undefined} = _Unused) ->
    {Key, Value, 0};
a({Key, Value, Timestamp}) ->
    {Key, Value, 2}.

-record(b, {
    field1 :: atom()
}).
b(#b{field1 = undefined} = _Unused) ->
    1;
b(#b{field1 = _}) ->
    2.
