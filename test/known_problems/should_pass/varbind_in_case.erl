-module(varbind_in_case).

-compile([export_all, nowarn_export_all]).

-spec tuple_bind({foo, integer()} | {bar, float()}) -> number().
tuple_bind(A) ->
    case A of
        {foo, N} ->
            ok;
        {bar, N} ->
            ok
        end,
    N.
