-module(varbind_in_case).

-compile([export_all, nowarn_export_all]).

%% Don't forget variable bindings in case scrutinee.

%% Inference mode
-spec foo() -> ok.
foo() ->
    case Ok = ok of
        ok -> ok
    end,
    Ok.

%% Checking mode
-spec ok(ok) -> ok.
ok(ok) -> ok.

-spec bar() -> ok.
bar() ->
    ok(case Ok = ok of ok -> ok end),
    Ok.

-spec merge_vars(boolean(), 1..2, 2..3) -> 1..3.
merge_vars(A, B, C) ->
    case A of
        true ->  V = B;
        false -> V = C
    end,
    V.

-spec tuple_bind({foo, integer()} | {bar, float()}) -> number().
tuple_bind(A) ->
    case A of
        {foo, N} ->
            ok;
        {bar, N} ->
            ok
        end,
    N.
