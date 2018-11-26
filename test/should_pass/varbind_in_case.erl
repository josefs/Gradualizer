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
