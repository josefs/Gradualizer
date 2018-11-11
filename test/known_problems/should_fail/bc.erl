-module(bc).

-spec f() -> binary().
f() ->
    << X || <<X/integer>> <= <<"abc">> >>.
