-module(nonempty_cons).

-compile([export_all, nowarn_export_all]).

-spec t() -> [integer(), ...].
t() ->
    [1].
