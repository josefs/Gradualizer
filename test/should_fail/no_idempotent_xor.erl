-module(no_idempotent_xor).

-compile(export_all).

-spec bla(true, true) -> _.
bla(X, Y) ->
    Z = X xor Y,    %% We should not infer Z :: true
    zz(Z).

-spec zz(true) -> true.
zz(X) -> X.
