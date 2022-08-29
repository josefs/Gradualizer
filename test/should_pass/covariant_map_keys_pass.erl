-module(covariant_map_keys_pass).

-compile([export_all, nowarn_export_all]).

-spec foo(#{ a | b | c := d }) -> ok.
foo(_) -> ok.

-spec bar(#{ a | c := d }) -> ok.
bar(X) -> foo(X).

