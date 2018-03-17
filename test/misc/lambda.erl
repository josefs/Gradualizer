-module(lambda).

-compile([export_all]).

f() -> (fun (X) -> X + 1 end)(2).
