-module(andalso_any).

-compile([export_all, nowarn_export_all]).

-spec f1() -> boolean().
f1() ->
  true andalso g1().

-spec g1() -> any().
g1() -> 3.

-spec f2() -> boolean().
f2() ->
  true andalso g2().

g2() -> 5.

f3() ->
  true andalso g3().

-spec g3() -> any().
g3() -> apa.
