-module(intersection_with_unreachable).

-compile([export_all, nowarn_export_all]).

%% Expected error: The clause on line 10 at column 1 cannot be reached.
-spec f(a, b) -> c;
       (e, f) -> g.
f(a, b) -> c;
f(e, f) -> g;
f(x, x) -> y.