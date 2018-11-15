-module(gradualizer_prelude).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-spec erlang:hd([A, ...]) -> A.
-spec erlang:tl([A, ...]) -> [A].
