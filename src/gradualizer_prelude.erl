-module(gradualizer_prelude).

%% This module contains specs to replace incorrect or inexact specs in OTP.

-spec erlang:hd([A, ...]) -> A.
-spec erlang:tl([A, ...]) -> [A].

%% change return Val to any() from term()
-spec application:get_env(Par) -> 'undefined' | {'ok', Val} when
      Par :: atom(),
      Val :: any().
-spec application:get_env(Application, Par) -> 'undefined' | {'ok', Val} when
      Application :: atom(),
      Par :: atom(),
      Val :: any().
-spec application:get_env(Application, Par, Def) -> Val when
      Application :: atom(),
      Par :: atom(),
      Def :: term(),
      Val :: any().