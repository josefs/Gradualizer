-module(erlang).

%% This module contains specs to replace incorrect or inexact specs in OTP.

%% The commented-out specs are the original specs from OTP 21 (erts-10.2.4) with
%% type variebles unfolded if they occur only once in the spec.

%% -spec hd([term(), ...]) -> term().
-spec hd([A, ...]) -> A.

%% -spec min(term(), term()) -> term().
%% -spec max(term(), term()) -> term().
-spec max(A, B) -> A | B.
-spec min(A, B) -> A | B.

%% -spec tl([term(), ...]) -> [term()].
-spec tl([A, ...]) -> [A].

-spec erlang:'--'(list(), list()) -> list().

%% Prior to OTP 24.1 the spec does not list `none' as valid `Args',
%% but the function accepts it and works properly.
-spec erlang:error(Reason, Args) -> no_return() when
      Reason :: term(),
      Args :: [term()] | none.

%% `erlang:error/3' was introduced in OTP 24, but it's spec is also fixed only since OTP 24.1.
-spec erlang:error(Reason, Args, Options) -> no_return() when
      Reason :: term(),
      Args :: [term()] | none,
      Options :: [Option],
      Option :: {'error_info', ErrorInfoMap},
      ErrorInfoMap :: #{'cause' => term(),
                        'module' => module(),
                        'function' => atom()}.
