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
