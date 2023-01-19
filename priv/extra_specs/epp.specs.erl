-module(epp).

%% `epp:open/1' is available at least since OTP-17,
%% so we're not overriding its spec here,
%% as its unlikely Gradualizer is that much backwards compatible.

%% `epp:open/5' was removed in OTP-24:
%% https://github.com/erlang/otp/commit/5281a8c7f77d45a3c36fca9c1a2e4d3812f6fc3d#diff-580a349c49b1d9b5415166e18f5279728d934efe0cebc4ee5a87823055ec3413
-spec open(_, _, _, _, _) -> any().
