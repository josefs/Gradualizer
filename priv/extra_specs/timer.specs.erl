-module(timer).

%% The time/0 type is defined as -nominal in OTP 28's timer module,
%% which Gradualizer does not yet support. Providing it here as a
%% regular type allows Gradualizer to resolve it.

-export_type([time/0]).

-type time() :: non_neg_integer().
