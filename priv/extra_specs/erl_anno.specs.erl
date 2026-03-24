-module(erl_anno).

%% These types are defined as -nominal in OTP 28's erl_anno module,
%% which Gradualizer does not yet support. Providing them here as
%% regular types allows Gradualizer to resolve them.

-export_type([location/0, line/0, column/0]).

-type line() :: non_neg_integer().
-type column() :: pos_integer().
-type location() :: line() | {line(), column()}.
