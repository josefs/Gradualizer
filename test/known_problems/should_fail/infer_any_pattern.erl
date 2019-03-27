-module(infer_any_pattern).

-export([pat_any/1]).

-gradualizer([infer]).

%% We would expect (at least when infer mode is enabled) that type of
%% I, S and L is integer(), string() and list() respectively but
%% currently they all become any()
%% (which is fine, but Gradualizer could be more precise)
-spec pat_any(any()) -> float().
pat_any({<<I>>}) -> I;
pat_any({S = "string"}) -> S;
pat_any({"prefix" ++ L}) -> L.
