-module(any_pattern).

-export([pat_any/1]).

%% This test case just increases coverage of add_any_types_pat
%% with various patterns.
-spec pat_any(any()) -> ok.
pat_any({$a}) -> ok;
pat_any({[]}) -> ok;
pat_any({<<_>>}) -> ok;
pat_any({_ = "string"}) -> ok;
pat_any({"prefix" ++ _}) -> ok.
