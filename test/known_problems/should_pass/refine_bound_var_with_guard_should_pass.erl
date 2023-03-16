-module(refine_bound_var_with_guard_should_pass).

-export([f/1]).

%% This type is simpler than gradualizer_type:abstract_type() by having less variants
%% and by using tuples to contain deeper nodes. The latter frees us from having to deal
%% with list (non)emptiness/length in the patterns.
-type simple_type() :: {type, list | nonempty_list, {a}}
                     | {type, atom, {b}}.

-spec f(simple_type()) -> a.
f({type, T, {InnerNode}})
  when T == list orelse T == nonempty_list ->
    %% Currently, this clause fails with:
    %%
    %%   test/known_problems/should_pass/refine_bound_var_with_guard_should_pass.erl:
    %%   The variable on line 14 at column 5 is expected to have type a but it has type b | a
    %%
    %%   f({type, T, {InnerNode}})
    %%     when T == list orelse T == nonempty_list ->
    %%       InnerNode;
    %%       ^^^^^^
    %%
    %% We can refactor it to pass (see g/1), but then we end up with almost identical repeated code.
    %% The end result is more clunky and doesn't feel as lightweight and flexible as normal Erlang.
    InnerNode;
f({type, atom, {_InnerNode}}) ->
    a.

%% Intentionally not exported.
-spec g(simple_type()) -> a.
g({type, list, {InnerNode}}) ->
    InnerNode;
g({type, nonempty_list, {InnerNode}}) ->
    InnerNode;
g({type, atom, {_InnerNode}}) ->
    a.
