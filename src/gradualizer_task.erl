%% @doc Task is a helper for running asynchronous operations.
%%
%% This is a port of the Elixir Task module borrowed from https://github.com/redink/task.
%% Original Elixir Task documentation can be found at https://hexdocs.pm/elixir/Task.html.
%% @end
-module(gradualizer_task).

-export([async/3,
         async/4,
         async/1,
         async/2,
         await/1,
         await/2]).

-export([async_opt/4,
         async_opt/5,
         async_opt/2,
         async_opt/3]).

-export([safe_await/2,
         safe_await/3]).

-export([async_do/3]).

-spec async(function()) -> {pid(), reference()}.
async(Fun) when erlang:is_function(Fun) ->
    async(erlang, apply, [Fun, []]).

-spec async(atom(), function()) -> {pid(), reference()}.
async(Node, Fun) when erlang:is_function(Fun) ->
    async(Node, erlang, apply, [Fun, []]).

-spec async(atom(), atom(), [term()]) -> {pid(), reference()}.
async(Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(?MODULE, async_do,
                              [Me, get_info(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async(atom(), atom(), atom(), [term()]) -> {pid(), reference()}.
async(Node, Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(Node, ?MODULE, async_do,
                              [Me, get_info(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async_opt(function(), [term()]) -> {pid(), reference()}.
async_opt(Fun, Opts) when erlang:is_function(Fun) ->
    async_opt(erlang, apply, [Fun, []], Opts).

-spec async_opt(atom(), function(), [term()]) -> {pid(), reference()}.
async_opt(Node, Fun, Opts) when erlang:is_function(Fun) ->
    async_opt(Node, erlang, apply, [Fun, []], Opts).

-spec async_opt(atom(), atom(),
                [term()], [term()]) -> {pid(), reference()}.
async_opt(Mod, Fun, Args, Opts) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_opt(?MODULE, async_do,
                             [Me, get_info(Me), {Mod, Fun, Args}],
                             [link | Opts]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec async_opt(atom(), atom(), atom(),
                [term()], [term()]) -> {pid(), reference()}.
async_opt(Node, Mod, Fun, Args, Opts) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_opt(Node, ?MODULE, async_do,
                             [Me, get_info(Me), {Mod, Fun, Args}],
                             [link | Opts]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec await({pid(), reference()}) -> any() | no_return().
await({Pid, Ref}) ->
    await({Pid, Ref}, 5000).

-spec await({pid(), reference()},
            non_neg_integer()) -> any() | no_return().
await({Pid, Ref}, TimeOut) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, noconnection} ->
            erlang:exit({nodedown, erlang:node(Pid),
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}});
        {'DOWN', Ref, _, _, Reason} ->
            erlang:exit({Reason,
                         {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    after TimeOut ->
              erlang:demonitor(Ref, [flush]),
              erlang:exit({timeout,
                           {?MODULE, await, [{Pid, Ref}, TimeOut]}})
    end.

-spec safe_await({pid(), reference()}, term()) -> any().
safe_await(TaskRef, DefaultResult) ->
    safe_await(TaskRef, DefaultResult, 5000).

-spec safe_await({pid(), reference()},
                 term(), non_neg_integer()) -> any().
safe_await(TaskRef, DefaultResult, TimeOut) ->
    case catch await(TaskRef, TimeOut) of
        {'EXIT', _} ->
            DefaultResult;
        Any ->
            Any
    end.

-spec async_do(pid(), {node(), pid() | atom()},
               {atom(), atom(), [term()]}) -> term().
async_do(TaskOwner, TaskOwnerInfo, MFA) ->
    initial_call(MFA),
    Ref = receive
              {TaskOwner, Ref1} ->
                  Ref1
          end,
    erlang:send(TaskOwner, {Ref, do_apply(TaskOwnerInfo, MFA)}).

get_info(Pid) ->
    Name = case erlang:process_info(Pid, [registered_name]) of
               [{registered_name, []}] ->
                   Pid;
               [{registered_name, RegisteredName}] ->
                   RegisteredName
           end,
    {erlang:node(Pid), Name}.

initial_call(MFA) ->
    erlang:put('$initial_call', get_initial_call(MFA)).

get_initial_call({Mod, Fun, Args}) ->
    {Mod, Fun, erlang:length(Args)}.

do_apply(TaskOwnerInfo, {Mod, Fun, Args} = MFA) ->
    try
        erlang:apply(Mod, Fun, Args)
    catch
        error:Value:Stacktrace ->
            task_exit(TaskOwnerInfo, MFA,
                      {Value, Stacktrace});
        throw:Value:Stacktrace ->
            task_exit(TaskOwnerInfo, MFA,
                      {{nocatch, Value}, Stacktrace});
        exit:Value ->
            task_exit(TaskOwnerInfo, MFA, Value)
    end.

task_exit(_, _, normal) ->
    erlang:exit(normal);
task_exit(_, _, shutdown) ->
    erlang:exit(shutdown);
task_exit(_, _, Reason)
  when erlang:tuple_size(Reason) =:= 2
       andalso
       erlang:element(2, Reason) =:= shutdown ->
    erlang:exit(Reason);
task_exit(TaskOwnerInfo, MFA, Reason) ->
    {Fun, Args} = get_running(MFA),

    error_logger:format("** Task ~p terminating~n" ++
                        "** Started from ~p~n" ++
                        "** When function == ~p~n" ++
                        "**      arguments == ~p~n" ++
                        "** Reason for termination == ~n" ++
                        "** ~p~n",
                        [erlang:self(), get_from(TaskOwnerInfo), Fun, Args, Reason]),
    erlang:exit(Reason).

get_from({Node, PidOrName}) when Node =:= erlang:node() ->
    PidOrName;
get_from(Other) ->
    Other.

get_running({Mod, Fun, Args}) ->
    {erlang:make_fun(Mod, Fun, erlang:length(Args)), Args}.
