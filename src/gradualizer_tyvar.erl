%% @private
%% @doc Centralise type variable name management.
-module(gradualizer_tyvar).

-export([start/0,
         new/3]).

-include("gradualizer.hrl").

-define(a2l(A), atom_to_list(A)).
-define(i2l(I), integer_to_list(I)).

-spec start() -> ok.
start() ->
    Heir = erlang:whereis(gradualizer_sup),
    is_pid(Heir) orelse erlang:error({not_running, gradualizer_sup}),
    Heir = ?assert_type(Heir, pid()),
    ?MODULE = ets:new(?MODULE, [named_table, public, {heir, Heir, no_data}]),
    ok.

%% @doc Generate a new type variable.
%%
%% To avoid generating atoms at runtime a string is returned.
-spec new(atom() | string(), atom(), integer()) -> gradualizer_type:gr_type_var().
new(Name, Mod, Line) ->
    I = ets:update_counter(?MODULE, next_tyvar, 1, {next_tyvar, 0}),
    lists:flatten([ensure_list(Name), "_", ?a2l(Mod), "_", ?i2l(Line), "_", ?i2l(I)]).

-spec ensure_list(atom() | string()) -> string().
ensure_list(Atom) when is_atom(Atom) -> ?a2l(Atom);
ensure_list(String) when is_list(String) -> String.
