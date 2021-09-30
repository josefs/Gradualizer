%%% @doc Test module for annotated types
-module(ann_types).

-export([f/2, g/2, h/1, annotated_named_record_arg/1, remote_named_record_arg/1]).

-spec f(A :: atom(), integer()) -> {A :: term(), B :: number()}.
f(A, B) ->
    {A, B}.

-spec g(A :: atom(), Map :: map()) -> NewMap :: map().
g(A, Map) ->
    Map#{ A => any }.

-type mytuple() :: {}.
-spec h(Pat :: mytuple()) -> ok.
h({}) ->
    ok.

-record(r, {}).
-type r() :: #r{}.
-spec annotated_named_record_arg(R :: r()) -> {ok, R :: r()}.
annotated_named_record_arg(#r{} = R) -> {ok, R}.

-spec remote_named_record_arg(R :: user_types:my_empty_record()) ->
    {ok, R :: user_types:my_empty_record()}.
remote_named_record_arg(#r{} = R) -> {ok, R}.

