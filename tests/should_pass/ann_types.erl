%%% @doc Test module for annotated types
-module(ann_types).

-export([f/2]).

-spec f(A :: atom(), integer()) -> {A :: term(), B :: number()}.
f(A, B) ->
    {A, B}.
