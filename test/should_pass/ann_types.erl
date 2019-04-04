%%% @doc Test module for annotated types
-module(ann_types).

-export([f/2]).

-spec f(A :: atom(), integer()) -> {A :: term(), B :: number()}.
f(A, B) ->
    {A, B}.

-spec g(A :: atom(), Map :: map()) -> NewMap :: map().
g(A, Map) ->
    Map#{ A => any }.
