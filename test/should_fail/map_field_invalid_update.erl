-module(map_field_invalid_update).

-export([f/1]).

-spec f(#{a := integer()}) -> #{a := binary()}.
f(#{} = Ctx) ->
    Ctx#{a := not_a_binary}.

