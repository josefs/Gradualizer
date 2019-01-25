-module(map_field_valid_update).

-export([f/1]).

-spec f(#{a := boolean()}) -> #{a := integer()}.
f(#{} = Ctx) ->
    Ctx#{a := 5}.
