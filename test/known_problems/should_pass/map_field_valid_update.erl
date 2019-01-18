-module(map_field_valid_update).

-export([f/1]).

-spec f(#{a := integer()}) -> #{a := binary()}.
f(#{} = Ctx) ->
    Ctx#{a := <<"binary">>}.
