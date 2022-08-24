-module(int_type_to_range_crash).

-spec f(integer()) -> any().
f(C) ->
    -lists:min([C, 0]).
