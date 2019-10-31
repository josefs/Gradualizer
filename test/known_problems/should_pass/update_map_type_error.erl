-module(update_map_type_error).

-compile(export_all).

-spec t(list()) -> map().
t(Rows) ->
    lists:foldl(fun({Id, Name, Member}, #{members := []} = List) ->
                        List#{id := Id,
                              name := Name,
                              members := [binary_to_list(Member)]};
                   ({_, _, Member}, #{members := Members} = List) ->
                        List#{members := [binary_to_list(Member) | Members]}
                end, #{id => undefined,
                       name => undefined,
                       members => []}, Rows).
