-module(map_type_error).

-compile(export_all).

-spec nomap() -> integer().
nomap() -> #{fruit => banana}.

-spec map_nomap(integer()) -> #{}.
map_nomap(I) -> I.
