-module(map_type_error).

-export([nomap/0]).

-spec nomap() -> integer().
nomap() -> #{fruit => banana}.
