-module(opaque).

-export([external/2, internal/2]).

-spec external(user_types:my_opaque(), integer() | undefined) -> integer().
external(_, undefined) -> 0;
external(_, I) -> I.

-opaque my_opaque() :: integer().

-spec internal(my_opaque(), integer() | undefined) -> integer().
internal(_, undefined) -> 0;
internal(_, I) -> I.
