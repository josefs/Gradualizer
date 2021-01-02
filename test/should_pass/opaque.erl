-module(opaque).

-export([external/2, internal/2, use_internal/2]).
-export_type([my_opaque/0]).

-spec external(user_types:my_opaque(), integer() | undefined) -> integer().
external(_, undefined) -> 0;
external(_, I) -> I.

-opaque my_opaque() :: integer().

-spec internal(my_opaque(), integer() | undefined) -> integer().
internal(_, undefined) -> 0;
internal(_, I) -> I.

-spec use_internal(my_opaque(), integer() | undefined) -> integer().
use_internal(I, undefined) -> I;
use_internal(_, I) -> I.
