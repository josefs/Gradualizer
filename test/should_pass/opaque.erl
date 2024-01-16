-module(opaque).

-export([external/2, external_update/0, internal/2, use_internal/2, external_return/0]).
-export_type([my_opaque/0]).

-spec external(user_types:my_opaque(), integer() | undefined) -> integer().
external(_, undefined) -> 0;
external(_, I) -> I.

-spec external_update() -> ok.
external_update() ->
    Val = user_types:new_opaque(),
    _Val2 = user_types:update_opaque(Val),
    ok.

-spec external_return() -> user_types:my_opaque().
external_return() -> user_types:new_opaque().

-opaque my_opaque() :: integer().

-spec internal(my_opaque(), integer() | undefined) -> integer().
internal(_, undefined) -> 0;
internal(_, I) -> I.

-spec use_internal(my_opaque(), integer() | undefined) -> integer().
use_internal(I, undefined) -> I;
use_internal(_, I) -> I.
