-module(opaque_fail).

-export([use_external/2, update_without_opaque/0, add_to_opaque/0, return_opaque/0]).

-spec use_external(user_types:my_opaque(), integer() | undefined) -> integer().
use_external(I, undefined) -> I;
use_external(_, I) -> I.

-spec update_without_opaque() -> ok.
update_without_opaque() ->
    _Val = user_types:update_opaque(3),
    ok.

-spec add_to_opaque() -> ok.
add_to_opaque() ->
    Val = user_types:new_opaque(),
    Val + 1,
    ok.

-spec return_opaque() -> user_types:my_opaque().
return_opaque() -> 3.
