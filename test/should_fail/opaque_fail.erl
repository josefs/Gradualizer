-module(opaque_fail).

-export([use_external/2]).

-spec use_external(user_types:my_opaque(), integer() | undefined) -> integer().
use_external(I, undefined) -> I;
use_external(_, I) -> I.
