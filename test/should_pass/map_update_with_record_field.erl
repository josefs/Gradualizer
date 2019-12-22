-module(map_update_with_record_field).

-export([mapup3/2]).

%% When record `r' is defined or included and used directly in the map type definition,
%% then `mapup3' passes typechecking.
%-record(r, {}).
%-type m() :: #{rec := #r{}}.

%% However, when the actual remote type and corresponding remote record is used,
%% then `mapup3' fails to typecheck, although it still should.
-type m() :: #{rec := user_types:my_empty_record()}.

-spec mapup3(m(), user_types:my_empty_record()) -> m().
mapup3(TypedMap, R) ->
    TypedMap#{rec => R}.
