-module(undefined_errors_helper).
-export([und_rec/0, und_ty/0]).
-export_type([j/0]).

-type j() :: undefined_type().

-spec und_rec() -> #undefined_record{}.
und_rec() -> ok.

-spec und_ty() -> undefined_errors:undefined_type().
und_ty() -> ok.
