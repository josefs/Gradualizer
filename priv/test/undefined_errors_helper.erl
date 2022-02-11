-module(undefined_errors_helper).
-export([und_rec/0, und_ty/0, not_exp_ty/0]).
-export_type([j/0,
              expands_to_undefined_remote/0,
              expands_to_struct_with_undefined_remote/0]).

-type j() :: undefined_type1().
-type not_exported() :: ok.
-type expands_to_undefined_remote() :: undefined_errors:undefined_type2().
-type expands_to_struct_with_undefined_remote() :: {struct, undefined_errors:undefined_type3()}.

-spec und_rec() -> #undefined_record{}.
und_rec() -> ok.

-spec und_ty() -> undefined_errors:undefined_type4().
und_ty() -> ok.

-spec not_exp_ty() -> not_exported().
not_exp_ty() -> ok.
