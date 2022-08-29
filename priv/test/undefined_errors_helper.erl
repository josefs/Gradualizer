-module(undefined_errors_helper).
-export([und_rec/0, und_ty/0, not_exp_ty/0]).
-export_type([j/0,
              expands_to_undefined_remote/0,
              expands_to_struct_with_undefined_remote/0,
              expands_to_struct_with_undefined_local/0,
              expands_to_struct_with_undefined_record/0]).

-type j() :: undefined_type1().
-type not_exported() :: ok.
-type expands_to_undefined_remote() :: undefined_errors:undefined_type2().
-type expands_to_struct_with_undefined_remote() :: {struct, undefined_errors:undefined_type3()}.
-type expands_to_struct_with_undefined_local() :: {struct, undefined_type4()}.
-type expands_to_struct_with_undefined_record() :: {struct, #undefined_record1{}}.

-spec und_rec() -> #undefined_record2{}.
und_rec() -> ok.

-spec und_ty() -> undefined_errors:undefined_type5().
und_ty() -> ok.

-spec not_exp_ty() -> not_exported().
not_exp_ty() -> ok.
