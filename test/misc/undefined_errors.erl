-module(undefined_errors).
-export([local_type/0, remote_type/0,
         local_call/0, remote_call/0,
         local_record/1, remote_record/0,
         record_field/1,
         normalize_remote_type/0,
         not_exported/0]).

-spec local_type() -> undefined_type().
local_type() -> ok.

-spec remote_type() -> undefined_errors_helper:j().
remote_type() -> ok.

-spec local_call() -> ok.
local_call() -> undefined_call().

-spec remote_call() -> ok.
remote_call() -> undefined_errors_helper:undefined_call().

-record(r, {a :: #s{}}).

-spec local_record(#r{}) -> boolean().
local_record(R) -> if
    (R#r.a)#s.a == c -> true;
    true -> false
   end.

-record(defined_record, {a, b, c}).
-spec remote_record() -> #defined_record{}.
remote_record() -> undefined_errors_helper:und_rec().

-spec record_field(#r{}) -> boolean().
record_field(R) -> if
    R#r.b == c -> true;
    true -> false
   end.

-spec normalize_remote_type() -> ok.
normalize_remote_type() -> undefined_errors_helper:und_ty().

-spec not_exported() -> undefined_errors_helper:not_exported_type().
not_exported() -> undefined_errors_helper:not_exp_ty().
