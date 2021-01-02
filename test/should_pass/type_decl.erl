-module(type_decl).

-export_type([a_type/0, an_opaque_type/0, parameterized/1]).

-type a_type() :: boolean().
-opaque an_opaque_type() :: integer().

-type parameterized(A) :: {A,A}.
