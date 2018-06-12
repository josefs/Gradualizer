-module(type_decl).

-type a_type() :: bool().
-opaque an_opaque_type() :: integer().

-type parameterized(A) :: {A,A}.
