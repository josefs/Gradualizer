-module(type_decl).

-type a_type() :: bool().
-opaque an_opaque_type() :: int().

-type parameterized(A) :: {A,A}.
