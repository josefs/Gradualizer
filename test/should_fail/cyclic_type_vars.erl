-module(cyclic_type_vars).

-compile(export_all).

-spec foo(A) -> B when
    A :: true | B,
    B :: [A].
foo(_) -> [].
