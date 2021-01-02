-module(list_tail).

-compile([export_all, nowarn_export_all]).

%% Improper list in type inference - currently considered a type error
atom_tail() ->
    [ 1 | list_to_atom("banana")].

