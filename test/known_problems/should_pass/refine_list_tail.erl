-module(refine_list_tail).

-compile([export_all, nowarn_export_all]).

%% Currently fails with:
%% "The variable 'T' has type [] | [any(), ...] but is expected to have type [any(), ...]"
-spec droplast(nonempty_list(T)) -> list(T).
droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].
