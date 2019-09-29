-module(proplists).

%% This module contains specs to replace incorrect or inexact specs in OTP.

%% Proplists: Changing term() to any() in the most frequently used functions.
-override_module(proplists).
-spec delete(any(), list()) -> list().
-spec get_all_values(any(), list()) -> list().
-spec get_value(any(), list()) -> any().
-spec get_value(any(), list(), any()) -> any().
-spec get_bool(any(), list()) -> boolean().
-spec get_keys(list()) -> list().
-spec lookup(any(), list()) -> none | tuple().
-spec lookup_all(any(), list()) -> [tuple()].
-spec append_values(any(), list()) -> list().
-spec substitute_aliases([{any(), any()}], list()) -> list().
