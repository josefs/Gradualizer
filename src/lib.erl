% This module contains useful helper functions.

-module(lib).

-export([merge_with/3]).

%% merge_with for maps. Similar to merge_with for dicts.
%% Arguably, this function should be in OTP.
merge_with(F, M1, M2) ->
    case maps:size(M1) < maps:size(M2) of
	true ->
	    maps:fold(fun (K, V1, M) ->
			      maps:update_with(K, fun (V2) -> F(K, V1, V2) end, V1, M)
		      end, M2, M1);
	false ->
	    maps:fold(fun (K, V2, M) ->
			      maps:update_with(K, fun (V1) -> F(K, V1, V2) end, V2, M)
		      end, M1, M2)
    end.
