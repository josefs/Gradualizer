-module(map_pattern).

-compile([export_all, nowarn_export_all]).

-type t() :: #{apa := integer(), bepa := boolean()}.

-spec f(t()) -> boolean().
f(#{bepa := Bepa}) ->
    Bepa.

%% If refinable(_) -> true, then this is not valid.
%-spec key_subtype(#{atom() => integer()}) -> integer().
%% But this is:
-spec key_subtype(#{banana := integer()}) -> integer().
key_subtype(#{banana := N}) ->
    N.
%% Or this is:
%-spec key_subtype(#{banana => integer()}) -> integer().
%key_subtype(#{}) -> DefaultReturnValue;
%key_subtype(#{banana := N}) ->
%    N.
%% Generally speaking, we expect recursive functions on lists to cover the empty list case,
%% so it makes sense to expect users specifying that their functions take a map with an optional
%% field handle the empty map case.

-spec map_union(#{a => atom()} | #{b := boolean()}) -> atom().
map_union(#{a := A}) -> A;
map_union(#{b := B}) -> B;
%% TODO: This is still order dependent due to some reason, so this clause cannot be the first one;
%%       possibly something is wrong with type_diff on maps...
map_union(#{}) -> defaultIfANotPresent.

%-spec any_map(map()) -> ok.
-spec any_map(#{apa := _}) -> ok.
any_map(#{apa := _}) -> ok.

-spec map_term(gradualizer:top()) -> any().
map_term(#{}) ->
    ok.

-spec map_type_var(nonempty_list(#{atom() => integer()} | atom())) -> integer().
map_type_var(L) ->
    V = lists:nth(2, L),
    %% at this point V :: T
    case V of
        %% pattern matching a map against a type var
        #{k := Int} ->
            Int
    end.

