-module(list_tail).

-compile([export_all]).

% Pass
-spec nth1([integer()]) -> integer().
nth1([]) -> 0;
nth1(Xs) -> lists:nth(1, Xs).

% Fail but should pass
-spec nth2([integer()]) -> integer().
nth2(Xs) ->
    case length(Xs) > 0 of
        true -> lists:nth(1, Xs);
        false -> 0
    end.

% Fail but should pass
-spec nth3([integer()]) -> integer().
nth3(Xs) ->
    case Xs of
        [] -> 0;
        _ -> lists:nth(1, Xs)
    end.

% Pass
-spec nth4([integer()]) -> integer().
nth4(Xs) ->
    case Xs of
        [] -> 0;
        NonEmptyXs -> lists:nth(1, NonEmptyXs)
    end.