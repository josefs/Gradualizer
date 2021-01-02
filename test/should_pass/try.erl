-module('try').

-compile([export_all, nowarn_export_all, nowarn_unused_vars]).

-spec t() -> 2..4.
t()->
    try A = 1 of
        1 ->
            %% a:b(A), unsafe
            B = 2
    catch _:_ ->
            %% a:b(A), unsafe
            %% a:b(B), unbound
            C = 3
    after
        %% a:b(A), unsafe
        %% a:b(B), unsafe
        %% a:b(C), unsafe
        D = 4
    end.
    %% A. unsafe
    %% B. unsafe
    %% C. unsafe
    %% D. unsafe

r()->
    try A = 1 of
        1 ->
            %% a:b(A), unsafe
            B = 2
    catch _:_ ->
            %% a:b(A), unsafe
            %% a:b(B), unbound
            C = 3
    after
        %% a:b(A), unsafe
        %% a:b(B), unsafe
        %% a:b(C), unsafe
        D = 4
    end.
    %% A. unsafe
    %% B. unsafe
    %% C. unsafe
    %% D. unsafe

s() ->
    try ok of
        ok ->
            ok
    catch _:_ ->
            nok
    end.
