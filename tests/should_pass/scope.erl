-module(scope).

-compile([export_all]).

f(X) ->
    case g(X) of
	true -> A = 5,
		B = 7;
	false -> B = 6
    end,
% Fails
%   A.
    B.

g(_X) ->
    true.

% Fails
% h() ->
%     (X = 5) + X.

t() ->
    { X = 5, Y = 4 },
% Fails
%    { X = 5, Y = 4, X },
    X.
