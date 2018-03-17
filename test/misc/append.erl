-module(append).

-compile(export_all).

% Thanks to Kostis Sagonas for suggesting this example

app1([], Ys) -> Ys;
app1([X|Xs], Ys) -> [X|app1(Xs, Ys)].

-spec app2(list(), list()) -> list().
app2([], Ys) -> Ys;
app2([X|Xs], Ys) -> [X|app2(Xs, Ys)].

-spec app3(list(), integer() | list()) -> integer() | list().
app3([], Ys) -> Ys;
app3([X|Xs], Ys) -> [X|app3(Xs, Ys)].

-spec app4(list(A), list(A)) -> list(A).
app4([], Ys) -> Ys;
app4([X|Xs], Ys) -> [X|app4(Xs, Ys)].

test() ->
    app3([1,2,3], 4).
