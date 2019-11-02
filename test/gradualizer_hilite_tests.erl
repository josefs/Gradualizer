-module(gradualizer_hilite_tests).

-include_lib("eunit/include/eunit.hrl").

prettyprint_and_highlight_test() ->
    Expr = {op,1,'andalso',{var,1,'X'},{op,1,'not',{var,1,'X'}}},
    Forms = [{attribute,1,spec,
                         {{f,1},
                          [{type,1,'fun',
                                 [{type,1,product,[{type,1,integer,[]}]},{atom,1,ok}]}]}},
             {function,1,f,1,
                        [{clause,1,
                                 [{var,1,'X'}],
                                 [],
                                 [Expr]}]}],
    Pretty = gradualizer_hilite:prettyprint_and_highlight(Expr, Forms, _Color = false),
    ?assertEqual("-spec f(integer()) -> ok.\n"
                 "f(X) -> X andalso not X.\n"
                 "        ^^^^^^^^^^^^^^^\n",
                 lists:flatten(Pretty)).
