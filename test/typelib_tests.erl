%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typelib, [verbose]).
-module(typelib_tests).

-include_lib("eunit/include/eunit.hrl").

-include("typelib.hrl").

-define(_am(E, A), ?_assertMatch(E, A)).

reduce(F, Acc, Ty) ->
    typelib:reduce_type(F, Acc, Ty).

reduce_test_() ->
    F1 = fun (E, Acc) -> {E, Acc} end,
    F2 = fun
             ({atom, _, _} = At, Acc) -> {At, [At | Acc]};
             (Ty, Acc) -> {Ty, Acc}
         end,
    ComplexTy = typelib:parse_type("atom1 | atom2 | "
                                   "{complex, integer(), [{atom() | string(), number()}]}"),
    F3 = fun
             ({type, _, tuple, [{atom, _, complex} | _]}, Acc) -> { {type, 0, none, []}, Acc };
             ({atom, _, Name} = Ty, Acc) -> {Ty, [Name | Acc]};
             (Ty, Acc) -> {Ty, Acc}
         end,
    [
     ?_am({{atom, _, _}, ok},           reduce(F1, ok, typelib:parse_type("my_atom"))),
     ?_am({{type, _, _, any}, ok},      reduce(F1, ok, {type, 0, fun_name, any})),
     ?_am({{integer, _, _}, ok},        reduce(F1, ok, typelib:parse_type("5"))),
     ?_am({{char, _, _}, ok},           reduce(F1, ok, typelib:parse_type("$a"))),
     ?_am({{type, _, any}, ok},         reduce(F1, ok, {type, 0, any})),
     ?_am({pos_inf, ok},                reduce(F1, ok, pos_inf)),
     ?_am({neg_inf, ok},                reduce(F1, ok, neg_inf)),
     ?_am({{var, _, _}, ok},            reduce(F1, ok, typelib:parse_type("A"))),
     ?_am({{atom, _, _}, ok},           reduce(F1, ok, typelib:parse_type("A :: {my_atom}"))),
     ?_am({{atom, _, _}, ok},           reduce(F1, ok, typelib:parse_type("{my_atom}"))),
     ?_am({{atom, _, _}, ok},           reduce(F1, ok, typelib:parse_type("remote:t(my_atom)"))),
     ?_am({{atom, _, _}, ok},           reduce(F1, ok, typelib:parse_type("t(my_atom)"))),
     %% TODO: recursive types `{op, _1, _2, _3}' and `{op, _1, _2, _3, _4}' are not tested.
     ?_am({_, [{atom, _, my_atom}]},    reduce(F2, [], typelib:parse_type("A :: {my_atom}"))),
     ?_am({_, [{atom, _, my_atom}]},    reduce(F2, [], typelib:parse_type("my_atom"))),
     ?_am({_, [{atom, _, my_atom},
               {atom, _, t},
               {atom, _, remote}]},     reduce(F2, [], typelib:parse_type("remote:t(my_atom)"))),
     ?_am({_, [{atom, _, my_atom}]},    reduce(F2, [], typelib:parse_type("t(my_atom)"))),
     ?_am({_, [atom2, atom1]},          reduce(F3, [], ComplexTy))
    ].
