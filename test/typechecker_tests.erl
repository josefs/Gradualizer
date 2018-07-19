%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typechecker, [verbose]).
-module(typechecker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Macro to convert type to abstract form
-define(t(T), typelib:remove_pos(typelib:parse_type(??T))).

subtype_test_() ->
    [
     %% The unknown type, both directions
     ?_assert(subtype(?t( any()             ), ?t( 1..10            ))),
     ?_assert(subtype(?t( 1..10             ), ?t( any()            ))),

     %% Term and none
     ?_assert(subtype(?t( sets:set()        ), ?t( term()           ))),
     ?_assert(subtype(?t( none()            ), ?t( sets:set()       ))),

     %% Integer
     ?_assert(subtype(?t( 1                 ), ?t( 1                ))),
     ?_assert(subtype(?t( 1                 ), ?t( integer()        ))),
     ?_assert(subtype(?t( 1..5              ), ?t( integer()        ))),
     ?_assert(subtype(?t( 1..5              ), ?t( 1..10            ))),
     ?_assert(subtype(?t( 2                 ), ?t( 1..10            ))),
     ?_assert(subtype(?t( pos_integer()     ), ?t( integer()        ))),
     ?_assert(subtype(?t( 5                 ), ?t( pos_integer()    ))),
     ?_assert(subtype(?t( 1..5              ), ?t( pos_integer()    ))),
     ?_assert(subtype(?t( neg_integer()     ), ?t( integer()        ))),
     ?_assert(subtype(?t( -5..-1            ), ?t( neg_integer()    ))),
     ?_assert(subtype(?t( -5                ), ?t( neg_integer()    ))),
     ?_assert(subtype(?t( non_neg_integer() ), ?t( integer()        ))),
     ?_assert(subtype(?t( pos_integer()     ), ?t( non_neg_integer()))),
     ?_assert(subtype(?t( 0..5              ), ?t( non_neg_integer()))),
     ?_assert(subtype(?t( 0                 ), ?t( non_neg_integer()))),

     %% Number
     ?_assert(subtype(?t( 1                 ), ?t( number()         ))),
     ?_assert(subtype(?t( 1..5              ), ?t( number()         ))),
     ?_assert(subtype(?t( integer()         ), ?t( number()         ))),
     ?_assert(subtype(?t( float()           ), ?t( number()         ))),

     %% Atom
     ?_assert(subtype(?t( a                 ), ?t( atom()           ))),
     ?_assert(subtype(?t( a                 ), ?t( a                ))),

     %% Binary, bitstring
     ?_assert(subtype(?t( binary()          ), ?t( bitstring()      ))),
     ?_assert(subtype(?t( <<>>              ), ?t( binary()         ))),
     ?_assert(subtype(?t( <<>>              ), ?t( bitstring()      ))),
     ?_assert(subtype(?t( <<_:4>>           ), ?t( <<_:_*2>>        ))),
     ?_assert(subtype(?t( <<_:6,_:_*4>>     ), ?t( <<_:4,_:_*2>>    ))),

     %% Union
     ?_assert(subtype(?t( b                 ), ?t( a|b              ))),
     ?_assert(subtype(?t( 1|a               ), ?t( integer()|atom() ))),
     ?_assert(subtype(?t( 1..5              ), ?t( 1..3|4..6        ))),
     ?_assert(subtype(?t( 1..5|a            ), ?t( 1..3|4..6|atom() ))),

     %% Lists
     ?_assert(subtype(?t( [a]               ), ?t( list()           ))),
     ?_assert(subtype(?t( list()            ), ?t( [a]              ))),
     ?_assert(subtype(?t( []                ), ?t( list()           ))),
     ?_assert(subtype(?t( []                ), ?t( [a]              ))),
     ?_assert(subtype(?t( [1]               ), ?t( [1..5]           ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( list()           ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( [a]              ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( [a, ...]         ))),
     ?_assert(subtype(?t( [a, ...]          ), ?t( [a]              ))),

     %% Tuples
     ?_assert(subtype(?t( {a,b,c}           ), ?t( tuple()          ))),
     ?_assert(subtype(?t( tuple()           ), ?t( {a,b,c}          ))),
     ?_assert(subtype(?t( {x, 1}            ), ?t( {atom(), 1..5}   ))),

     %% Maps
     ?_assert(subtype(?t( map()             ), ?t( #{a := b}        ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( map()            ))),
     ?_assert(subtype(?t( #{a => b}         ), ?t( #{}              ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( #{a => b}        ))),
     ?_assert(subtype(?t( #{1..5 := a }     ), ?t( #{5 := atom()}   ))),

     %% Type variables
     ?_assert(subtype(?t( A                 ), ?t( integer()        ))),
     ?_assert(subtype(?t( integer()         ), ?t( A                ))),

     %% Annotated types
     ?_assert(subtype(?t( integer()         ), ?t( A :: number()    ))),
     ?_assert(subtype(?t( A :: integer()    ), ?t( number()         ))),
     ?_assert(subtype(?t( A :: integer()    ), ?t( A :: number()    )))
    ].

not_subtype_test_() ->
    [
     %% Term and none
     ?_assert(subtype(?t( integer()         ), ?t( term()           ))),
     ?_assert(subtype(?t( none()            ), ?t( integer()        ))),

     %% Numeric
     ?_assertNot(subtype(?t( 1              ), ?t( 2                ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( 1                ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( 1..5             ))),
     ?_assertNot(subtype(?t( 1..10          ), ?t( 1                ))),
     ?_assertNot(subtype(?t( 1..10          ), ?t( 1..5             ))),
     ?_assertNot(subtype(?t( integer()      ), ?t( float()          ))),
     ?_assertNot(subtype(?t( float()        ), ?t( integer()        ))),
     ?_assertNot(subtype(?t( number()       ), ?t( integer()        ))),
     ?_assertNot(subtype(?t( number()       ), ?t( float()          ))),

     %% Atom
     ?_assertNot(subtype(?t( a              ), ?t( b                ))),

     %% Binary, bitstring
     ?_assertNot(subtype(?t( bitstring()    ), ?t( binary()         ))),
     ?_assertNot(subtype(?t( bitstring()    ), ?t( <<>>             ))),
     ?_assertNot(subtype(?t( <<_:4,_:_*4>>  ), ?t( <<_:6,_:_*2>>    ))),
     ?_assertNot(subtype(?t( <<_:6,_:_*2>>  ), ?t( <<_:4,_:_*4>>    ))),

     %% Union
     ?_assertNot(subtype(?t( a|b            ), ?t( b                ))),

     %% Lists
     ?_assertNot(subtype(?t( []             ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( []             ), ?t( [a, ...]         ))),
     ?_assertNot(subtype(?t( list()         ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( [a]            ), ?t( nonempty_list()  ))),
     ?_assertNot(subtype(?t( [a]            ), ?t( [a, ...]         ))),
     ?_assertNot(subtype(?t( [b]            ), ?t( [a]              ))),

     %% Tuples
     ?_assertNot(subtype(?t( {}             ), ?t( {any()}          ))),
     ?_assertNot(subtype(?t( {1..2, 3..4}   ), ?t( {1, 3}           ))),

     %% Maps
     ?_assertNot(subtype(?t( #{}            ), ?t( #{a := b}        ))),
     ?_assertNot(subtype(?t( #{a => b}      ), ?t( #{a := b}        ))),
     ?_assertNot(subtype(?t( #{a := 1..5}   ), ?t( #{a := 2}        ))),
     ?_assertNot(subtype(?t( #{1 := atom()} ), ?t( #{1 := a}        )))
    ].

normalize_test_() ->
    [
     ?_assertEqual(?t( 1..6 ), typechecker:normalize(?t( 1..3|4..6 ))),
     %% ?t(-8) is parsed as {op,0,'-',{integer,0,8}}
     ?_assertEqual({integer, 0 , -8},
                   typechecker:normalize(?t( (bnot 3) * (( + 7 ) rem ( 5 div - 2 ) ) bxor (1 bsl 6 bsr 4) )))
    ].

normalize_e2e_test_() ->
    [
     {"Normalize local user types",
      %% arg and return type should be both resolved to list(atom())
      ?_assert(type_check_forms(["-type inner(T) :: list(T).",
                                 "-type outer(T) :: inner(T).",
                                 "-spec f(inner(atom())) -> outer(atom()).",
                                 "f(A) -> A."]))},
     {"Normalize integer ops in user types",
      ?_assert(type_check_forms(["-type one() :: -1 | +1.",
                                 "-type int16() :: -1 bsl 15..(1 bsl 15) - 1.",
                                 "-spec f(one(), int16()) -> {one(), int16()}.",
                                 "f(A, B) -> {A, B}."]))},
     {"Normalize tuple of any arity and map with any associations in user types",
      ?_assert(type_check_forms(["-type t() :: tuple() | map().",
                                 "-spec f(t()) -> t().",
                                 "f(A) -> A."]))},
     {"Normalize literals in user types",
      ?_assert(type_check_forms(["-type t() :: foo | 1 | $a.",
                                 "-spec f(t()) -> t().",
                                 "f(A) -> A."]))}
    ].

type_check_call_test_() ->
    [%% Return type of a function call expr must be a subtype of expected result type
     ?_assert(type_check_forms(["-spec f() -> number().",
                                "f() -> g().",
                                "-spec g() -> integer()."])),
     ?_assertNot(type_check_forms(["-spec f() -> integer().",
                                   "f() -> g().",
                                   "-spec g() -> number()."]))
    ].

add_type_pat_test_() ->
    [{"Pattern matching list against any()",
      ?_assert(type_check_forms(["f([E|_]) -> E."]))},
     {"Pattern matching record against any()",
      ?_assert(type_check_forms(["-record(f, {r}).",
                                 "f(#r{f = F}) -> F."]))}
     ].

logic_op_test_() ->
    [
     [?_assert(type_check_forms(["-spec f(boolean(), any()) -> any().",
                                 "f(A1, A2) -> A1 ",Op," A2."])),
      ?_assert(type_check_forms(["-spec f(any(), boolean()) -> any().",
                                 "f(A1, A2) -> A1 ",Op," A2."])),
      ?_assert(type_check_forms(["-spec f(any(), any()) -> boolean().",
                                 "f(A1, A2) -> A1 ",Op," A2."])),

      ?_assertNot(type_check_forms(["-spec f(integer(), any()) -> any().",
                                    "f(A1, A2) -> A1 ",Op," A2."])),
      ?_assertNot(type_check_forms(["-spec f(any(), integer()) -> any().",
                                    "f(A1, A2) -> A1 ",Op," A2."])),
      ?_assertNot(type_check_forms(["-spec f(any(), any()) -> integer().",
                                    "f(A1, A2) -> A1 ",Op," A2."]))
     ]
     || Op <- ["and", "or", "xor"]
    ].

lazy_logic_op_test_() ->
    [?_assert(type_check_forms(["-spec f(boolean(), integer()) -> false | integer().",
                                "f(B1, B2) -> B1 andalso B2."])),
     ?_assert(type_check_forms(["-spec f(boolean(), integer()) -> true | integer().",
                                "f(B1, B2) -> B1 orelse B2."])),
     ?_assertNot(type_check_forms(["-spec f(boolean(), atom()) -> true | integer().",
                                   "f(B1, B2) -> B1 orelse B2."])),
     ?_assertNot(type_check_forms(["-spec f(boolean(), integer()) -> integer().",
                                   "f(B1, B2) -> B1 andalso B2."]))
     %% TODO this should fail because `(B1 + 1)' cannot return boolean()
     %% but type interference (`type_check_expr') does not return exact type yet
     %% ?_assertNot(type_check_forms(["-spec f(any(), any()) -> any().",
     %%                               "f(B1, B2) -> B1 + 1 orelse B2."]))
    ].

handle_type_error_test_() ->
    [
     %% {type_error, nil, Line, Ty}
     ?_assertNot(type_check_forms(["-spec f() -> atom().",
                                   "f() -> []."]))
    ].

subtype(T1, T2) ->
    case typechecker:subtype(T1, T2, {tenv, #{}, #{}}) of
	{true, _} ->
	    true;
	false ->
	    false
    end.

type_check_forms(String) ->
    ok =:= typechecker:type_check_forms(ensure_form_list(merl:quote(String)), []).

ensure_form_list(List) when is_list(List) ->
    List;
ensure_form_list(Other) ->
    [Other].
