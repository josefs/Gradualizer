%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typechecker, [verbose]).
-module(typechecker_tests).

-include_lib("eunit/include/eunit.hrl").

-include("typechecker.hrl").

%% Macro to convert type to abstract form
-define(t(T), t(??T)).
t(T) -> typelib:remove_pos(typelib:parse_type(T)).

subtype_test_() ->
    [
     %% The unknown type, both directions
     ?_assert(subtype(?t( any()             ), ?t( 1..10            ))),
     ?_assert(subtype(?t( 1..10             ), ?t( any()            ))),

     %% remote types need gradualizer_db to be started
     {setup,
      fun setup_app/0,
      fun cleanup_app/1,
      [
       %% Top and none
       ?_assert(subtype(?t( sets:set()        ), ?t( gradualizer:top() ))),
       ?_assert(subtype(?t( none()            ), ?t( sets:set()        )))
      ]},

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

     %% Boolean
     %% We support both the new boolean() and the deprecated bool()
     %% and they're both equivalent.
     ?_assert(subtype(?t( bool()            ), ?t( boolean()        ))),
     ?_assert(subtype(?t( boolean()         ), ?t( bool()           ))),

     %% Atom
     ?_assert(subtype(?t( a                 ), ?t( atom()           ))),
     ?_assert(subtype(?t( a                 ), ?t( a                ))),

     %% Binary, bitstring
     ?_assert(subtype(?t( binary()          ), ?t( bitstring()      ))),
     ?_assert(subtype(?t( <<>>              ), ?t( binary()         ))),
     ?_assert(subtype(?t( <<>>              ), ?t( bitstring()      ))),
     ?_assert(subtype(?t( <<_:4>>           ), ?t( <<_:_*2>>        ))),
     ?_assert(subtype(?t( <<_:6,_:_*4>>     ), ?t( <<_:4,_:_*2>>    ))),
     ?_assert(subtype(?t( <<_:48>>          ), ?t( binary()         ))),

     %% Union
     ?_assert(subtype(?t( b                 ), ?t( a|b              ))),
     ?_assert(subtype(?t( 1|a               ), ?t( integer()|atom() ))),
     ?_assert(subtype(?t( 1..5              ), ?t( 1..3|4..6        ))),
     ?_assert(subtype(?t( 1..5|a            ), ?t( 1..3|4..6|atom() ))),
     ?_assert(subtype(?t( a|b               ), ?t( atom()           ))),
     ?_assert(subtype(?t( (A :: boolean())|(B :: boolean()) ), ?t( boolean() ))),

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
     ?_assert(subtype(?t( []                ), ?t( iolist()         ))),
     ?_assert(subtype(?t( maybe_improper_list(byte(), binary()) ), ?t( iolist() ))),
     ?_assert(subtype(?t( nonempty_list()   ), ?t( iodata()         ))),
     ?_assert(subtype(?t( [byte()]          ), ?t( string()         ))),

     %% Tuples
     ?_assert(subtype(?t( {a,b,c}           ), ?t( tuple()          ))),
     ?_assert(subtype(?t( tuple()           ), ?t( {a,b,c}          ))),
     ?_assert(subtype(?t( {x, 1}            ), ?t( {atom(), 1..5}   ))),

     %% Maps
     ?_assert(subtype(?t( map()             ), ?t( #{a := b}        ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( map()            ))),
     ?_assert(subtype(?t( #{}               ), ?t( #{a => b}        ))),
     ?_assert(subtype(?t( #{a := b}         ), ?t( #{a => b}        ))),
     ?_assert(subtype(?t( #{a => b}         ), ?t( #{a => b, c => d}))),
     ?_assert(subtype(?t( #{a := b, c := d} ), ?t( #{a := b, _ => _}))),
     ?_assert(subtype(?t( #{5 := a }        ), ?t( #{1..5 := atom()}))),
     ?_assert(subtype(?t( #{5 := pid()}     ), ?t( #{_ => pid(), 1..10 => integer()} ))),

     %% Fun objects
     ?_assert(subtype(t("fun((...) -> integer())"), t("fun()"))),
     ?_assert(subtype(t("fun((...) -> integer())"), t("fun((...) -> number())"))),
     ?_assert(subtype(t("fun((atom()) -> integer())"), t("fun((...) -> number())"))),

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
     %% Top and none
     ?_assert(subtype(?t( integer()         ), ?t( gradualizer:top() ))),
     ?_assert(subtype(?t( none()            ), ?t( integer()         ))),

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
     ?_assertNot(subtype(?t( #{}            ), ?t( #{a := b}                            ))),
     ?_assertNot(subtype(?t( #{a => b}      ), ?t( #{a := b}                            ))),
     ?_assertNot(subtype(?t( #{a := 1..5}   ), ?t( #{a := 2}                            ))),
     ?_assertNot(subtype(?t( #{1 := atom()} ), ?t( #{1 := a}                            )))
     %% TODO: We're not capable of handling maps with overlapping keys yet.
     %?_assertNot(subtype(?t( #{5 := pid()}  ), ?t( #{1..10 => integer(), _ => pid()}    )))
    ].

-define(glb(T1, T2, R),
    [ ?_assertEqual(deep_normalize(R), deep_normalize(element(1,glb(T1, T2)))),
      ?_assertEqual(deep_normalize(R), deep_normalize(element(1,glb(T2, T1)))) ]).

glb_test_() ->

    Ts = [ ?t(any()), ?t(none()), ?t(gradualizer:top()), ?t(tuple()), ?t(atom) ],

    [
     %% any()
     [ ?glb( ?t(any()),  T, ?t(any()) )   || T <- Ts, T /= ?t(none()) ],
     [ ?glb( ?t(gradualizer:top()), T, T) || T <- Ts ],
     [ ?glb( ?t(none()), T, ?t(none()) )  || T <- Ts ],

     %% Integer types
     ?glb( ?t(-5..10), ?t(2..3 | 5..15), ?t(2..3 | 5..10) ),
     ?glb( ?t(neg_integer()), ?t(non_neg_integer()), ?t(none()) ),

     %% Atoms
     ?glb( ?t(a | b), ?t(atom()), ?t(a | b) ),
     ?glb( ?t(a),     ?t(atom()), ?t(a) ),
     ?glb( ?t(b),     ?t(a | b),  ?t(b) ),

     %% Lists
     [ ?glb( ?t(maybe_improper_list(gradualizer:top(), gradualizer:top())), T, deep_normalize(T) ) ||
        T <- [ ?t([]), ?t([integer()]), ?t(list()),
               ?t([atom, ...]),
               ?t(maybe_improper_list(tuple(), _)),
               ?t(maybe_improper_list(a | b, tail)),
               ?t(nonempty_improper_list(number(), [] | tail)) ] ],

     ?glb( ?t(maybe_improper_list(a, b)), ?t([a]), ?t(none()) ),
     ?glb( ?t([]), ?t([a, ...]), ?t(none()) ),
     ?glb( ?t([]), ?t(nonempty_improper_list(a, b | [])), ?t(none()) ),

     %% Tuples
     [ ?glb( ?t(tuple()), T, T ) || T <- [?t(tuple()), ?t({}), ?t({integer()}),
                                          ?t({a, {}})] ],
     ?glb( ?t({a, b}), ?t({a, b, _}), ?t(none()) ),
     ?glb( ?t({number(), a | c}), ?t({float() | x, a | b}), ?t({float(), a}) ),

     %% Records
     ?glb( ?t(#r{}), ?t(tuple()), ?t(#r{}) ),
     ?glb( ?t(#r{}), ?t(#s{}),    ?t(none()) ),

     %% Maps
     ?glb( ?t(map()), ?t(#{a := integer()}), ?t(#{a := integer()}) ),
     ?glb( ?t(#{ a := integer() }), ?t(#{ b := float() }), ?t(none()) ),

     ?glb( ?t(#{ a := 1 }), ?t(#{ a := integer() }), ?t(#{ a := 1 }) ),

     ?glb( ?t(#{ a := pos_integer() }), ?t(#{ a := integer() }), ?t(#{ a := pos_integer() }) ),
     ?glb( ?t(#{ a := b }), ?t(#{ a := b }), ?t(#{ a := b }) ),
     ?glb( ?t(#{ integer() => integer() }), ?t(#{ 1..5 => 1..5, foo => bar }), ?t(#{ 1..5 => 1..5 }) ),

     %% Binary types
     ?glb( ?t(binary()),       ?t(<<_:_*32>>),      ?t(<<_:_*32>>) ),
     ?glb( ?t(<<_:4, _:_*8>>), ?t(<<_:12, _:_*8>>), ?t(<<_:12, _:_*8>>) ),
     ?glb( ?t(<<_:4, _:_*8>>), ?t(<<_:7, _:_*8>>),  ?t(none()) ),
     ?glb( ?t(<<_:4, _:_*8>>), ?t(<<_:_*12>>),      ?t(none()) ),   %% glb is really <<_:12, _:_*24>>

     %% Functions
     ?glb( t("fun((integer(), number()) -> a | b)"),
           t("fun((number(),  number()) -> b | c)"),
           t("fun((number(),  number()) -> b)")),

     ?glb( t("fun((integer(), number()) -> a | b)"),
           t("fun((number(),  float())  -> b | c)"),
           ?t(none()) ), %% Should be same as previous

     ?glb( t("fun((...) -> a | b)"),
           t("fun((...) -> b | c)"),
           t("fun((...) -> b)")),

     ?glb( t("fun((...)                -> a | b)"),
           t("fun((number(), number()) -> b | c)"),
           t("fun((number(), number()) -> b)")),

     %% Annotated types
     ?glb( ?t(X :: integer()), ?t(number()), ?t(integer()) ),

     ?_assert(true)
    ].

%% Call to glb never returns
glb_issue_test_() ->
    {setup,
     fun setup_app/0,
     fun cleanup_app/1,
     [?_assertMatch(Result when is_tuple(Result),
                    glb( ?t([erl_parse:abstract_type()]),
                         {type,0,list,[{type,0,union,[{ann_type,0,[{var,891,'Name'},{user_type,[{file,"erl_parse.erl"},{location,0}],af_atom,[]}]},
                                                      {user_type,[{file,"erl_parse.erl"},{location,0}],af_record_field_type,[]}]}]}
                       )
                  )
     ]}.

normalize_test_() ->
    [
     {"Merge intervals",
      ?_assertEqual(?t( 1..6 ),
                    typechecker:normalize(?t( 1..3|4..6 ),
                                          gradualizer_lib:create_tenv(?MODULE, [], [])))},
     {"Remove singleton atoms if atom() is present",
      ?_assertEqual(?t( atom() ),
                    typechecker:normalize(?t( a | atom() | b ),
                                          gradualizer_lib:create_tenv(?MODULE, [], [])))},
     {"Evaluate numeric operators in types",
      %% ?t(-8) is parsed as {op,0,'-',{integer,0,8}}
      ?_assertEqual({integer, 0 , -8},
                    typechecker:normalize(?t( (bnot 3) *
                                              (( + 7 ) rem ( 5 div - 2 ) ) bxor
                                              (1 bsl 6 bsr 4) ),
                                          gradualizer_lib:create_tenv(?MODULE, [], [])))},
     {"normalize(boolean()) == normalize(normalize(boolean))",
      ?_assertEqual(typechecker:normalize(?t( boolean() ),
                                          gradualizer_lib:empty_tenv()),
                    typechecker:normalize(typechecker:normalize(?t( boolean() ),
                                                                gradualizer_lib:empty_tenv()),
                                          gradualizer_lib:empty_tenv()))
     }
    ].

unfold_bounded_type_test() ->
    OrigSpecStr =
        "-spec unzip(List1) -> {List2, List3} when\n"
        "      List1 :: [{A, B}],\n"
        "      List2 :: [A],\n"
        "      List3 :: [B],\n"
        "      A :: term(),\n"
        "      B :: term().",
    ExpectedTypeStr =
        "fun(([{A, B}]) -> {[A], [B]})",

    {attribute, _, spec, {{unzip, 1}, [BoundedFun]}} = merl:quote(OrigSpecStr),
    Env = #env{tenv = gradualizer_lib:create_tenv(?MODULE, [], [])},
    UnfoldedType = typechecker:unfold_bounded_type(Env, BoundedFun),
    UnfoldedTypeStr = typelib:pp_type(UnfoldedType),
    ?assertEqual(ExpectedTypeStr, UnfoldedTypeStr).

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
      ?_assert(type_check_forms(["-type t() :: foo | 1.", %% | $a (char literal)
                                                          %% added in OTP 20
                                 "-spec f(t()) -> t().",
                                 "f(A) -> A."]))},
     {"Normalize fun with any args",
      %% matching [] as arg is needed to enforce checking if
      %% [] is subtype of normalised t()
      ?_assertNot(type_check_forms(["-type t() :: fun((...) -> integer()).",
                                    "-spec f(t()) -> integer().",
                                    "f([]) -> 1."]))}
    ].

propagate_types_test_() ->
    %% Checking type_check_expr, which propagates type information but doesn't
    %% infer types of literals and language constructs.
    [%% the inferred type of a literal tuple should be any()
     ?_assertEqual("any()",
                   type_check_expr(_Env = "",
                                   _Expr = "{1, 2}")),
     %% the inferred type of a tuple with a untyped function call
     %% should also be any()
     ?_assertMatch("any()",
                   type_check_expr(_Env = "h() -> 2.",
                                   _Expr = "{1, h()}")),
     %% the inferred type of a tuple with a typed function call
     %% should be {any(), restype()}
     ?_assertMatch("{any(), integer()}",
                   type_check_expr(_Env = "-spec h() -> integer().",
                                   _Expr = "{1, h()}")),

     %% List. The element type is the union of the element types.
     ?_assertMatch("[1..3, ...]",
                   type_check_expr(_Env = "-spec a() -> 1.\n"
                                          "-spec b() -> 2.\n"
                                          "-spec c() -> 3.",
                                   _Expr = "[a(), b(), c()]")),
     %% Fun.
     ?_assertMatch("any()",
                   type_check_expr(_Env = "",
                                   _Expr = "fun (_A, _B) ->\n"
                                           "    receive C -> C end\n"
                                           "end")),
     ?_assertMatch("fun((any(), any()) -> integer())",
                   type_check_expr(_Env = "-spec i(integer()) -> integer().",
                                   _Expr = "fun (A, B) -> i(B) end")),
     ?_assertMatch("fun((any(), any()) -> integer())",
                   type_check_expr(_Env = "-spec i(integer()) -> integer().",
                                   _Expr = "fun F(A, B) -> i(B) end")),

     %% inferred type of boolean negation
     ?_assertMatch("any()",
                   type_check_expr(_Env = "-spec f() -> any().",
                                   _Expr = "not f()")),
     %% (returns a normalised type, in this case of boolean())
     ?_assertMatch("false | true",
                   type_check_expr(_Env = "-spec f() -> boolean().",
                                   _Expr = "not f()")),
     ?_assertMatch("false",
                   type_check_expr(_Env = "-spec f() -> true.",
                                   _Expr = "not f()")),

     %% infered type of number negation
     ?_assertMatch("any()",
                   type_check_expr(_Env = "-spec f() -> any().",
                                   _Expr = "- f()")),
     ?_assertMatch("float()",
                   type_check_expr(_Env = "-spec f() -> float().",
                                   _Expr = "- f()")),
     ?_assertMatch("integer()",
                   type_check_expr(_Env = "-spec f() -> integer().",
                                   _Expr = "- f()")),
     ?_assertMatch("1",
                   type_check_expr(_Env = "-spec f() -> -1.",
                                   _Expr = "- f()")),
     ?_assertMatch("-3..1",
                   type_check_expr(_Env = "-spec f() -> -1..(1+2).",
                                   _Expr = "- f()")),
     ?_assertMatch("neg_integer() | 0",
                   type_check_expr(_Env = "-spec f() -> non_neg_integer().",
                                   _Expr = "- f()")),
     ?_assertMatch("-7..-5 | -3..-1",
                   type_check_expr(_Env = "-spec f() -> 1..3 | 5..7.",
                                   _Expr = "- f()")),
     ?_assertMatch("-2..-1 | non_neg_integer()",
                   type_check_expr(_Env = "-spec f() -> neg_integer() | 0..2.",
                                   _Expr = "- f()")),

     %% inferred type of record index
     ?_assertMatch("any()",
                   type_check_expr(_Env = "",
                                   _Expr = "#r.f"))
    ].

type_check_in_test_() ->
    %% Test checking an expression against a known type
    %% implemented by the function typechecker:type_check_expr_in.
    [
     ?_assert(type_check_forms(["-spec f(float() | foo) -> ok.",
                                "g() -> f(3.14)."])),
     ?_assertNot(type_check_forms(["-spec f(foo) -> ok.",
                                   "g() -> f(3.14)."])),
     %% Although there is no spec for f/1 - inferred type is `fun((any()) -> any())'
     ?_assert(type_check_forms(["f(_) -> ok.",
                                "-spec g() -> fun((integer()) -> integer()).",
                                "g() -> fun f/1."],
                               [infer])),
     %% Although there is not spec for f/1 - inferred arity does not match
     ?_assertNot(type_check_forms(["-spec g() -> fun(() -> integer()).",
                                   "g() -> fun f/1."],
                                  [infer]))
    ].

infer_types_test_() ->
    %% Checking type_check_expr with inference enabled
    [?_assertEqual("{1, [101 | 104 | 108 | 111, ...], [], banana, float(), $c}",
                   type_check_expr(_Env = "",
                                   _Expr = "{1, \"hello\", \"\", banana, 3.14, $c}",
                                   [infer])),
     %% the inferred type of a tuple with a untyped function call
     %% should be any()
     ?_assertMatch("{1, any()}",
                   type_check_expr(_Env = "h() -> 2.",
                                   _Expr = "{1, h()}",
                                   [infer])),
     %% the inferred type of a tuple with a typed function call
     ?_assertMatch("{1, integer()}",
                   type_check_expr(_Env = "-spec h() -> integer().",
                                   _Expr = "{1, h()}",
                                   [infer])),
     %% catch a type error that isn't caught without this inference.
     ?_assertNot(type_check_forms(["f() -> V = [1, 2], g(V).",
                                   "-spec g(integer()) -> any().",
                                   "g(Int) -> Int + 1."],
                                  [infer])),
     %% infer exact type of bitstrings
     ?_assertMatch("<<_:7, _:_*16>>",
                   type_check_expr(_Env = "f() -> receive X -> X end.",
                                   _Expr = "<<(f())/utf16, 7:7>>",
                                   [infer])),
     %% infer exact type of strings
     ?_assertMatch("[$0..$c, ...]",
                   type_check_expr(_Env = "",
                                   _Expr = "\"0123456789abc\"",
                                   [infer])),
     %% infer exact type of record index
     ?_assertMatch("2",
                   type_check_expr(_Env = "-record(r, {f}).",
                                   _Expr = "#r.f",
                                   [infer]))
    ].

type_check_call_test_() ->
    [%% Return type of a function call expr must be a subtype of expected result type
     ?_assert(type_check_forms(["-spec f() -> number().",
                                "f() -> g().",
                                "-spec g() -> integer()."])),
     ?_assertNot(type_check_forms(["-spec f() -> integer().",
                                   "f() -> g().",
                                   "-spec g() -> number()."])),
     %% Passing an incompatible type to a spec'ed function
     ?_assertNot(type_check_forms(["-spec int_top() -> gradualizer:top().",
                                   "int_top() -> 5.",
                                   "-spec int_arg(integer()) -> integer().",
                                   "int_arg(I) -> I + 1.",
                                   "f() -> int_arg(int_top())."]))
    ].

type_check_fun_test_() ->
    [%% Anonimous fun - union fun type
     ?_assert(type_check_forms(["-spec f() -> fun() | fun().",
                                "f() -> fun(A) -> A end."])),
     %% Anonimous fun - union + any fun type
     ?_assert(type_check_forms(["-spec f(fun() | fun()) -> fun().",
                                "f(F) -> F = fun(A) -> A end."])),
     %% Named fun - union fun type
     ?_assert(type_check_forms(["-spec f() -> fun() | fun().",
                                "f() -> fun NF(A) -> A end."])),
     %% Named fun - union + any fun type
     ?_assert(type_check_forms(["-spec f(fun() | fun()) -> fun().",
                                "f(F) -> F = fun NF(A) -> A end."])),
     ?_assert(type_check_forms(["-spec f(fun() | fun()) -> any().",
                                "f(F) -> F()."])),
     %% Fun application with wrong number of arguments
     ?_assertNot(type_check_forms(["-spec f(fun((integer()) -> integer())) -> any().",
                                   "f(F) -> F()."])),
     %% Calling multiclause fun
     ?_assert(type_check_forms(["-spec g(integer()) -> integer();",
                                "       (atom()) -> atom().",
                                "-spec f() -> {integer(), atom()}.",
                                "f() -> {g(1), g(a)}."])),
     %% Applying fun with any args
     ?_assert(type_check_forms(["-spec f(fun((...) -> integer())) -> integer().",
                                "f(F) -> F(1, 2)."]))
    ].

type_check_clause_test_() ->
    [%% Basic `if' test, X can be any term as guards don't have to return boolean().
     ?_assert(type_check_forms(["-spec f(gradualizer:top()) -> boolean().",
                                "f(X) ->",
                                "    if X -> false;",
                                "       false -> true",
                                "    end."])),
     %% Each clause must return a subtype of ResType (atom())
     ?_assertNot(type_check_forms(["-spec f(gradualizer:top()) -> atom().",
                                   "f(X) ->",
                                   "    if X -> 1;",
                                   "       false -> a",
                                   "    end."])),

    %% The try block has to return ResTy atom()
     ?_assertNot(type_check_forms(["-spec f(gradualizer:top()) -> atom().",
                                   "f(X) ->",
                                   "    try 1",
                                   "    catch _ -> error",
                                   "    end."])),
     %% The try clause has to return ResTy atom(),
     %% but it returns the return type of g() via pattern matching
     ?_assertNot(type_check_forms(["-spec f(gradualizer:top()) -> atom().",
                                   "f(X) ->",
                                   "    try g() of G -> G",
                                   "    catch _ -> error",
                                   "    end.",
                                   "-spec g() -> float()."])),
     %% The catch clause has to return ResTy integer()
     ?_assertNot(type_check_forms(["-spec f(gradualizer:top()) -> integer().",
                                   "f(X) ->",
                                   "    try 1",
                                   "    catch _ -> error",
                                   "    end."])),

     %% erlang:throw/1 needs gradualizer_db to be started
     {setup,
      fun setup_app/0,
      fun cleanup_app/1,
      [
       %% The return type of the after clause is ignored
       ?_assert(type_check_forms(["-spec f(gradualizer:top()) -> integer().",
                                  "f(X) ->",
                                  "    try throw(error)",
                                  "    after error",
                                  "    end."]))
      ]},

     %% Correct try without an after block
     ?_assert(type_check_forms(["-spec f(gradualizer:top()) -> atom().",
                                "f(X) ->",
                                "    try ok",
                                "    catch _ -> error",
                                "    end."])),

     %% should emmit: "The clause on line 3 is expected to have 1 argument(s) but it has 0
     ?_assertNot(type_check_forms(["-spec h() -> fun((integer()) -> atom()).",
                                   "h() ->",
                                   "    fun() -> ok end."]))
    ].

add_type_pat_test_() ->
    [{"Pattern matching list against any()",
      ?_assert(type_check_forms(["f([E|_]) -> E."]))},
     {"Pattern matching record against any()",
      ?_assert(type_check_forms(["-record(r, {f}).",
                                 "f(#r{f = F}) -> F."]))}
    ].

%% it is the responsibility of the compiler to catch semantically
%% invalid programs (such as one with undefined records)
%% but to improve code coverage we test them quickly.
%% Gradualizer should not crash but return error nicely
illegal_forms_test_() ->
    [%% undefined records
     ?_assertNot(type_check_forms(["-spec f() -> gradualizer:top().",
                                   "f() -> #r{f = 1}."])),
     ?_assertNot(type_check_forms(["-record(r, {f1}).",
                                   "-spec f() -> gradualizer:top().",
                                   "f() -> #r{f2 = 1}."])),
     %% invalid record_info
     ?_assertNot(type_check_forms(["f() -> record_info(foo, bar)."])),
     %% illegal pattern
     ?_assertNot(type_check_forms(["-spec f(gradualizer:top()) -> gradualizer:top().",
                                   "f(1 + A) -> ok."]))
    ].

%%
%% Helper functions
%%

setup_app() ->
    {ok, Apps} = application:ensure_all_started(gradualizer),
    Apps.

cleanup_app(Apps) ->
    [ok = application:stop(App) || App <- Apps],
    ok.

subtype(T1, T2) ->
    case typechecker:subtype(T1, T2, gradualizer_lib:create_tenv(?MODULE, [], [])) of
        {true, _} ->
            true;
        false ->
            false
    end.

glb(T1, T2) ->
    glb(T1, T2, #env{tenv = gradualizer_lib:create_tenv(?MODULE, [], [])}).

glb(T1, T2, Env) ->
    typechecker:glb(T1, T2, Env).

deep_normalize(T) ->
    deep_normalize(T, gradualizer_lib:create_tenv(?MODULE, [], [])).

deep_normalize(T, TEnv) ->
    case typechecker:normalize(T, TEnv) of
        {type, P, N, Args} when is_list(Args) ->
            {type, P, N, [ deep_normalize(A, TEnv) || A <- Args ]};
        TN -> TN
    end.

type_check_forms(String) ->
    type_check_forms(String, []).

type_check_forms(String, Opts) ->
    [] =:= typechecker:type_check_forms(ensure_form_list(merl:quote(String)),
                                        Opts).

ensure_form_list(List) when is_list(List) ->
    List;
ensure_form_list(Other) ->
    [Other].

type_check_expr(EnvStr, ExprString) ->
    type_check_expr(EnvStr, ExprString, []).

type_check_expr(EnvStr, ExprString, Opts) ->
    Env = create_env(EnvStr, Opts),
    Expr = merl:quote(ExprString),
    {Ty, _VarBinds, _Cs} = typechecker:type_check_expr(Env, Expr),
    typelib:pp_type(Ty).

create_env(String, Opts) ->
    Forms = ensure_form_list(merl:quote(String)),
    ParseData = typechecker:collect_specs_types_opaques_and_functions(Forms),
    typechecker:create_env(ParseData, Opts).
