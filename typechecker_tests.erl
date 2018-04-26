%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typechecker, [verbose]).
-module(typechecker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Macro to convert type to abstract form
-define(t(T), parse_type(??T)).

subtype_test() ->
    %% The unknown type, both directions
    ?assert(typechecker:subtype(?t( any()         ) , ?t( 1..10          ))),
    ?assert(typechecker:subtype(?t( 1..10         ) , ?t( any()          ))),

    %% Term and none
    ?assert(typechecker:subtype(?t( foo()         ) , ?t( term()         ))),
    ?assert(typechecker:subtype(?t( none()        ) , ?t( foo()          ))),

    %% Integer
    ?assert(typechecker:subtype(?t( 1             ) , ?t( integer()      ))),
    ?assert(typechecker:subtype(?t( 1..5          ) , ?t( integer()      ))),
    ?assert(typechecker:subtype(?t( 1..5          ) , ?t( 1..10          ))),
    ?assert(typechecker:subtype(?t( 2             ) , ?t( 1..10          ))),

    %% Number
    ?assert(typechecker:subtype(?t( 1               ) , ?t( number()       ))),
    ?assert(typechecker:subtype(?t( 1..5            ) , ?t( number()       ))),
    ?assert(typechecker:subtype(?t( integer()       ) , ?t( number()       ))),
    ?assert(typechecker:subtype(?t( float()         ) , ?t( number()       ))),

    %% Atom
    ?assert(typechecker:subtype(?t( a               ) , ?t( atom()         ))),
    ?assert(typechecker:subtype(?t( a               ) , ?t( a              ))),

    %% Lists
    ?assert(typechecker:subtype(?t( [a]             ) , ?t( list()         ))),
    ?assert(typechecker:subtype(?t( list()          ) , ?t( [a]            ))),
    ?assert(typechecker:subtype(?t( []              ) , ?t( list()         ))),
    ?assert(typechecker:subtype(?t( []              ) , ?t( [a]            ))),
    ?assert(typechecker:subtype(?t( [1]             ) , ?t( [1..5]         ))),
    ?assert(typechecker:subtype(?t( nonempty_list() ) , ?t( list()         ))),
    ?assert(typechecker:subtype(?t( nonempty_list() ) , ?t( [a]            ))),
    ?assert(typechecker:subtype(?t( nonempty_list() ) , ?t( [a, ...]       ))),
    ?assert(typechecker:subtype(?t( [a, ...]        ) , ?t( [a]            ))),

    %% Maps
    ?assert(typechecker:subtype(?t( map()           ) , ?t( #{a := b}      ))),
    ?assert(typechecker:subtype(?t( #{a := b}       ) , ?t( map()          ))),
    ?assert(typechecker:subtype(?t( #{a => b}       ) , ?t( #{}            ))),
    ?assert(typechecker:subtype(?t( #{a := b}       ) , ?t( #{a => b}      ))),
    ?assert(typechecker:subtype(?t( #{1..5 := a }   ) , ?t( #{5 := atom()} ))),
    ok.

not_subtype_test() ->
    %% Numeric
    ?assertNot(typechecker:subtype(?t( 1             ), ?t( 2               ))),
    ?assertNot(typechecker:subtype(?t( integer()     ), ?t( 1               ))),
    ?assertNot(typechecker:subtype(?t( integer()     ), ?t( 1..5            ))),
    ?assertNot(typechecker:subtype(?t( 1..10         ), ?t( 1               ))),
    ?assertNot(typechecker:subtype(?t( 1..10         ), ?t( 1..5            ))),
    ?assertNot(typechecker:subtype(?t( integer()     ), ?t( float()         ))),
    ?assertNot(typechecker:subtype(?t( float()       ), ?t( integer()       ))),
    ?assertNot(typechecker:subtype(?t( number()      ), ?t( integer()       ))),
    ?assertNot(typechecker:subtype(?t( number()      ), ?t( float()         ))),

    %% Atom
    ?assertNot(typechecker:subtype(?t( a             ), ?t( b               ))),

    %% Lists
    ?assertNot(typechecker:subtype(?t( []            ), ?t( nonempty_list() ))),
    ?assertNot(typechecker:subtype(?t( []            ), ?t( [a, ...]        ))),
    ?assertNot(typechecker:subtype(?t( list()        ), ?t( nonempty_list() ))),
    ?assertNot(typechecker:subtype(?t( [a]           ), ?t( nonempty_list() ))),
    ?assertNot(typechecker:subtype(?t( [a]           ), ?t( [a, ...]        ))),
    ?assertNot(typechecker:subtype(?t( [b]           ), ?t( [a]             ))),

    %% Maps
    ?assertNot(typechecker:subtype(?t( #{}            ), ?t( #{a := b}      ))),
    ?assertNot(typechecker:subtype(?t( #{a => b}      ), ?t( #{a := b}      ))),
    ?assertNot(typechecker:subtype(?t( #{a := 1..5}   ), ?t( #{a := 2}      ))),
    ?assertNot(typechecker:subtype(?t( #{1 := atom()} ), ?t( #{1 := a}      ))),
    ok.

-spec parse_type(string()) -> erl_parse:abstract_type().
parse_type(Src) ->
    AttrSrc = "-type t() :: " ++ Src ++ ".",
    {ok, Tokens, _EndLocation} = erl_scan:string(AttrSrc),
    {ok, {attribute, _, type, {t, Type, []}}} = erl_parse:parse_form(Tokens),
    Type.
