%% @doc This is an eunit test suite. Run the tests using
%%      eunit:test(typechecker, [verbose]).
-module(typechecker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Macro to convert type to abstract form
-define(t(T), parse_type(??T)).

subtype_test() ->
    %% The unknown type, both directions
    ?assert(subtype(?t( any()         ) , ?t( 1..10          ))),
    ?assert(subtype(?t( 1..10         ) , ?t( any()          ))),

    %% Term and none
    ?assert(subtype(?t( foo()         ) , ?t( term()         ))),
    ?assert(subtype(?t( none()        ) , ?t( foo()          ))),

    %% Integer
    ?assert(subtype(?t( 1             ) , ?t( integer()      ))),
    ?assert(subtype(?t( 1..5          ) , ?t( integer()      ))),
    ?assert(subtype(?t( 1..5          ) , ?t( 1..10          ))),
    ?assert(subtype(?t( 2             ) , ?t( 1..10          ))),

    %% Number
    ?assert(subtype(?t( 1               ) , ?t( number()       ))),
    ?assert(subtype(?t( 1..5            ) , ?t( number()       ))),
    ?assert(subtype(?t( integer()       ) , ?t( number()       ))),
    ?assert(subtype(?t( float()         ) , ?t( number()       ))),

    %% Atom
    ?assert(subtype(?t( a               ) , ?t( atom()         ))),
    ?assert(subtype(?t( a               ) , ?t( a              ))),

    %% Lists
    ?assert(subtype(?t( [a]             ) , ?t( list()         ))),
    ?assert(subtype(?t( list()          ) , ?t( [a]            ))),
    ?assert(subtype(?t( []              ) , ?t( list()         ))),
    ?assert(subtype(?t( []              ) , ?t( [a]            ))),
    ?assert(subtype(?t( [1]             ) , ?t( [1..5]         ))),
    ?assert(subtype(?t( nonempty_list() ) , ?t( list()         ))),
    ?assert(subtype(?t( nonempty_list() ) , ?t( [a]            ))),
    ?assert(subtype(?t( nonempty_list() ) , ?t( [a, ...]       ))),
    ?assert(subtype(?t( [a, ...]        ) , ?t( [a]            ))),

    %% Tuples
    ?assert(subtype(?t( {a,b,c}         ) , ?t( tuple()        ))),
    ?assert(subtype(?t( tuple()         ) , ?t( {a,b,c}        ))),
    ?assert(subtype(?t( {x, 1}          ) , ?t( {atom(), 1..5} ))),

    %% Maps
    ?assert(subtype(?t( map()           ) , ?t( #{a := b}      ))),
    ?assert(subtype(?t( #{a := b}       ) , ?t( map()          ))),
    ?assert(subtype(?t( #{a => b}       ) , ?t( #{}            ))),
    ?assert(subtype(?t( #{a := b}       ) , ?t( #{a => b}      ))),
    ?assert(subtype(?t( #{1..5 := a }   ) , ?t( #{5 := atom()} ))),
    ok.

not_subtype_test() ->
    %% Numeric
    ?assertNot(subtype(?t( 1             ), ?t( 2               ))),
    ?assertNot(subtype(?t( integer()     ), ?t( 1               ))),
    ?assertNot(subtype(?t( integer()     ), ?t( 1..5            ))),
    ?assertNot(subtype(?t( 1..10         ), ?t( 1               ))),
    ?assertNot(subtype(?t( 1..10         ), ?t( 1..5            ))),
    ?assertNot(subtype(?t( integer()     ), ?t( float()         ))),
    ?assertNot(subtype(?t( float()       ), ?t( integer()       ))),
    ?assertNot(subtype(?t( number()      ), ?t( integer()       ))),
    ?assertNot(subtype(?t( number()      ), ?t( float()         ))),

    %% Atom
    ?assertNot(subtype(?t( a             ), ?t( b               ))),

    %% Lists
    ?assertNot(subtype(?t( []            ), ?t( nonempty_list() ))),
    ?assertNot(subtype(?t( []            ), ?t( [a, ...]        ))),
    ?assertNot(subtype(?t( list()        ), ?t( nonempty_list() ))),
    ?assertNot(subtype(?t( [a]           ), ?t( nonempty_list() ))),
    ?assertNot(subtype(?t( [a]           ), ?t( [a, ...]        ))),
    ?assertNot(subtype(?t( [b]           ), ?t( [a]             ))),

    %% Tuples
    ?assertNot(subtype(?t( {}             ), ?t( {any()}        ))),
    ?assertNot(subtype(?t( {1..2, 3..4}   ), ?t( {1, 3}         ))),

    %% Maps
    ?assertNot(subtype(?t( #{}            ), ?t( #{a := b}      ))),
    ?assertNot(subtype(?t( #{a => b}      ), ?t( #{a := b}      ))),
    ?assertNot(subtype(?t( #{a := 1..5}   ), ?t( #{a := 2}      ))),
    ?assertNot(subtype(?t( #{1 := atom()} ), ?t( #{1 := a}      ))),
    ok.

-spec parse_type(string()) -> erl_parse:abstract_type().
parse_type(Src) ->
    AttrSrc = "-type t() :: " ++ Src ++ ".",
    {ok, Tokens, _EndLocation} = erl_scan:string(AttrSrc),
    {ok, {attribute, _, type, {t, Type, []}}} = erl_parse:parse_form(Tokens),
    Type.

subtype(T1, T2) ->
    case typechecker:subtype(T1, T2) of
	{true, _} ->
	    true;
	false ->
	    false
    end.
