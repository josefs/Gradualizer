-module(gradualizer_type_gen).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-type ascii_lower_t() :: 97..122.
-type ascii_number_t() :: 48..57.
-type ascii_upper_t() :: 64..90.
-type ascii_alpha_t() :: ascii_lower_t() | ascii_upper_t().
-type ascii_alnum_t() :: ascii_lower_t() | ascii_number_t() | ascii_upper_t().

-type readable_string_t() :: [ascii_alnum_t()].

readable_char() ->
    ?LET(C, readable_char_t(), C).

readable_string() ->
    ?LET(S, readable_string_t(), S).

readable_nonempty_string() ->
    ?LET({Al, Alnum}, {ascii_alpha_t(), readable_string_t()}, [Al | Alnum]).

readable_lcid() ->
    ?LET({Lower, Alnum}, {ascii_lower_t(), readable_string_t()}, [Lower | Alnum]).

readable_ucid() ->
    ?LET({Upper, Alnum}, {ascii_upper_t(), readable_string_t()}, [Upper | Alnum]).

readable_atom() ->
    ?LET(PreAtom, readable_lcid(), list_to_atom(PreAtom)).

type_name() ->
    readable_atom().

%% This generates repeated fields, not sure yet if it's a problem.
-type annotation() :: {'file', readable_string_t()}
                    | {'generated', generated()}
                    | {'location', location_t()}
                    | {'record', record()}.

-type anno_t() :: location_t().
%% The generated annotations are pretty big,
%% which is not the usual case in the real world.
%anno() ->
%    ?LET(Anno, anno_t(), Anno).
%
%% This is closer to reality.
anno() ->
    ?LET(T, frequency([{9, location_t()}, {1, list(annotation())}]), T).
%
%% Whereas this allows for defining way more of the generators with just type definitions.
%-type anno() :: location_t().

-type column() :: pos_integer().
-type generated() :: boolean().
-type line() :: non_neg_integer().
-type location_t() :: line() | {line(), column()}.
-type record() :: boolean().
-type text() :: string().

abstract_type() ->
    ?LAZY(oneof([
                 af_annotated_type(),
                 af_atom(),
                 %af_bitstring_type(),
                 af_empty_list_type(),
                 %af_fun_type(),
                 af_integer_range_type(),
                 %af_map_type(),
                 af_predefined_type(),
                 %af_record_type(),
                 %af_remote_type(),
                 %af_singleton_integer_type(),
                 af_tuple_type()
                 %af_type_union(),
                 %af_type_variable(),
                 %af_user_defined_type()
                ])).

af_annotated_type() ->
    ?LET({Anno, AfAnno, AbsType}, {anno(), af_anno(), abstract_type()},
         {'ann_type', Anno, [AfAnno, AbsType]}).

af_anno() ->
    ?LET(AfVar, af_variable(), AfVar).

af_variable() ->
    ?LET({Anno, At}, {anno(), readable_lcid()}, {'var', Anno, At}).

%% These cannot be easily expressed in the type language.
%-type af_atom_t() :: af_lit_atom(atom() | string()).
%-type af_lit_atom(A) :: {'atom', anno(), A}.

af_atom() ->
    ?LET({Anno, At},
         {anno(), oneof([readable_atom(), readable_nonempty_string()])},
         {atom, Anno, At}).

-type af_empty_list_type_t() :: {'type', anno_t(), 'nil', []}.

af_empty_list_type() ->
    ?LET(T, af_empty_list_type_t(), T).

-type af_integer_range_type_t() ::
        {'type', anno_t(), 'range', [af_range_integer_type()]}.

af_integer_range_type() ->
    %% SUCHTHAT would be nice, but we hit:
    %%
    %%   {cant_generate,[{gradualizer_type_gen,af_integer_range_type,0}]}
    %%
    %% even with constraint_tries as high as 5000.
    %?SUCHTHAT({type, _, range, Interval}, ?LET(T, af_integer_range_type_t(), T),
    %          length(Interval) == 2).
    %% Let's try a different approach.
    %% This one is also a lot faster!
    ?LET({Anno, Left, Right},
         {anno(), af_range_integer_type(), af_range_integer_type()},
         {type, Anno, range, [Left, Right]}).

-type af_range_integer_type() :: 'neg_inf' | 'pos_inf' | af_singleton_integer_type().

-type af_singleton_integer_type() :: af_integer()
                                   | af_character().
%% TODO: These are not handled by gradualizer_int:int_type_to_range()
%%       and it's not obvious how/if they should be.
                                   %| af_unary_op(af_singleton_integer_type())
                                   %| af_binary_op(af_singleton_integer_type()).

-type af_integer() :: {'integer', anno_t(), non_neg_integer()}.
-type af_character() :: {'char', anno_t(), ascii_alnum_t()}.

-type af_binary_op(T) :: {'op', anno_t(), binary_op(), T, T}.
-type binary_op() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                   | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                   | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                   | '=/='.

-type af_unary_op(T) :: {'op', anno_t(), unary_op(), T}.
-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

af_predefined_type() ->
    ?LET({Anno, TypeName},
         {anno(), predefined_type_name()},
         {type, Anno, TypeName, []}).

predefined_type_name() ->
    oneof([
           term, binary, nonempty_binary, bitstring, nonempty_bitstring, boolean, byte, char, nil,
           number, list, maybe_improper_list, nonempty_list, string, nonempty_string, iodata,
           iolist, map, function, module, mfa, arity, identifier, node, timeout, no_return,
           non_neg_integer, pos_integer, neg_integer
          ]).

af_tuple_type() ->
    ?LET({Anno, T},
         {anno(), oneof([any, [abstract_type()]])},
         {type, Anno, tuple, T}).
