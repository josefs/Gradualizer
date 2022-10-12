%% Syntax elements for type annotations
%%
%% The macro ?annotate_type/2 can be used to annotate an expression with a
%% type.  This is useful to add type annotation after fetching a value from an
%% ets table, received a message on a known form or read from a dict, proplist,
%% process dictionary, etc.
%%
%% The specified type must be compatible with the type detected by Gradualizer.
%% Otherwise a type error is reported.
%%
%%     N = ?annotate_type( Message, non_neg_integer() )
%%
%% The macro ?assert_type/2 can be used to refine (downcast) a type propagated
%% by Gradualizer.  For example, the programmer may know that the length of a
%% list is within a certain range, rather than any non_neg_integer():
%%
%%     Arity = ?assert_type( length(Args), arity() )
%%
%% The functions '::'/2 and ':::'/2 can also be used directly if the type is
%% quoted:
%%
%%     N = '::'(Message, "non_neg_integer()")
%%
%% Gradualizer detects occurrences of the functions '::'/2 and ':::'/2 and
%% adjusts type checking accordingly.  The macros are supplied only for
%% convenience.
%%
-compile({inline, ['::'/2, ':::'/2]}).
-compile({nowarn_unused_function, ['::'/2, ':::'/2]}).
-ignore_xref(['::'/2, ':::'/2]).

'::'(Expr, _Type) -> Expr.
':::'(Expr, _Type) -> Expr.

%% Type annotation
-define(annotate_type(Expr, Type), '::'(Expr, ??Type)).

%% Refinement (downcast) AKA type assertion
-define(assert_type(Expr, Type), ':::'(Expr, ??Type)).
