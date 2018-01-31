-module(types).

-export([process_type/1, create_type_env/1,
	mk_graph_top/2, mk_graph/3]).

% Every subterm of a type has an identity. 
-type type() :: {integer(), term()}.

-type rec() :: {integer(), rec()}.

% -type type_rec() :: int() | ty_var() | tuple(list(type())).

-type warning() :: atom().

-spec process_type(term()) -> {term(),warning()}.
process_type(Type) ->
  {Type, warning}. 


create_type_env(Types) ->
    Graph = digraph:new(),
    mk_graph_top(Types, Graph),
    digraph_utils:cyclic_strong_components(Graph).

%% I will not need this stuff! I don't have to track which types are recursive.

%% mk_graph_top([], _Graph) ->
%%     ok;
%% mk_graph_top([{Name, Type, _IS_THIS_MAYBE_CONSTRAINTS} | Types], Graph) ->
%%     digraph:add_vertex(Graph, Name),
%%     mk_graph(Type, Name, Graph),
%%     mk_graph_top(Types, Graph).

%% mk_graph({type, _, Name, Args}, Top, Graph) ->
%%    % Maybe I shouldn't add types to the graph, only user_types
%%     digraph:add_vertex(Graph, Name),
%%     digraph:add_edge(Graph, Top, Name),
%%     _ = [ mk_graph(Type, Top, Graph) || Type <- Args ],
%%     ok;
%% mk_graph({user_type, _, Name, Args}, Top, Graph) ->
%%     digraph:add_vertex(Graph, Name),
%%     digraph:add_edge(Graph, Top, Name),
%%     _ = [ mk_graph(Type, Top, Graph) || Type <- Args ],
%%     ok.

type_eq({user_type, _, Name1, Args1},{user_type, _, Name2, Args2}) ->
    Name1 == Name2 andalso types_eq(Args1, Args2);
type_eq({tuple, _, Args1}, {tuple, _, Args2}) ->
    types_eq(Args1, Args2);
type_eq({atom, _, Atom1}, {atom, _, Atom2}) ->
    Atom1 == Atom2;
type_eq({integer, _, I1}, {integer, _, I2}) ->
    I1 == I2;
type_eq({type, _, 'fun', Arg1, ResTy1}, {type, _, 'fun', Arg2, ResTy2}) ->
    type_eq(Arg1, Arg2) andalso type_eq(ResTy1, ResTy2);
type_eq({product, _, Args1}, {product, _, Args2}) ->
    types_eq(Args1, Args2);
type_eq({type, _, Name1, Args1}, {type, _, Name2, Args2}) ->
    Name1 == Name2 andalso types_eq(Args1, Args2).


types_eq([],[]) ->
    true;
types_eq([Ty1 | Types1], [Ty2 | Types2]) ->
    type_eq(Ty1, Ty2) andalso types_eq(Types1, Types2);
types_eq(_,_) ->
    false.
