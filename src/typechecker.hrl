-ifndef(__TYPECHECKER_HRL__).
-define(__TYPECHECKER_HRL__, true).

-record(clauses_controls, {exhaust}).

-record(env, {fenv              = #{}   :: #{{atom(), arity()} =>
                                                  [gradualizer_type:af_constrained_function_type()]
                                                | [gradualizer_type:gr_any_type()]
                                            },
              imported          = #{}   :: #{{atom(), arity()} => module()},
              venv              = #{}   :: typechecker:venv(),
              tenv                      :: gradualizer_lib:tenv(),
              infer             = false :: boolean(),
              verbose           = false :: boolean(),
              exhaust           = true  :: boolean(),
              %% Controls driving the type checking algorithm
              %% per clauses-list (fun/case/try-catch/receive).
              clauses_stack     = []    :: [#clauses_controls{}],
              %% Performance hack: Unions larger than this limit are replaced by any() in normalization.
              union_size_limit          :: non_neg_integer(),
              current_spec      = none  :: erl_parse:abstract_form() | none,
              solve_constraints = false :: boolean()
             }).

-endif. %% __TYPECHECKER_HRL__
