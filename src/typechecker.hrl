-ifndef(__TYPECHECKER_HRL__).
-define(__TYPECHECKER_HRL__, true).

-record(env, {fenv              = #{}   :: map(),
              imported          = #{}   :: #{{atom(), arity()} => module()},
              venv              = #{}   :: typechecker:venv(),
              tenv                      :: gradualizer_lib:tenv(),
              infer             = false :: boolean(),
              verbose           = false :: boolean(),
              exhaust           = true  :: boolean(),
              %% Performance hack: Unions larger than this limit are replaced by any() in normalization.
              union_size_limit          :: non_neg_integer(),
              current_spec      = none  :: erl_parse:abstract_form() | none
             }).

-endif. %% __TYPECHECKER_HRL__
