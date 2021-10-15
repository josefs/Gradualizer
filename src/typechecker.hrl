-ifndef(__TYPECHECKER_HRL__).
-define(__TYPECHECKER_HRL__, true).

-record(env, {fenv     = #{},
              imported = #{}   :: #{{atom(), arity()} => module()},
              venv     = #{},
              tenv             :: gradualizer_lib:tenv(),
              infer    = false :: boolean(),
              verbose  = false :: boolean(),
              exhaust  = true  :: boolean()
             }).

-endif. %% __TYPECHECKER_HRL__
