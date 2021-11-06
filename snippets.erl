TF = fun (Trace, ok) -> io:format("~p\n", [Trace]) end.
dbg:stop_clear().
dbg:tracer(process, {TF, ok}).
%dbg:p(all, [call, return_to]).
dbg:p(all, [call]).
%dbg:tpl(typechecker, add_type_pat, x).
%dbg:tpl(typechecker, subtype, x).
%dbg:tpl(typechecker, glb, x).
dbg:tpl(typechecker, []).

application:ensure_all_started(gradualizer).
{ok, [Forms]} = file:consult("infinite-loop-forms.1.erl").
typechecker:type_check_forms(Forms, []).

TF = fun (Trace, ok) -> io:format("~p\n", [Trace]) end.
dbg:stop_clear().
dbg:tracer(process, {TF, ok}).
dbg:p(all, [call]).
dbg:tpl(gradualizer_type, reduce, x).

StartAt = 20000.
TF = fun
         (_, 0) -> erlang:halt();
         (Trace, R) -> io:format("~p\n", [Trace]), R - 1
     end.
dbg:stop_clear().
dbg:tracer(process, {TF, StartAt}).
dbg:p(all, [call, return_to]).
dbg:tpl(typechecker, normalize, x).
dbg:tpl(typechecker, normalize_rec, x).
%dbg:tpl(typechecker, flatten_type_rec_check, x).

code:add_path("/Users/erszcz/work/erszcz/gradualizer/_build/test/lib/gradualizer/test/property_test").
Ty = {user_type,1,t2,
      [{type,1,union,
        [{user_type,1,t2,
          [{type,1,record,
            [{atom,1,r1},
             {type,1,field_type,[{atom,1,f1},{user_type,1,t1,[]}]}]}]},
         {atom,1,type_variable},
         {user_type,1,t1,[]}]}]}.
Env = gradualizer_prop:create_env(Ty, [{return_text_env, false}]).
typechecker:normalize(typelib:remove_pos(Ty), Env).



StartAt = 20000.
TF = fun
         (_, 0) -> erlang:halt();
         (Trace, R) -> io:format("~p\n", [Trace]), R - 1
     end.
dbg:stop_clear().
dbg:tracer(process, {TF, StartAt}).
dbg:p(all, [call, return_to]).
%dbg:tpl(typechecker, glb, 4, x).
%dbg:tpl(typechecker, glb_ty, 4, x).
dbg:tpl(typechecker, stop_glb_recursion, 3, x).

code:add_path("/Users/erszcz/work/erszcz/gradualizer/_build/test/lib/gradualizer/test/property_test").
Ty1 = {user_type,1,t2,
       [{type,1,union,
         [{user_type,1,t2,
           [{type,1,record,
             [{atom,1,r1},
              {type,1,field_type,[{atom,1,f1},{user_type,1,t1,[]}]}]}]},
          {atom,1,type_variable},
          {user_type,1,t1,[]}]}]}.
Ty2 = typelib:parse_type("t1()").
Env = gradualizer_prop:create_env(gradualizer_prop:type_union([Ty1, Ty2]),
                                  [{return_text_env, false}]).
typechecker:glb(typelib:remove_pos(Ty1), typelib:remove_pos(Ty2), Env).

T1 = {user_type,0,t2,
      [{type,0,union,
        [{user_type,0,t2,
          [{type,0,record,
            [{atom,0,r1},
             {type,0,field_type,[{atom,0,f1},{user_type,0,t1,[]}]}]}]},
         {atom,0,type_variable},
         {user_type,0,t1,[]}]}]}.
T2 = {type,0,tuple,[]}.
A = #{{{type,0,union,
        [{atom,0,type_variable},
         {type,0,tuple,[]},
         {user_type,0,t2,
          [{type,0,union,
            [{user_type,0,t2,
              [{type,0,record,
                [{atom,0,r1},
                 {type,0,field_type,[{atom,0,f1},{user_type,0,t1,[]}]}]}]},
             {atom,0,type_variable},
             {user_type,0,t1,[]}]}]}]},
       {type,0,tuple,[]}} =>
      0}.

StartAt = 20000.
TF = fun
         (_, 0) -> erlang:halt();
         (Trace, R) -> io:format("~p\n", [Trace]), R - 1
     end.
dbg:stop_clear().
dbg:tracer(process, {TF, StartAt}).
dbg:p(all, [call, return_to]).
dbg:tpl(typechecker, refinable, x).
dbg:tpl(typechecker, normalize, x).

f().
StartAt = 20.
TF = fun
         (_, 0) -> erlang:halt();
         (Trace, R) -> io:format("~p\n~p\n\n", [R, Trace]), R - 1
     end.
dbg:stop_clear().
dbg:tracer(process, {TF, StartAt}).
dbg:p(all, [call, return_to]).
%dbg:tpl(typechecker, glb, 4, x).
%dbg:tpl(typechecker, glb_ty, 4, x).
dbg:tpl(typechecker, stop_glb_recursion, 3, x).

code:add_path("/Users/erszcz/work/erszcz/gradualizer/_build/test/lib/gradualizer/test/property_test").
TStr1 = "A :: A |
              t2(t2(any_atom) | #r2{} | #r1{f2 :: t2(t1())} | t1()) |
              t1()".
Ty1 = typelib:parse_type(TStr1).
Ty2 = typelib:parse_type("t2(t1())").
Env = gradualizer_prop:create_env(gradualizer_prop:type_union([Ty1, Ty2]),
                                  [{return_text_env, false}]).
typechecker:glb(typelib:remove_pos(Ty1), typelib:remove_pos(Ty2), Env).

f().
code:add_path("/Users/erszcz/work/erszcz/gradualizer/_build/test/lib/gradualizer/test/property_test").
%TStr1 = ""
%"t2(t2(#{14 := any_atom})) |"
%"t2(#{#{t1() := <<_:$Z, _:_*$q>>, tuple() => fun()} => #r3{f1 :: t2(t1())}}) |"
%"t1()"
%"".
TStr1 = ""
"t2(#{#{t1() := <<_:$Z, _:_*$q>>, tuple() => fun()} => #r3{f1 :: t2(t1())}})"
"".
Ty1 = typelib:parse_type(TStr1), ok.
Ty2 = typelib:parse_type("t2(t1())"), ok.
io:format("~ts\n", [typelib:pp_type(Ty1)]).
%io:format("~ts\n", [typelib:pp_type(Ty2)]).
{TextEnv, Env} = gradualizer_prop:create_env(gradualizer_prop:type_union([Ty1, Ty2]), [return_text_env]), ok.
io:format("~ts\n", [typelib:pp_type(Ty1)]).
io:format("~ts\n", [TextEnv]).
Ty1Norm1 = typechecker:normalize(typelib:remove_pos(Ty1), Env), ok.
io:format("~ts\n", [typelib:pp_type(Ty1Norm1)]).
Ty1Norm2 = typechecker:normalize(typechecker:normalize(typelib:remove_pos(Ty1), Env), Env), ok.
io:format("~ts\n", [typelib:pp_type(Ty1Norm2)]).
Ty1Norm3 = typechecker:normalize(typechecker:normalize(typechecker:normalize(typelib:remove_pos(Ty1), Env), Env), Env), ok.
io:format("~ts\n", [typelib:pp_type(Ty1Norm3)]).

%% Ok, the conclusion seems to be that norm^1(Ty) != norm^2(Ty), since on each call a given UserTy
%% is expanded once. I.e. the first call of norm expands t2() (the type grows) to its definition,
%% the second call expands t2 contained in the part expanded in the previous call, ...
%% This means we have to decouple managing the trace in GLB from normalising the params.
%% What about compatible, though? Maybe it's possible to do the same.
