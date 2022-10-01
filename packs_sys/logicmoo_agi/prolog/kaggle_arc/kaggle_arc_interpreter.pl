/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


decl_pt(_):- fail.

check_args(P,MC):- functor(P,F,A),functor(T,F,A),functor(C,F,A),decl_pt(_,T),check_args(P,1,A,A,T,C,MC),!.
check_args(P,P):- !. 

check_args(P,Arity,Arity,1,T,C,MC):- fail,
 call(C), arg(An,T,ArgType),arg(An,C,Result),
 MC = t,
 arg(An,P,Return),into_type(ArgType,Result,Return).

check_args(P,Arity,An,2,T,C,MC):- fail,
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),
 is_group(ArgIn), ArgType = object,!,
 arg(Arity,P,Result),arg(Arity,C,Return),
 findall(Return,(member(CallArg,ArgIn),check_args(P,Arity,Arity,1,T,C,MC)),Result).

check_args(P,Arity,An,Left,T,C,MC):-  
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),into_type(ArgType,ArgIn,CallArg),
 AnM1 is An+1,LeftP1 is Left-1, check_args(P,Arity,AnM1,LeftP1,T,C,MC),!.
check_args(_P,_Arity,_An,_Left,_T,C,C).

into_type(Type,G,O):- nonvar_or_ci(O),!,into_type(Type,G,M),!,M=O.
into_type(_Type,G,O):- plain_var(G),O=G,!.
into_type(Type,G,O):- plain_var(G),throw(var_into_type(Type,G)),O=fake(Type,G).
into_type(+,X,X).
into_type(oid,X,ID):- into_oid(X,ID).
into_type(num,X,X):- assertion(number(X)).
into_type(dir,X,X):- assertion(nav(X,_,_)).
into_type(grid,X,O):- into_grid(X,O).
into_type(object,X,O):- is_object(X)-> X=O ; into_obj(X,O).
into_type(group,X,O):- into_group(X,O).

pass_thru_workflow(G):- var(G),!.
pass_thru_workflow([]).
pass_thru_workflow([options(V)]):- nonvar(V).

member_or_it(G,InO):- is_list(InO),!,member(G,InO).
member_or_it(G,G).


show_workflow(InO,_,InO):-pass_thru_workflow(InO),!. 
show_workflow(In,String,Out):- nonvar(Out),!,arcST,trace,must_det_ll((show_workflow(In,String,OutM),Out=OutM)).
show_workflow(InO,String,InO):- string(String),!, 
 ignore((InO\==[], nl, writeln(String), forall(member_or_it(G,InO),ignore(print_grid(_,_,String,G))))).
show_workflow(InO,[],InO):-!.
show_workflow(In,[H|T],Out):-
  show_workflow(In,H,Mid),!,
  show_workflow(Mid,T,Out).
show_workflow(In,add(P),Out):- !,
  show_workflow(In,P,Mid),!,
  my_append(Mid,In,Out).
show_workflow(In,each(P),Out):- show_workflow_each(In,P,Out).
show_workflow(In,P,Out):- must_det_ll(call(P,In,Out)),!.
show_workflow(In,P,In):- arcdbg(warn(failed(show_workflow(P)))),!.
 
show_workflow_each([],_P,[]):-!.

show_workflow_each(In,P,Out):- \+ is_list(In),into_list(In,List),!,show_workflow_each(List,P,Out).

show_workflow_each(In,P,Out):- is_grid(In),!,show_workflow(In,P,Out).
show_workflow_each([H|T],P,[Mid|Out]):-
  show_workflow(H,P,Mid),!,
  show_workflow_each(T,P,Out).
show_workflow_each(In,P,Out):- show_workflow(In,P,Out).

into_singles(Obj,Obj):- is_object(Obj),!.
into_singles(Group,Obj):- is_group(Group),!,member(Obj,Group).
into_singles(Class,Obj):- (iz(Obj,Class),deterministic(YN)), (YN==true->!;true).
into_singles(Obj,Obj).

vert_pos(Class,Y):- into_singles(Class,Obj),loc(Obj,_X,Y).
horiz_pos(Class,X):- into_singles(Class,Obj),loc(Obj,X,_Y).

when_config(This,Goal):-test_config(This)-> call(Goal) ; true.
test_config(This):- get_current_test(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

test_cond_or(This,_That):- test_config(This),!.
test_cond_or(This, That):- term_variables(This,[That|_]),!.

call_expanded(VM,G):-  exp_call(VM,G,GG),G\=@=GG,!,call_expanded(VM,GG).
call_expanded(_VM,G):- catch(call(G),E,(arcST,pp(E),rrtrace(G))).

quinish(Var):- var(Var),!.
quinish(Var):- is_grid(Var),!.
quinish(Var):- is_map(Var),!.
quinish(Var):- is_object(Var),!.
quinish(Var):- is_group(Var),!.
quinish(Var):- is_list(Var),!.
quinish(Var):- number(Var),!.

exp_call(_,Var,Var):- quinish(Var),!.
exp_call(VM,get(K),Value):- !, get_kov(K,VM,Value).
%exp_call(VM,length(G1,G2),length(GG1,GG2)):-!, exp_call(VM,G1,GG1),exp_call(VM,G2,GG2).
%exp_call(VM,(G1,G2),(GG1,GG2)):-!, exp_call(VM,G1,GG1),exp_call(VM,G2,GG2).
%exp_call(VM,(G1;G2),(GG1;GG2)):-!, exp_call(VM,G1,GG1),exp_call(VM,G2,GG2).
%exp_call(VM,(G1->G2),(GG1->GG2)):-!, exp_call(VM,G1,GG1),exp_call(VM,G2,GG2).
exp_call(VM,K,Value):- atom(K),notrace(catch(get_kov(K,VM,Value),_,fail)),!.
%  len(points(i))>6
%exp_call(VM,G,GG):- compound(G),arg(N,G,A),nonvar(A),exp_call(VM,A,AA),A\==AA,!,setarg(N,G,AA),exp_call(VM,G,GG).
exp_call(_VM,P,(length(X,R),call(F,R,N))):- P=..[F,E,N],compound(E),E=len(X),!.
exp_call(_VM,Expr,Value):- notrace(catch(Value is Expr,_,fail)),!.
exp_call(VM,I,O):- compound(I),
       compound_name_arguments(I, F, Args),
       maplist(exp_call(VM), Args, ArgsNew),
       compound_name_arguments( O, F, ArgsNew ),!.
%exp_call(_VM,P,(=(X,R),call(F,R,N))):- P=..[F,E,N],compound(E),E=val(X).
exp_call(_,E,E).

run_dsl(Prog,In,Out):- run_dsl(_VM,Prog,In,Out).
run_dsl(VM,Prog,In,Out):- 
 ((var(VM)->get_training(VM);true)), 
   luser_linkval(dsl_pipe,In),run_dsl(VM,enact,Prog,In,Out).


vm_grid(VM,Goal,In,Out):- In==VM,!,vm_grid(VM,Goal,VM.grid,Out).
vm_grid(VM,Goal,In,Out):-
  maybe_set_vm(VM),
  set_vm_grid(VM,In),
  my_submenu_call(Goal),!,
  (var(Out) -> Out = VM.grid ; set_vm_grid(VM,Out)).  

set_vm_grid(VM,In):- In == VM,!.
set_vm_grid(VM,In):- var(In),!, In = VM.grid . 
set_vm_grid(VM,In):- is_grid(In), !, set_vm_grid_now(VM,In).
set_vm_grid(VM,In):- into_grid(In,Grid), set_vm_grid_now(VM,Grid),!.
set_vm_grid(VM,In):- is_map(In), map_to_grid(_Was,In,Obj,_Grid,_Closure), Obj\=@=In, !, set_vm_grid(VM,Obj).
set_vm_grid(VM,In):- collapsible_section(debug,set_vm_grid_now(VM,In)).

set_vm_grid_now(VM,Grid):- VM.grid=@=Grid,!.
set_vm_grid_now(VM,Grp):- 
  data_type(Grp,Type),
  gset(VM.type) = data_type(Type),
  pp(yellow,set_vm_grid_now(Type)),pp(cyan,Type),fail.
set_vm_grid_now(VM,In):- VM==In,!.
set_vm_grid_now(VM,In):- is_map(In),!,map_to_grid(_Was,In,Obj,_Grid,_Closure), Obj\=@=In, !, set_vm_grid(VM,Obj).
set_vm_grid_now(VM,Grp):- is_group(Grp), !,
  gset(VM.points_o) = VM.points,
  gset(VM.objs)=Grp,
  globalpoints(Grp, Points),
  gset(VM.points)=Points,
  grid_size(Grp,H,V), gset(VM.h)=H, gset(VM.v)=V, 
  points_to_grid(H,V,Points,Grid),
  gset(VM.grid)=Grid,!.

set_vm_grid_now(VM,Obj):- is_object(Obj), !,
  gset(VM.objs)=[Obj],
  object_grid(Obj,Grid),
  gset(VM.grid)=Grid,
  v_hv(Grid,H,V), gset(VM.h)=H, gset(VM.v)=V, 
  gset(VM.points_o) = VM.points,
  localpoints_include_bg(Obj, Points),
  gset(VM.points)=Points .

set_vm_grid_now(VM,Grid):- is_grid(Grid), !,
  gset(VM.grid)=Grid, 
  grid_size(Grid,H,V), gset(VM.h)=H, gset(VM.v)=V, 
  gset(VM.points_o) = VM.points,
  localpoints_include_bg(Grid, Points),
  gset(VM.points)=Points.

set_vm_grid_now(VM,In):- gset(VM.last_key) = In,!.

expand_dsl_value(VM, Mode,In,Val,OutValue):- is_list(Val),!, maplist(expand_dsl_value(VM, Mode,In),Val,OutValue).
expand_dsl_value(VM, Mode,In,Val,OutValue):-
  run_dsl(VM, Mode,Val,In,OutValue).

:- meta_predicate(run_dsl(+,+,+,+,-)).

run_dsl(VM,Mode,AttVar,In,Out):- attvar(AttVar),get_attr(AttVar,prog,Prog),!,run_dsl(VM,Mode,Prog,In,Out).
run_dsl(VM,Mode,Prog,_In,_Out):- var(Prog),!,term_hash(VM,Hash),throw(var_solving_progs(vm(Hash),Mode,Prog,in,out)).
run_dsl(VM,Mode,lmDSL(Prog),In,Out):- !, run_dsl(VM,Mode,Prog,In,Out).
run_dsl(_VM,_Mode,[],In,Out):- !,In=Out,nop(( plain_var(Out)->Out=In; true)).
run_dsl(VM,Mode,(H*->Prog1;Prog2),In,Out):-!, (run_dsl(VM,Mode,H,In,GridM)*->run_dsl(VM,Mode,Prog1,GridM,Out);run_dsl(VM,Mode,Prog2,In,Out)).
run_dsl(VM,Mode,(H->Prog1;Prog2),In,Out):-!, (run_dsl(VM,Mode,H,In,GridM)->run_dsl(VM,Mode,Prog1,GridM,Out);run_dsl(VM,Mode,Prog2,In,Out)).
run_dsl(VM,Mode,(Prog1;Prog2),In,Out):-!, (run_dsl(VM,Mode,Prog1,In,Out);run_dsl(VM,Mode,Prog2,In,Out)).
run_dsl(VM,Mode,[H|Prog],In,Out):-!, run_dsl(VM,Mode,H,In,GridM), run_dsl(VM,Mode,Prog,GridM,Out).
run_dsl(VM,Mode,(H,Prog),In,Out):-!, run_dsl(VM,Mode,H,In,GridM), run_dsl(VM,Mode,Prog,GridM,Out).
run_dsl(VM,Mode,doall(All),In,OutO):- !, run_dsl(VM,Mode,forall(All,true),In,OutO).
run_dsl(VM,Mode,-->(All,Exec),In,Out):-!, run_dsl(VM,Mode,forall(All,Exec),In,Out).
run_dsl(VM,Mode,forall(All,Exec),In,OutO):-!,  
 luser_linkval(dsl_pipe,In),
 forall(run_dsl(VM,Mode,All,dsl_pipe,Mid),
  (run_dsl(VM,enforce,Exec,Mid,Out),
   luser_linkval(dsl_pipe,Out))),luser_getval(dsl_pipe,OutO).

run_dsl(VM,_Mode,call(G),In,Out):-!, call_expanded(VM,G),(plain_var(Out)->Out=In; true).

%run_dsl(VM, Mode,[N|V],In,OutValue):-!, vm_grid(VM, maplist(expand_dsl_value(VM, Mode,In),[N|V],OutValue),In,_Out).

run_dsl(VM, Mode,Name=Val,In,Out):- nonvar(Name),run_dsl(VM, Mode,nb_set(Name,Val),In,Out).
run_dsl(VM,_Mode,get(Name,Val),In,Out):- !, vm_grid(VM,get_vm(Name,Val),In,Out).

run_dsl(VM,_Mode,get(Name),In,Out):- !, vm_grid(VM, (get_vm(Name,Out),nonvar(Out)),In,Out),!.
run_dsl(VM,_Mode,get(Name),_In,Out):- maybe_set_vm(VM),
  must_det_ll(get_vm(Name,Val)),nonvar(Val),!,into_grid(Val,VOut), trim_to_rect(VOut,Rect), print_grid(Name,Rect),
  set(VM.grid)=Rect,
  Rect=Out.
%run_dsl(VM, Mode,(Name),In,Val):- atom(Name), vm_grid(VM,get_vm(Name,Val),In,_Out). %%atom(Name),run_dsl(VM,Mode, nb_get_value(VM,Name,Val),In,_),!.
run_dsl(VM, Mode,SET_NV,In,Out):- compound(SET_NV), SET_NV=..[set,Name,Val], !, expand_dsl_value(VM, Mode,In,Val,OutValue), run_dsl(VM,Mode,vm_set(Name,OutValue),In,Out).
run_dsl(VM,_Mode,b_set(Name,Val),In,Out):- !, expand_dsl_value(VM, Mode,In,Val,OutValue),run_dsl(VM,Mode,vm_set(Name,OutValue),In,Out).
run_dsl(VM,_Mode,nb_set(Name,Val),In,Out):- !, expand_dsl_value(VM, Mode,In,Val,OutValue),run_dsl(VM,Mode,vm_set(Name,OutValue),In,Out).
run_dsl(VM,_Mode,nb_link(Name,Val),In,Out):- !, expand_dsl_value(VM, Mode,In,Val,OutValue),run_dsl(VM,Mode,vm_set(Name,OutValue),In,Out).
run_dsl(VM,_Mode,vm_set(Name,Val),In,Out):- !, vm_grid(VM,set_vm(Name,Val),In, Out).
run_dsl(VM,_Mode,i(Indiv),In,Out):- !, vm_grid(VM,(individuate(Indiv,In,Objs),set_vm_grid(VM,Objs)),In,Out).

run_dsl(VM,_Mode,o(_Indiv),In,In):- var(VM.grid_target),!.
run_dsl(VM,_Mode,o(Indiv),In,Out):- !, vm_grid(VM,(individuate(Indiv,VM.grid_target,Objs),set_vm_grid(VM,Objs)),In,Out).
run_dsl(_VM,_Mode,get_in(In),Pass,Pass):- copy_term(Pass,In),!.
run_dsl(_VM,_Mode,set_out(Out),_In,Out):-!.

run_dsl(_VM,Mode,Prog,In,_Out):- ppt(yellow,run_dsl(vm,Mode,Prog,in,out)), once(print_grid(_,_,Prog,In)),fail.

run_dsl(VM,Mode,Prog,In,Out):- In==dsl_pipe,!,  must_det_ll((luser_getval(dsl_pipe,PipeIn),PipeIn\==[])), run_dsl(VM,Mode,Prog,PipeIn,Out).
run_dsl(VM,Mode,Prog,In,Out):- Out==dsl_pipe,!, run_dsl(VM,Mode,Prog,In,PipeOut),luser_linkval(dsl_pipe,PipeOut).
run_dsl(_VM,_Mode,sameR,In,Out):-!, duplicate_term(In,Out).

% prevents unneeded updates such as color/position settings
run_dsl(VM,_Mode,Prog,In,Out):- \+ missing_arity(Prog, 0), !, vm_grid(VM, call_expanded(VM,Prog),In,Out).
run_dsl(VM,_Mode,Step,In,Out):- \+ missing_arity(Step, 1), functor(Step,F,_), is_fti_step(F), !, vm_grid(VM, call(Step,VM),In,Out).
run_dsl(VM,_Mode,Step,In,Out):- \+ missing_arity(Step, 1), functor(Step,F,_), is_fti_stepr(F), Step=..[F|ARGS], !, vm_grid(VM, apply(F,[VM|ARGS]),In,Out).
run_dsl(VM,_Mode,Step,In,Out):- \+ missing_arity(Step, 1), functor(Step,F,_), ping_indiv_grid(F), !, vm_grid(VM, call(Step,VM.grid),In,Out).

run_dsl(VM,_Mode,Step,In,Out):-  i_step(Step), !, vm_grid(VM,fti(VM,Step),In,Out).


run_dsl(VM,enforce,color(Obj,Color),In,Out):-!, 
 color(Obj,ColorWas),subst_color(ColorWas,Color,In,Out),
    override_object_io(VM,color(Color),Obj,In,Out).

run_dsl(VM,enforce,vert_pos(Obj,New),In,Out):-!, loc(Obj,X,_Old), override_object_io(VM,loc(X,New),Obj,In,Out).

run_dsl(VM,Mode,Prog,In,Out):- \+ missing_arity(Prog,2), !, 
  vm_grid(VM, run_dsl_call_io(VM,Mode,Prog,In,Out), In, Out).

run_dsl(_VM,Mode,Prog,In,In):- arcdbg(warn(missing(run_dsl(Mode,Prog)))),!,fail.


run_dsl_call_io(VM,Mode,Prog,In,Out):- ( (call_expanded(VM,call(Prog,In,M))) 
  *->  M=Out ; (arcdbg(warn(nonworking(run_dsl(Mode,Prog)))),fail)).
%run_dsl_call_io(_VM,_Mode,_Prog,InOut,InOut).

override_object_io(_VM,Update,Obj,In,Out):- 
  remove_global_points(Obj,In,Mid), 
  override_object(Update,Obj,ObjCopy), 
  add_global_points(ObjCopy,Mid, Out).


sync_colors(Orig,Colors):- is_object(Orig),!,colors(Orig,Colors),
  globalpoints(Orig,OrigGPoints),colors(OrigGPoints,Colors),
  localpoints(Orig,OrigLPoints),colors(OrigLPoints,Colors),!.
sync_colors(Orig,Colors):- colors(Orig,Colors).

uncast_grid_to_object(Orig,Grid,NewObj):- 
 must_det_ll((
  localpoints(Grid,LocalPoints),
  (( LocalPoints==[]) -> (arcST,writeq(LocalPoints),trace ); true),
  rebuild_from_localpoints(Orig,LocalPoints,NewObj))).

closure_grid_to_group(Orig,Grid,Group):- individuate(Orig,Grid,Group).

back_to_map(Was,Dict,Prev,Grid,Closure,New, Ret):-
  ppt(back_to_map(Was,Dict,Prev,Grid,Closure,New)),
  call(Closure,New,NewPrev),
  gset(Dict.Was) = NewPrev ,
  Ret = Dict.

:- if( \+ current_predicate(any_to_ace_str/2)).
:- include(kaggle_arc_pfc).
%:- use_module(library(pfc_lib)).
:- endif.

:- decl_pt(into_grid(+(any),-mv(grid))).
into_grids(P,G):- no_repeats(G,quietly(cast_to_grid(P,G, _))).

:- decl_pt(into_grid(+(any),-grid)).
into_grid(A+B,AA+BB):- nonvar(A), !, cast_to_grid(A,AA, _),cast_to_grid(B,BB, _).
into_grid(P,G):- cast_to_grid(P,G, _).

map_to_grid(objs,Dict,Obj,Grid,Closure):- get_kov(objs,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.
map_to_grid(grid,Dict,Grid,Obj,Closure):- get_kov(grid,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.
map_to_grid(points,Dict,Obj,Grid,Closure):- get_kov(points,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.

print_grid_to_string(G,S):- with_output_to(string(S),print_grid(G)).
print_grid_to_atom(G,S):- with_output_to(atom(S),print_grid(G)).
% ?- print_grid(gridFn(X)).
cast_to_grid(P,G, =):- var(P),!,ignore(get_current_test(TestID)),test_grids(TestID,G),grid_to_tid(G,P).
cast_to_grid(Grid,Grid, (=) ):- is_grid(Grid),!.
cast_to_grid(gridOpFn(Grid,OP),GridO,reduce_grid):- !, unreduce_grid(Grid,OP,GridO).
cast_to_grid(Points,Grid,globalpoints):- is_points_list(Points), !, points_to_grid(Points,Grid),!.
cast_to_grid(Obj,Grid, uncast_grid_to_object(Obj)):- is_object(Obj),!, object_grid(Obj,Grid),!.
cast_to_grid(Grp,Grid, closure_grid_to_group(Grp)):- is_group(Grp), object_grid(Grp,Grid),!.
cast_to_grid(Obj,Grid, Closure):- resolve_reference(Obj,Var), Obj=@=Var, !,cast_to_grid(Var,Grid,Closure).
cast_to_grid(Text,Grid, print_grid_to_string ):- string(Text),!,text_to_grid(Text,Grid).
cast_to_grid(OID, Grid, (=) ):- atom(OID),oid_to_gridoid(OID,Grid),!.
cast_to_grid(Text,Grid, print_grid_to_atom ):- atom(Text),!,text_to_grid(Text,Grid).
% TODO Comment out next line to prefer the line after
cast_to_grid(Dict,Grid, (=) ):- is_map(Dict), get_kov(grid,Dict,Grid),!.
cast_to_grid(Dict,Grid, back_to_map(Was,Dict,Prev,Grid,Closure)):- is_map(Dict), map_to_grid(Was,Dict,Prev,Grid,Closure),!.
cast_to_grid(TestID>(Tst+N)*IO,Grid,(=)):- !, kaggle_arc_io(TestID,(Tst+N),IO,Grid).
cast_to_grid(Naming,Grid, Closure ):- 
  ((known_gridoid(Naming,NG),Naming\==NG,cast_to_grid(NG,Grid, Closure))*->true;
   (fail,recast_to_grid0(Naming,Grid, Closure))).
  
recast_to_grid0(Points,Grid, throw_no_conversion(Points,grid)):- compound(Points),
  grid_size(Points,GH,GV),
  make_grid(GH,GV,Grid),
  Success = found_points(false),
  forall(between(1,GV,V),
   ((nth1(V,Grid,Row),
     forall(between(1,GH,H),      
     (hv_c_value_or(Points,CN,H,V,_)->
        (nb_set_nth1(H,Row,CN), 
         (arg(1,Success,false)->nb_setarg(1,Success,true);true))))))),!,
  Success = found_points(true).


vm_objs(O,VM):- largest_first(VM.objs,List),!, member(O,List).
% vm_objs(O):- get_vm(VM), vm_objs(O,VM).
%first_object_grid.
first_object_term(Prop,_In,At):- get_vm(VM), vm_objs(O,VM), call(Prop,O,Ret), make_key(Ret,At),!, gset(VM.last_key) = At.
first_object_grid(Prop,_In,At):- get_vm(VM), vm_objs(O,VM), call(Prop,O,Ret), make_key(Ret,At),!, gset(VM.last_key) = At.
first_object_bool(Prop,_In,At):- get_vm(VM), vm_objs(O,VM), call_bool(iz(O,Prop),Ret),!, make_key(iz(Ret),At),!, gset(VM.last_key) = At.
%first_object_grid(Prop,_In,Ret):- get_vm(VM), member(O,VM.objs), call(Prop,O,Ret).

make_key(Ret,Ret):-!.
make_key(Ret,At):-
  copy_term(Ret,Copy),numbervars(Copy,0,_,[attvar(bind)]),
  with_output_to(atom(At),
  write_term(Copy,[attributes(ignore),ignore_ops(true),numbervars(true)])).
monogrid(X,Y):- object_grid(X,M),into_monochrome(M,Y).                                                   


uncast(_Obj,Closure,In,Out):- call(Closure,In,Out).
%known_gridoid(ID,G):- plain_var(ID),!,(known_grid(ID,G);known_object(ID,G)).
known_gridoid(ID,G):- is_object(ID),!,G=ID.
known_gridoid(ID,G):- known_object(ID,G),!.
known_gridoid(ID,G):- known_grid(ID,G).
%known_gridoid(ID,G):- plain_var(ID),!,arcST,throw(var_named_test(ID,G)).

known_grid(ID,GO):- (known_grid0(ID,G),deterministic(YN),true), (YN==true-> ! ; true), to_real_grid(G,GO).


oid_to_gridoid(GID,G):- current_predicate(gid_to_grid/2), call(call,gid_to_grid,GID,G).
oid_to_gridoid(ID,G):-  atom(ID),atomic_list_concat(Term,'_',ID), Term\==[ID], !,append(GOID,[OID],Term),
  testid_name_num_io(GOID,_Name,_Example,_Num,_IO),
  ((atom(OID),atom_number(OID,ONum))-> int2glyph(ONum,GL);GL=OID),
  get_current_test(TestID),
  g_2_o(TestID,GL,G).


known_grid0(ID,_):- var(ID),!,fail.
known_grid0(ID,G):- is_grid(ID),!,G=ID.
known_grid0(ID,_):- is_list(ID),!,fail.
known_grid0(OID, Grid):- atom(OID),oid_to_gridoid(OID,Grid),!.
known_grid0(ID,_):- is_object(ID),!,fail.
known_grid0(_,ID):- is_object(ID),!,fail.
known_grid0(ID,G):-  testid_name_num_io(ID,TestID,Trn,Num,IO),ExampleNum=Trn+Num,!,(kaggle_arc_io(TestID,ExampleNum,IO,G),deterministic(YN),true),(YN==true-> ! ; true).
known_grid0(ID,G):- is_grid_tid(G,ID),!.
known_grid0(ID,G):- fix_test_name(ID,Name,ExampleNum),!,
  (kaggle_arc_io(Name,ExampleNum,_IO,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- learned_color_inner_shape(ID,magenta,BG,G,_),get_bgc(BG).
known_grid0(ID,G):- compound(ID),ID=(_>(Trn+Num)*IO),!,fix_test_name(ID,Name,Trn+Num),!,(kaggle_arc_io(Name,Trn+Num,IO,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- compound(ID),ID=(_>_),fix_test_name(ID,Name,ExampleNum),!,(kaggle_arc_io(Name,ExampleNum,_IO,G),deterministic(YN),true), (YN==true-> ! ; true).
%known_grid0(ID,G):- (is_shared_saved(ID,G),deterministic(YN),true), (YN==true-> ! ; true).
%known_grid0(ID,G):- (is_unshared_saved(ID,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- (atom(ID);string(ID)),notrace(catch(atom_to_term(ID,Term,_),_,fail)), Term\==ID,!,known_grid0(Term,G).




addProgramStep(_VM,Step):-
  pp(addProgramStep(vm,Step)).

kaggle_arc_io(Name,ExampleNum,IO,G):- 
  arg(_,v(trn+_,tst+_),ExampleNum),
  kaggle_arc(Name,ExampleNum,In,Out), ((IO=in,G=In);(IO=out,G=Out)).

into_gridnameA(G,Name):- known_grid(Name,G).


%grid_to_tid(Grid,Name):- is_grid_tid(Grid,Name)*->true; (plain_var(Name)->(luser_getval(grid_name,Name),Name\=[],grid_to_tid(Grid,Name))).
:- dynamic(is_grid_tid/2).
set_grid_tid(Grid,ID):-
  my_assertion((ground(ID),nonvar_or_ci(Grid))),
  my_assertion(\+ is_grid(ID)),
  luser_setval(grid_name,ID),
  ignore(( \+ into_gridnameA(Grid,ID),
  copy_term(Grid,GGrid),numbervars(GGrid,1,_),
  asserta(is_grid_tid(GGrid,ID)))).

to_assertable_grid(A,A):- ground(A),!.
%to_assertable_grid(A,C):- copy_term(A,B),numbervars(B,0,_,[attvar(skip),singletons(true)]),B\==A,to_assertable_grid(B,C).
to_assertable_grid(A,B):- 
  term_attvars(A,V),maplist(get_attrs,V,ATTS),term_attvars(A+ATTS,V2),maplist(get_attrs,V2,ATTS2),
  copy_term(A+V2+ATTS2,B+VV2+VATTS2,_),
  maplist(save_atts,VATTS2,VV2),numbervars(B+VV2+VATTS2,0,_,[attvar(skip),singletons(true)]).

save_atts(A,'$attrs'(Attrs)):- attvar(A),!,get_attrs(A,Attrs).
save_atts(A,'$attrs'(A)). 

to_assertable(A,A):- ground(A),!.
to_assertable(A,'$VAR'(N)):- plain_var(A),!,format(atom(N),'~q',[A]).

grid_to_tid(Grid,ID):- var(Grid),!,known_gridoid(ID,Grid).
grid_to_tid(obj(_),_):- !,fail.
%grid_to_tid(Grid,ID):- atom(Grid),!,ID=Grid.
grid_to_tid(Grid,ID):- \+ ground(Grid), to_assertable_grid(Grid,GGrid),!,grid_to_tid(GGrid,ID).
grid_to_tid(Grid,ID):- kaggle_arc_io(TestID,Trn+Num,IO,Grid),!,ID = (TestID>(Trn+Num)*IO).
grid_to_tid(Grid,ID):- known_grid0(ID,GVar),Grid=@=GVar,!.
grid_to_tid(Grid,ID):- must_be_free(ID),makeup_gridname(Grid,ID), set_grid_tid(Grid,ID),!.

into_oid(X,ID):- atom(X),!,X=ID.
into_oid(X,ID):- is_grid(X),grid_to_gid(X,ID),!.
into_oid(X,ID):- is_object(X),obj_to_oid(X,ID),!.
into_oid(X,ID):- tid_to_gids(X,ID),!.

grid_to_gid(Grid,OID):- is_grid_tid(Grid,OID),atom(OID),!.
grid_to_gid(Grid,OID):- grid_to_tid(Grid,ID),!,(clause(tid_to_gids(ID,OID),true)*-> true ; term_to_oid(ID,OID)).

makeup_gridname(_Grid,GridName):- get_current_test(ID),flag(made_up_grid,F,F+1),GridName = (ID>('ExampleNum'+F)*io).

incomplete(X,X).

into_obj(G,O):- is_object(G),!,G=O.
into_obj(G,O):- is_grid(G),!,individuate(whole,G,Objs),last(Objs,O),!.
into_obj(G,O):- no_repeats(O,known_obj0(G,O))*->true; (into_grid(G,GG),!,into_obj(GG,O)),!.

  %set(VM.points)=[],!.


:- module_transparent register_obj/1.
%register_obj(O):- quietly((wots(S,weto(arcST)), asserta(obj_cache(TestID,O,S)))),!.
register_obj(O):-  must_det_ll(o2g(O,_)),!.
/*register_obj(L):- asserta(obj_cache(TestID,L,'')),
  ignore(( false, O=obj(L),amass(O,Mass),Mass>7,format('~N'),arc_portray(O,false),nl)).
*/
:- dynamic(obj_cache/3).
:- module_transparent obj_cache/2.

:- dynamic(oid_glyph_object/3).

o2g(Obj,Glyph):- var(Obj),!,gid_glyph_oid(_,Glyph,OID),oid_glyph_object(OID,Glyph,Obj).
%o2g(Obj,Glyph):-  g2o(Glyph,Obj),!.
o2g(Obj,NewGlyph):- var(NewGlyph),must_det_ll((o2g_f(Obj,NewGlyph))),!. 
o2g(Obj,NewGlyph):- trace,o2g_f(Obj,NewGlyph).

/*
 obj_to _oid(Obj,Old), int2glyph(Old,Glyph), 
 (g2o(Glyph,O2) ->
       (O2=@=Obj->NewGlyph=Glyph; 
         must_det_ll(( 
           flag(indiv,Iv,Iv+1),
           int2glyph(Iv,NewGlyph),!,           
           subst001(Obj,obj_to_oid(ID,Old),obj_to_oid(ID,Iv),NewObj),
           (number(NewGlyph)->trace;true),
           set_glyph_to_object(NewGlyph,NewObj))))
  ; ((number(NewGlyph)->trace;true),NewGlyph=Glyph,(number(NewGlyph)->trace;true),set_glyph_to_object(NewGlyph,Obj))),
 set_glyph_to_object(NewGlyph,Obj).
*/

o2c(Obj,Glyph):- color(Obj,Glyph),!.

o2ansi(I,S):- integer(I),int2glyph(I,G),!,o2ansi(G,S). 
o2ansi(G,S):- atom(G),!,g2o(G,O),o2ansi(O,S),!.
o2ansi(Obj,S):- o2g(Obj,G),colors(Obj,Colors),maplist(arg(1),Colors,NColors),
  wots(S,maplist(user:print_ncolors(G),NColors)).
print_ncolors(G,C):- sformat(F,'~q',[G]),color_print(C,F).

:- system:import(print_ncolors/2).

:- dynamic(g_2_o/3).
g_2_o(_,_,_):- fail.

%set_glyph_to_object(G,O):- ignore(luser_linkval(G,O)),(get_current_test(TestID),my_asserta_if_new(g_2_o(TestID,G,O))).

g2o(G,O):- var(G), !, oid_glyph_object(_,G,O).
g2o(G,O):- integer(G),int2glyph(G,C),!,g2o(C,O),!.
g2o(C,O):- compound(C), !, compound_name_arguments(C,objFn,[G|_]), !, g2o(G,O).
g2o(G,O):- \+ atom(G), !, string(G),Chars=[_|_],atom_chars(G,Chars),!,chars2o(Chars,O).
g2o(G,O):- oid_to_object(G,O)-> true;(oid_glyph_object(_,G,O)*->true;(Chars=[_,_|_],atom_chars(G,Chars),chars2o(Chars,O))).



%get_glyph_to_object(G,O):- ((luser_getval(G,O),is_object(O))*->true;(get_current_test(TestID),g_2_o(TestID,G,O))).

chars2o(['o',C,'_'|_],O):- !, g2o(C,O).
chars2o(Chars,O):- \+ member('_',Chars), member(C,Chars),g2o(C,O),!.


known_object(G,O):- known_obj0(G,O).

known_obj0(G,_):- G==[],!,fail.
known_obj0(G,E):- plain_var(G),!,enum_object(E),G=E.
known_obj0(G,O):- is_object(G),!,G=O.
known_obj0(obj(O),obj(O)):- !, is_list(O),!.
known_obj0(G,O):- g2o(G,O),!.
known_obj0(G,O):- is_grid(G),!,grid_to_individual(G,O).
known_obj0(G,O):- is_group(G),into_group(G,OL),OL=[_],must([O|_]=OL).

% this is bad  ?- into_grid('E',ID),grid_to_tid(G,ID).  ?- into_grid('Z',ID),grid_to_tid(G,ID).

into_group(GI,G):- into_group(GI,G, _ ).

into_group(G,G,(=)) :- G==[],!.
into_group(P,G,(=)):- is_group(P),!,G=P.
into_group(G, G, _):- plain_var(G),!, %throw(var_into_group(G)),
   (why_grouped(_Why, G)*->true; 
     ((arc_grid_pair(In,Out),individuate_pair(complete,In,Out,InC,OutC),append(InC,OutC,Objs)),
      (why_grouped(_Why, G)*->true; G=Objs))).
into_group(VM,G,(group_to_and_from_vm(VM))):- is_vm(VM),G=VM.objs,is_group(G),!.
into_group(VM,G,(group_to_and_from_vm(VM))):- is_vm(VM),run_fti(VM),G=VM.objs,is_group(G),!.
into_group(G,I, into_grid):- is_grid(G),!,compute_shared_indivs(G,I).
into_group(P,G, into_obj):- is_object(P),!,G=[P].
into_group(P,G, lambda_rev(why_grouped)):- known_gridoid(P,M),!,into_group(M,G).
into_group(P,G, _):- arcST,throw(into_group(P,G)).
/*
into_group(P,G):- is_object(P),points_to_grid(P,M),!,into_group(M,G).
%into_group(G,G):- is_grid(G),!.

into_group(P,G):- is_group(P),set_grid_nums(P),
  mapgroup(into_group,P,Gs),!,combine_grids(overlay,Gs,G).
into_group(P,G):-
  mapgroup(into_group,P,Gs),!, 
  set_grid_nums(Gs), arg(1,Gs,G).
into_group(P,G):- mapgroup(into_group,P,Gs),!, set_grid_nums(Gs), combine_grids(overlay,Gs,G).
*/


gather_object(Obj1,Var,Expression,Grid,Grid):-
  create_bag(Obj1),
  forall(Expression,ain(part_of(Var,Obj1))).

gather_object(Obj1,Var,Expression,Grid,Grid):-
  create_bag(Obj1),ain(iz(Obj1,group)),
  forall(Expression,ain(part_of(Var,Obj1))).


wall_thickness(X,N):- iz(X,polygon),calc(wall_thickness(X,N)).

calc(_).

create_bag(Obj1):- gensym(bag_,Obj1),ain(iz(Obj1,group)).


missing_arity(P2,N):- compound(P2),!,compound_name_arity(P2,F,Am2),A is Am2 + N, \+ current_predicate(F/A).
missing_arity(F,N):- \+ current_predicate(F/N).
% turtle(H,V,Dir,N,H2,V2):- 
prim_ops([
  call_object_grid_size(obj),
  trim_grid_to_size(point,visual_hw),
  fill_from_point(point,color),
  create_a_ray(point,dir,len),
  object_as_own_grid(obj,gridOps),
  copy_one_object(obj,point),
  rotate_one_object(obj,nsew),
  flatten_one_object(obj),
  sort_by_gravity(nsew),
  flip_grid(hOrv),
  rotate_grid(nsew)]).


throw_missed(G):-  Info = missed(G),wdmsg(Info),break, arcST,throw_missed_pt2(G,Info).
throw_missed_pt2(_,Info):- tracing,!,throw(Info).
throw_missed_pt2(G,Info):- notrace,nortrace,trace,wdmsg(Info),break,rrtrace(G),throw(Info).



% make or do plan
do_change(Change,Grid1,Grid2):- \+ is_list(Change),!,one_change(Change,Grid1,Grid2).
do_change(Change,Grid1,Grid2):- do_change_nd(Change,Grid1,Grid2).

do_change_nd([],Grid1,Grid1).
do_change_nd([H|T],Grid1,Grid2):- one_change(H,Grid1,GridM),do_change_nd(T,GridM,Grid2).

one_change(sameR,Grid1,Grid2):- is_grid(Grid2),Grid1=Grid2,!.
one_change(colorChange(C1,C2),Grid1,Grid2):- 
  first_color(Grid1,C1),ignore((is_grid(Grid2),first_color(Grid2,C2))),
  subst001(Grid1,C1,C2,Grid2).
one_change(blank1Color(C1),Grid1,Grid2):- 
  first_color(Grid1,C1),copy_cells(==(C1),free_cell,Grid1,Grid2).
one_change(same_size,Grid1,Grid2):- plain_var(Grid2),grid_size(Grid1,H,V),grid_size(Grid2,H,V),!.


/*
nth_fact(P, I):- clause(P, true, Ref), nth_clause(P, I, Ref).


% make or do plan
do_change(Change, Grid1, Grid2):- \+ is_list(Change), !, one_change(Change, Grid1, Grid2).
do_change(Change, Grid1, Grid2):- do_change_nd(Change, Grid1, Grid2).

do_change_nd([], Grid1, Grid1).
do_change_nd([H|T], Grid1, Grid2):- one_change(H, Grid1, GridM), do_change_nd(T, GridM, Grid2).

one_change(sameR, Grid1, Grid2):- is_grid(Grid2), Grid1=Grid2, !.
one_change(colorChange(C1, C2), Grid1, Grid2):- 
 first_color(Grid1, C1), ignore((is_grid(Grid2), first_color(Grid2, C2))), subst001(Grid1, C1, C2, Grid2).
one_change(blank1Color(C1), Grid1, Grid2):- 
 first_color(Grid1, C1), copy_cells(==(C1), free_cell, Grid1, Grid2).
one_change(same_size, Grid1, Grid2):- plain_var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C1), !.
one_change(resize(C1, C2), Grid1, Grid2):- plain_var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C2).

*/

arc_expand_arg(objFn(X,_),Var,known_object(X,Var)).
arc_expand_arg(gridFn(X),Var,known_grid(X,Var)).
arc_expand_arg(groupFn(X),Var,into_group(X,Var)).

goal_expansion_query(Goal,Out):- fail,
   compound(Goal), predicate_property(Goal,meta_predicate(_)),!,
   arg(N,Goal,P), compound(P), goal_expansion_query(P,MOut), 
   MOut\=@=P, setarg(N,Goal,MOut), expand_goal(Goal, Out).

goal_expansion_query(Goal,Out):- compound(Goal),
   get_setarg_p1(setarg,I,Goal,P1), compound(I), arc_expand_arg(I,Var,Exp),
   call(P1,Var),expand_goal((Exp,Goal),Out).


goal_expansion_q(Goal,I,Out,O):-  var(I), \+ source_location(_,_),luser_getval('$goal', Term),% writeq(Term=@=Goal),nl,
  Goal=@=Term,
  (goal_expansion_query(Goal,Out)-> Goal\=@=Out),I=O.

:- export(thread_httpd:http_process/4).
:- system:import(thread_httpd:http_process/4).

:- fixup_exports.

:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).
%goal_expansion(Goal,I,Out,O):- goal_expansion_q(Goal,I,Out,O).

% ?- print_grid(gridFn(X)).
%:- export(is_toplevel_query/2).
%:- b_setval('$goal', []).
:- luser_linkval('$goal', []).
%:- b_setval('$goal_expanded', []).
:- luser_linkval('$goal_expanded', []).
expand_query(Goal, Expanded, Bindings, ExpandedBindings):- current_predicate(luser_linkval/2),
    % Have vars to expand and varnames are empty
    luser_getval('$goal', WGoal), WGoal\=@=Goal, % this prevents the loop
    luser_linkval('$goal', Goal),
    quietly((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])), % this prevents the loop
    luser_linkval('$variable_names', Bindings),
    debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    writeq(Goal+Bindings),nl,
    expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    luser_linkval('$goal_expanded', Expanded).    


