/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).


:- meta_predicate(grid_call(+,+,-)).

must_grid_call(T,I,O):- (grid_call(T,I,O)*->true; (print_side_by_side_msg(failed_grid_call(T),I,O),trace,fail)).
 
gref_call(P1,In,Out):- 
  duplicate_term(in(In),IIn),
  call(P1,IIn), 
  dref_grid(IIn,Out).
dref_grid(IIn,Grid):- is_grid(IIn),!,Grid=IIn.
dref_grid(IIn,Grid):- arg(_,IIn,Grid),is_grid(Grid),!.

grid_call(T,I,O):- plain_var(I),var(O),!,into_grid(_,G),G\=@=I,I=G,grid_call(T,G,O).
grid_call(=,I,O):- !, I=O. 
grid_call(P2,IO,IIOO):- is_plus_split(IO,I,O),!,unplus_split(IIOO,II,OO),grid_call(P2,I,II),grid_call(P2,O,OO).
grid_call(Nil,I,I):- Nil==[],!. 
grid_call([H|T],I,O):- nonvar(H), !, grid_call(H,I,M), grid_call(T,M,O).

grid_call(P2,IG,IIOO):- is_grid_group(IG),!, grid_group_call(P2,IG,IIOO).
%grid_call(T,I,O):- into_p2(T,I,O,P),check_args(P,PP),call(PP).
grid_call(T,I,O):- call(T,I,O).



into_p2(P2,I,O,PIO):- atom(P2),!,PIO=..[P2,I,O].
into_p2(P2,I,O,PIO):- P2=..FArgs,append(FArgs,[I,O],FArgsIO),!,PIO=..FArgsIO.


grid_group_call(P2,IG,IIOO):- findall(O,(member(I,IG), object_call(P2,I,O)),List),List\==[],list_to_set(List,IIOO).

:- meta_predicate(object_call(+,+,-)).
object_call(=,I,O):- !, I=O. 


object_call(Nil,I,I):- Nil==[],!. 
object_call(P2,IO,IIOO):- is_plus_split(IO,I,O),!,unplus_split(IIOO,II,OO), object_call(P2,I,II),object_call(P2,O,OO).
object_call([H|T],I,O):- nonvar(H), !, object_call(H,I,M), object_call(T,M,O).
object_call(T,I,O):- call(T,I,O).

:- meta_predicate(grid_call_alters(+,+,-)).
grid_call_alters([H|T],I,O):- !, grid_call_alters(H,I,M),grid_call_alters(T,M,O).
grid_call_alters(T,I,O):- grid_call(T,I,O),I\=@=O.

:- meta_predicate(try_p2(+,+,-)).
try_p2(P2,In,Out):- grid_call(P2,In,Mid),Mid=@=Out.

is_plus_split(IO,I,O):- compound(IO),unplus_split(IO,I,O).
unplus_split(II+OO,II,OO).

show_grid_call(P2,IO,IIIOOO):- is_plus_split(IO,I,O),!,unplus_split(IIIOOO,III,OOO),
 must_det_ll((grid_to_gid(I,GIDI),grid_to_gid(O,GIDO),
  copy_term(P2,P22),
  grid_call_for_info(P2,I,III,S1),
  once(((grid_call_for_info(P2,O,OOO,S2),Aligned=true)*-> true;
       ((P2\=@=P22,grid_call_for_info(P22,O,OOO,S2),Aligned=copied)*-> true;
       ((O=OOO,tersify(P22,S2),Aligned=false))))),  
  if_t(((III+OOO)\=@=(I+O)), 
     print_side_by_side(green,III,called(S1,left,from(GIDI)),_,OOO,aligned(Aligned,S2,right,from(GIDO)))))),!.

show_grid_call(P2,I,III):-
  grid_call_for_info(P2,I,III,S1),
   if_t(((III)\=@=(I)), print_side_by_side(green,I,before(S1),_,III,after(S1))),!.

grid_call_for_info(P2,I,III,S1):- once(grid_call(P2,I,II)),(grid_footer(II,III,F)-> tersify(s(F,P2),S1) ; (III=II,tersify(P2,S1))),!.
grid_call_for_info(P2,I,III,S1):- II=I,(grid_footer(II,III,F)-> tersify(s(F,P2),S1) ; (III=II,tersify(P2,S1))),!.



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

vert_pos(Class,Y):- into_singles(Class,Obj),loc2D(Obj,_X,Y).
horiz_pos(Class,X):- into_singles(Class,Obj),loc2D(Obj,X,_Y).

when_config(This,Goal):-test_config(This)-> call(Goal) ; true.
test_config(This):- once(test_info(_,_)), get_current_test(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

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
  vis2D(Grid,H,V), gset(VM.h)=H, gset(VM.v)=V, 
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

run_dsl(VM,enforce,vert_pos(Obj,New),In,Out):-!, loc2D(Obj,X,_Old), override_object_io(VM,loc2D(X,New),Obj,In,Out).

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
:- include(pfc_3_0/pfc_3_0_0).
%:- use_module(library(pfc_lib)).
:- endif.

:- decl_pt(into_grids(+(prefer_grid),-mv(grid))).
into_grids(P,G):- no_repeats(G,quietly(cast_to_grid(P,G, _))).

:- decl_pt(into_grid(+(prefer_grid),-grid)).
into_grid(P,G):- var(P),!,ignore(get_current_test(TestID)),test_grids(TestID,G),grid_to_tid(G,P).
into_grid(A+B,AA+BB):- nonvar(A), !, cast_to_grid(A,AA, _),cast_to_grid(B,BB, _).
into_grid(P,G):- cast_to_grid(P,G, _).

map_to_grid(objs,Dict,Obj,Grid,Closure):- get_kov(objs,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.
map_to_grid(grid,Dict,Grid,Obj,Closure):- get_kov(grid,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.
map_to_grid(points,Dict,Obj,Grid,Closure):- get_kov(points,Dict,Obj), Obj\=[], cast_to_grid(Obj,Grid,Closure),!.

print_grid_to_string(G,S):- with_output_to(string(S),print_grid(G)).
print_grid_to_atom(G,S):- with_output_to(atom(S),print_grid(G)).
% ?- print_grid(gridFn(X)).
cast_to_grid(P,G, Cvt):- attvar(P),get_attr(P,expect_p2,O),!,cast_to_grid(O,G, Cvt).
cast_to_grid(P,G, =):- var(P),!,ignore(get_current_test(TestID)),test_grids(TestID,G),grid_to_tid(G,P).
cast_to_grid(Grid,Grid, (=) ):- is_grid(Grid),!.
cast_to_grid(gridOpFn(Grid,OP),GridO,reduce_grid):- !, unreduce_grid(Grid,OP,GridO).
cast_to_grid(Points,Grid,globalpoints):- is_points_list(Points), !, points_to_grid(Points,Grid),!.
cast_to_grid(Obj,Grid, uncast_grid_to_object(Obj)):- is_object(Obj),!, object_grid(Obj,Grid),!.
cast_to_grid(Grp,Grid, closure_grid_to_group(Grp)):- is_group(Grp), group_to_grid(Grp,Grid),!.

cast_to_grid(Text,Grid, print_grid_to_string ):- string(Text),text_to_grid(Text,Grid),!.
cast_to_grid(Text,Grid, print_grid_to_atom ):- atom(Text),atom_length(Text,Len),Len>20,atom_contains(Text,'|'),text_to_grid(Text,Grid),!.

% TODO Comment out next line to prefer the line after
% cast_to_grid(Dict,Grid, (=) ):- is_map(Dict), get_kov(grid,Dict,Grid),!.
cast_to_grid(Dict,Grid, back_to_map(Was,Dict,Prev,Grid,Closure)):- is_map(Dict), map_to_grid(Was,Dict,Prev,Grid,Closure),!.

cast_to_grid(Obj,Grid, Closure):- cast_to_grid1(Obj,Grid, Closure).

%cast_to_grid1(OID, Grid, Uncast):- atom(OID),g2o(OID,Obj),Obj\==OID,!,cast_to_grid(Obj,Grid,Uncast).
cast_to_grid1(TestID>(Tst+N)*IO,Grid,(=)):- !, kaggle_arc_io(TestID,(Tst+N),IO,Grid).
cast_to_grid1(Obj,Grid, Closure):- resolve_reference(Obj,Var), Obj\=@=Var, !,cast_to_grid(Var,Grid,Closure).
cast_to_grid1(OID, Grid, (=) ):-  atom(OID),oid_to_gridoid(OID,Grid),!.
cast_to_grid1(Naming,Grid, Closure ):- 
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


group_to_grid(Grp,Grid):-   
  reproduction_objs(Grp,Objs),
  grid_size(Objs,H,V),
  maplist(globalpoints,Objs,Points),
  append(Points,AllPoints),
  points_to_grid(H,V,AllPoints,Grid),!.

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

called_gid(Suffix,P2,Color,MGrid):- was_grid_gid(Color,OID),
  (atom_concat(_,Suffix,OID) -> MGrid=Color ; (atom_concat(OID,Suffix,MOID),called_gid3(P2,MOID,Color,MGrid))),!.
called_gid(_Suffix,P2,Color,MGrid):- call(P2,Color,MGrid),!.
called_gid3(_P2,MOID,_Color,MGrid):- was_grid_gid(MGrid,MOID),!.
called_gid3(P2,MOID,Color,MGrid):- call(P2,Color,MGrid),assert_grid_gid(MGrid,MOID).

grid_to_gid(Grid,OID):- was_grid_gid(Grid,OID),!.
grid_to_gid(Grid,OID):- grid_to_etid(Grid,ID),!,(clause(tid_to_gids(ID,OID),true)*-> true ; term_to_oid(ID,OID)).
grid_to_gid(Grid,OID):- grid_to_tid(Grid,ID),!,(clause(tid_to_gids(ID,OID),true)*-> true ; term_to_oid(ID,OID)).
% be03b35f

was_grid_gid(G,GID):- current_predicate(gid_to_grid/2), call(call,gid_to_grid,GID,G),assertion(atom(GID)).
was_grid_gid(Grid,GID):- atom(GID),oid_to_gridoid(GID,G),into_grid(G,GG),Grid=GG,assertz_if_new(gid_to_grid(GID,Grid)).
was_grid_gid(Grid,GID):- is_grid_tid(Grid,GID),atom(GID),assertz_if_new(gid_to_grid(GID,Grid)).
assert_grid_tid(Grid,GID):- asserta_new(is_grid_tid(Grid,GID)),ignore((atom(GID),asserta_new(gid_to_grid(GID,Grid)))).
assert_grid_gid(Grid,GID):- assertz_if_new(is_grid_tid(Grid,GID)),ignore((atom(GID),assertz_if_new(gid_to_grid(GID,Grid)))).

oid_to_gridoid(GID,G):- current_predicate(gid_to_grid/2), call(call,gid_to_grid,GID,G),!.

oid_to_gridoid(ID,G):-  atom(ID),atomic_list_concat(Term,'_',ID), append([o,_,_],GOID,Term),
  testid_name_num_io(GOID,_Name,_Example,_Num,_IO),g2o(ID,G),!.
/*
oid_to_gridoid(ID,G):-  atom(ID),atomic_list_concat(Term,'_',ID), append([o,GGL,OID],GOID,Term),
  testid_name_num_io(GOID,_Name,_Example,_Num,_IO),!,
  ((atom(OID),atom_number(OID,ONum))-> int2glyph(ONum,GL);GL=GGL),
  get_current_test(TestID),
  g_2_o(TestID,GL,G).*/
oid_to_gridoid(ID,G):- atom(ID),
  testid_name_num_io(ID,Name,Example,Num,IO),atom(IO),
  kaggle_arc_io(Name,Example+Num,IO,G),!.



known_grid0(ID,_):- var(ID),!,fail.
known_grid0(ID,G):- is_grid(ID),!,G=ID.
known_grid0(ID,_):- is_list(ID),!,fail.
known_grid0(OID, Grid):- atom(OID),oid_to_gridoid(OID,Grid),!.
known_grid0(ID,_):- is_object(ID),!,fail.
known_grid0(_,ID):- is_object(ID),!,fail.
known_grid0(ID,G):- true,testid_name_num_io(ID,TestID,Trn,Num,IO),ExampleNum=Trn+Num,!,(kaggle_arc_io(TestID,ExampleNum,IO,G),deterministic(YN),true),(YN==true-> ! ; true).
known_grid0(ID,G):- is_grid_tid(G,ID),!.
known_grid0(ID,G):- was_grid_gid(G,ID),!.
known_grid0(ID,G):- true,fix_test_name(ID,Name,ExampleNum),!,
  (kaggle_arc_io(Name,ExampleNum,_IO,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- learned_color_inner_shape(ID,magenta,BG,G,_),get_bgc(BG).
known_grid0(ID,G):- compound(ID),ID=(_>(Trn+Num)*IO),!,fix_test_name(ID,Name,Trn+Num),!,(kaggle_arc_io(Name,Trn+Num,IO,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- compound(ID),ID=(_>_),fix_test_name(ID,Name,ExampleNum),!,(kaggle_arc_io(Name,ExampleNum,_IO,G),deterministic(YN),true), (YN==true-> ! ; true).
%known_grid0(ID,G):- (is_shared_saved(ID,G),deterministic(YN),true), (YN==true-> ! ; true).
%known_grid0(ID,G):- (is_unshared_saved(ID,G),deterministic(YN),true), (YN==true-> ! ; true).
known_grid0(ID,G):- (atom(ID);string(ID)),notrace(catch(atom_to_term(ID,Term,_),_,fail)), Term\==ID,!,known_grid0(Term,G).


:- dynamic(kaggle_arc_answers/4).

addProgramStep(_VM,Step):-
  pp(addProgramStep(vm,Step)).

kaggle_arc_io(Name,ExampleNum,IO,G):- 
  arg(_,v(trn+_,tst+_),ExampleNum),
  kaggle_arc(Name,ExampleNum,In,Out), ((IO=in,G=In);(IO=out,G=Out)).
%kaggle_arc_io(Name,tst+ID,out,Grid):- kaggle_arc_answers(Name,ID,ID,Grid).


into_gridnameA(G,Name):- known_grid(Name,G).


%grid_to_tid(Grid,Name):- is_grid_tid(Grid,Name)*->true; (plain_var(Name)->(luser_getval(grid_name,Name),Name\=[],grid_to_tid(Grid,Name))).
:- dynamic(is_grid_tid/2).
:- dynamic(is_grid_gid/2).
set_grid_tid(Grid,ID):-
  my_assertion((ground(ID),nonvar_or_ci(Grid))),
  my_assertion(\+ is_grid(ID)),
  luser_setval(grid_name,ID),
  ignore(( \+ into_gridnameA(Grid,ID),
  copy_term(Grid,GGrid),numbervars(GGrid,1,_),
  assert_grid_tid(GGrid,ID))).

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

grid_to_tid(Grid,TID):- var(Grid),!,known_gridoid(TID,Grid).
grid_to_tid(obj(_),_):- !,fail.
%grid_to_tid(Grid,TID):- atom(Grid),!,TID=Grid.
grid_to_tid(Grid,TID):- nonvar(TID),!,grid_to_tid(Grid,TID),must_det_ll(TID=TID).
grid_to_tid(Grid,TID):- \+ ground(Grid), to_assertable_grid(Grid,GGrid),!,grid_to_tid(GGrid,TID).
grid_to_tid(Grid,TID):- grid_to_etid(Grid,TID),!.
grid_to_tid(Grid,TID):- must_be_free(TID),makeup_gridname(Grid,TID), set_grid_tid(Grid,TID),!.

grid_to_etid(Grid,_ID):- assertion(nonvar(Grid)),fail.
grid_to_etid(Grid,TID):- is_grid_tid(Grid,TID),!.
grid_to_etid(Grid,TID):- get_current_test(TestID), kaggle_arc_io(TestID,Trn+Num,IO,Grid), name_num_io_id(TestID,Trn,Num,IO,TID),!.
grid_to_etid(Grid,TID):- kaggle_arc_io(TestID,Trn+Num,IO,Grid), name_num_io_id(TestID,Trn,Num,IO,TID),!.
%grid_to_etid(Grid,TID):- was_grid_gid(Grid,TID),!.
grid_to_etid(Grid,TID):- known_grid0(TID,GVar),Grid=@=GVar,!.

name_num_io_id(TestID,Trn,Num,IO,ID):- must_det_ll((ID = (TestID>(Trn+Num)*IO))).

into_oid(X,ID):- atom(X),!,X=ID.
into_oid(X,ID):- is_grid(X),grid_to_gid(X,ID),!.
into_oid(X,ID):- is_object(X),obj_to_oid(X,ID),!.
into_oid(X,ID):- tid_to_gids(X,ID),!.


makeup_gridname(Grid,TID):- nonvar(TID),!,makeup_gridname(Grid,GridNameM),!,must_det_ll((GridNameM=TID)).
makeup_gridname(Grid,TID):- get_current_test(TestID), kaggle_arc_io(TestID,Trn+Num,IO,Grid), name_num_io_id(TestID,Trn,Num,IO,TID),!.
makeup_gridname(Grid,TID):- is_grid_tid(Grid,TID),!.
%makeup_gridname(Grid,TID):- was_grid_gid(Grid,TID),!.
makeup_gridname(Grid,TID):- get_current_test(TestID),
  flag(made_up_grid,F,F+1),
   get_example_num(Example+Num),
   (ground(Example+Num)->atomic_list_concat([Example,Num,ex],'_',HH);HH= 'Example'),
   name_num_io_id(TestID,HH,F,io,TID),
   assert_grid_tid(Grid,TID), nop(dumpST), 
    %nop
    (print_grid(no_name(TestID,TID),Grid)).

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

:- system:import(print_ncolors/2).

:- dynamic(g_2_o/3).
g_2_o(_,_,_):- fail.

%set_glyph_to_object(G,O):- ignore(luser_linkval(G,O)),(get_current_test(TestID),my_asserta_if_new(g_2_o(TestID,G,O))).

g2o(G,O):- var(G), !, oid_glyph_object(_,G,O).
g2o(G,O):- integer(G),!,int2glyph(G,C),!,g2o(C,O),!.
g2o(C,O):- compound(C), !, compound_name_arguments(C,objFn,[G|_]), !, g2o(G,O).
g2o(G,O):- \+ atom(G), !, string(G),!,atom_string(A,G),!,g2o(A,O).
g2o(G,_):- is_fg_color(G),!,fail.
g2o(G,O):- oid_to_object(G,O)-> true;(oid_glyph_object(_,G,O)*->true;(Chars=[_,_|_],atom_chars(G,Chars),chars2o(Chars,O))).



%get_glyph_to_object(G,O):- ((luser_getval(G,O),is_object(O))*->true;(get_current_test(TestID),g_2_o(TestID,G,O))).

chars2o(['o','_',C|_],O):- g2o(C,O),!.
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
     (arc_grid_pair(In,Out),individuate_pair(complete,In,Out,InC,OutC),append(InC,OutC,Objs),
       % tries again
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
arc_expand_arg(objFn(X),Var,known_object(X,Var)).
arc_expand_arg(gridFn(X),Var,known_grid(X,Var)).
arc_expand_arg(groupFn(X),Var,into_group(X,Var)).
arc_expand_arg(Atom,Var,true):- atom(Atom), arc_expand_atom(Atom,Var), Var\=@=Atom.

arc_expand_atom(X,_):- \+ atom(X),!,fail.
arc_expand_atom(X,Var):- \+ atom_concat('o_',_,X), known_grid(X,Var),!.
arc_expand_atom(X,Var):- known_object(X,Var),!.

goal_expansion_query(Goal,Out):- fail,
   compound(Goal), predicate_property(Goal,meta_predicate(_)),!,
   arg(N,Goal,P), compound(P), goal_expansion_query(P,MOut), 
   MOut\=@=P, setarg(N,Goal,MOut), expand_goal(Goal, Out).

goal_expansion_query(Goal,Out):- compound(Goal),
   get_setarg_p1(setarg,I,Goal,P1), compound(I), arc_expand_arg(I,Var,Exp),
   call(P1,Var),expand_goal((Exp,Goal),Out).

is_goal_query(Goal):- 
  \+ source_location(_,_),luser_getval('$goal', Term), !, % writeq(Term=@=Goal),nl,
  Goal=@=Term.

goal_expansion_q(Goal,I,Out,O):- var(I), is_goal_query(Goal), (goal_expansion_query(Goal,Out)-> Goal\=@=Out),I=O.

:- export(thread_httpd:http_process/4).
:- system:import(thread_httpd:http_process/4).

:- include(kaggle_arc_footer).

:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).
goal_expansion(Goal,I,Out,O):- 
   nb_current(arc_can_expand_query,t),
   \+ current_prolog_flag(arc_term_expansion,false),
   current_predicate(goal_expansion_q/4),
   goal_expansion_q(Goal,I,Out,O).

% ?- print_grid(gridFn(X)).
%:- export(is_toplevel_query/2).
%:- b_setval('$goal', []).
:- luser_linkval('$goal', []).
%:- b_setval('$goal_expanded', []).
:- luser_linkval('$goal_expanded', []).
expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
  current_predicate(luser_linkval/2),
    % Have vars to expand and varnames are empty
    luser_getval('$goal', WGoal), WGoal\=@=Goal, % this prevents the loop
    luser_linkval('$goal', Goal),
    quietly((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])), % this prevents the loop
    luser_linkval('$variable_names', Bindings),
    debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    writeq(Goal+Bindings),nl,
    expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    luser_linkval('$goal_expanded', Expanded).    


