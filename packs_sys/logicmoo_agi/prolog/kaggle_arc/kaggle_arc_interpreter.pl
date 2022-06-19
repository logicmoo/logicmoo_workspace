/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


decl_pt(_):- fail.

check_args(P,MC):- functor(P,F,A),functor(T,F,A),functor(C,F,A),decl_pt(T),check_args(P,1,A,A,T,C,MC).
check_args(P,P):- !. 

check_args(P,Arity,Arity,1,T,C,MC):- !,
 call(C),arg(An,T,ArgType),arg(An,C,Result),
 MC = t,
 arg(An,P,Return),into_type(ArgType,Result,Return).
check_args(P,Arity,An,2,T,C,MC):-
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),
 is_group(ArgIn), ArgType = object,!,
 arg(Arity,P,Result),arg(Arity,C,Return),
 findall(Return,(member(CallArg,ArgIn),check_args(P,Arity,Arity,1,T,C,MC)),Result).
check_args(P,Arity,An,Left,T,C,MC):-  
 arg(An,P,ArgIn),arg(An,T,ArgType),arg(An,C,CallArg),into_type(ArgType,ArgIn,CallArg),
 AnM1 is An+1,LeftP1 is Left-1, check_args(P,Arity,AnM1,LeftP1,T,C,MC).

into_type(Type,G,O):- nonvar_or_ci(O),!,into_type(Type,G,M),!,M=O.
into_type(Type,G,O):- plain_var(G),throw(var_into_type(Type,G)),O=fake(Type,G).
into_type(+,X,X).
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
show_workflow(In,String,Out):- nonvar(Out),!,trace,must_det_ll((show_workflow(In,String,OutM),Out=OutM)).
show_workflow(InO,String,InO):- string(String),!, 
 ignore((InO\==[], nl, writeln(String), forall(member_or_it(G,InO),ignore(print_grid(_,_,String,G))))).
show_workflow(InO,[],InO):-!.
show_workflow(In,[H|T],Out):-
  show_workflow(In,H,Mid),!,
  show_workflow(Mid,T,Out).
show_workflow(In,add(P),Out):- !,
  show_workflow(In,P,Mid),!,
  my_append(Mid,In,Out).
show_workflow(In,each(P),Out):- 
  show_workflow_each(In,P,Out).
show_workflow(In,P,Out):- must_det_ll(call(P,In,Out)),!.
show_workflow(In,P,In):- arcdbg(warn(failed(show_workflow(P)))),!.
 
show_workflow_each([],_P,[]):-!.
show_workflow_each(In,P,Out):- is_grid(In),!,show_workflow(In,P,Out).
show_workflow_each([H|T],P,[Mid|Out]):-
  show_workflow_each(H,P,Mid),!,
  show_workflow_each(T,P,Out).
show_workflow_each(In,P,Out):- show_workflow(In,P,Out).

into_singles(Obj,Obj):- is_object(Obj),!.
into_singles(Group,Obj):- is_group(Group),!,member(Obj,Group).
into_singles(Class,Obj):- (iz(Obj,Class),deterministic(YN)), (YN==true->!;true).
into_singles(Obj,Obj).

vert_pos(Class,Y):- into_singles(Class,Obj),loc_xy(Obj,_X,Y).
horiz_pos(Class,X):- into_singles(Class,Obj),loc_xy(Obj,X,_Y).

when_config(This,Goal):-test_config(This)-> call(Goal) ; true.
test_config(This):- current_test_name(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

test_cond_or(This,_That):- test_config(This),!.
test_cond_or(This, That):- term_variables(This,[That|_]),!.

run_dsl(Prog,In,Out):- nb_linkval(dsl_pipe,In),run_dsl(enact,Prog,In,Out).

run_dsl(Mode,Prog,In,Out):- plain_var(Prog),!,throw(var_solving_progs(Mode,Prog,In,Out)).
run_dsl(_Mode,Prog,In,Out):- Prog==[],!,In=Out,nop(( plain_var(Out)->Out=In; true)).
run_dsl(Mode,(H,Prog),In,Out):-!, run_dsl(Mode,H,In,GridM), run_dsl(Mode,Prog,GridM,Out).
run_dsl(Mode,[H|Prog],In,Out):-!, run_dsl(Mode,H,In,GridM), run_dsl(Mode,Prog,GridM,Out).
run_dsl(Mode,Prog,_In,_Out):- pt(yellow,run_dsl(Mode,Prog,in,out)),fail.
run_dsl(Mode,Prog,In,Out):- In==dsl_pipe,!,  nb_current(dsl_pipe,PipeIn), run_dsl(Mode,Prog,PipeIn,Out).
run_dsl(Mode,Prog,In,Out):- Out==dsl_pipe,!, run_dsl(Mode,Prog,In,PipeOut),nb_linkval(dsl_pipe,PipeOut).
run_dsl(Mode,doall(All),In,OutO):- !, run_dsl(Mode,forall(All,true),In,OutO).
run_dsl(Mode,lmDSL(Prog),In,Out):- !, run_dsl(Mode,Prog,In,Out).
run_dsl(_Mode,call(G),In,Out):-!,call(G),(plain_var(Out)->Out=In; true).
run_dsl(_Mode,same,In,Out):-!, duplicate_term(In,Out).
run_dsl(Mode,-->(All,Exec),In,Out):-!, run_dsl(Mode,forall(All,Exec),In,Out).
run_dsl(Mode,forall(All,Exec),In,OutO):-!,  
 nb_linkval(dsl_pipe,In),
 forall(run_dsl(Mode,All,dsl_pipe,Mid),
  (run_dsl(enforce,Exec,Mid,Out),
   nb_linkval(dsl_pipe,Out))),nb_current(dsl_pipe,OutO).
% prevents unneeded updates such as color/position settings
run_dsl(enforce,Prog,In,In):- \+ missing_arity(Prog, 0), call(Prog), !. 
run_dsl(enforce,color(Obj,Color),In,Out):-!, 
 color(Obj,ColorWas),subst_color(ColorWas,Color,In,Out),
    override_object_io(color(Color),Obj,In,Out).

run_dsl(enforce,vert_pos(Obj,New),In,Out):-!, 
  loc_xy(Obj,X,_Old),
    override_object_io(loc_xy(X,New),Obj,In,Out).

run_dsl(_Mode,Prog,In,In):- \+ missing_arity(Prog, 0), !, call(Prog).
run_dsl(Mode,Prog,In,Out):- \+ missing_arity(Prog,2), !,
 (call(Prog,In,M)*-> 
    =(M,Out) ; (arcdbg(warn(nonworking(run_dsl(Mode,Prog)))),fail)).
run_dsl(Mode,Prog,In,In):- arcdbg(warn(missing(run_dsl(Mode,Prog)))),!,fail.

override_object_io(Update,Obj,In,Out):- 
  remove_global_points(Obj,In,Mid), 
  override_object(Update,Obj,ObjCopy), 
  add_global_points(ObjCopy,Mid, Out).


sync_colors(Orig,Colors):- is_object(Orig),!,colors(Orig,Colors),
  globalpoints(Orig,OrigGPoints),colors(OrigGPoints,Colors),
  localpoints(Orig,OrigLPoints),colors(OrigLPoints,Colors),!.
sync_colors(Orig,Colors):- colors(Orig,Colors).
closure_grid_to_object(Orig,Grid,NewObj):- 
  sync_colors(Orig,Colors),
  object_indv_id(Orig,ID,_Iv),
  grid_size(Grid,H,V), 
  
  globalpoints(Grid,Points),
  make_indiv_object(ID,H,V,Points,[object_shape(grid)],PartialObj),
  sync_colors(PartialObj,Colors),
  transfer_props(Orig,[loc_xy,object_shape],PartialObj,NewObj),!.

closure_grid_to_group(Orig,Grid,Group):- individuate(Orig,Grid,Group).

into_grids(P,G):- no_repeats(G,quietly(recast_to_grid(P,G, _))).
into_grid(P,G):- quietly(recast_to_grid(P,G, _)).

print_grid_to_string(G,S):- with_output_to(string(S),print_grid(G)).
print_grid_to_atom(G,S):- with_output_to(atom(S),print_grid(G)).
% ?- print_grid(gridFn(X)).
recast_to_grid(Grid,Grid, (=) ):- is_grid(Grid),!.
recast_to_grid(Obj,Grid, closure_grid_to_object(Obj)):- is_object(Obj),!, object_grid(Obj,Grid),!.
recast_to_grid(Grp,Grid, closure_grid_to_group(Grp)):- is_group(Grp), !, object_grid(Grp,Grid),!.
recast_to_grid(Points,Grid,globalpoints):- is_points_list(Points), !, points_to_grid(Points,Grid),!.
recast_to_grid(Text,Grid, print_grid_to_string ):- string(Text),!,text_to_grid(Text,Grid).
recast_to_grid(Text,Grid, print_grid_to_atom ):- atom(Text),!,text_to_grid(Text,Grid).

recast_to_grid(Naming,Grid, Closure ):- (named_gridoid(Naming,NG),recast_to_grid(NG,Grid, Closure))*->true;recast_to_grid0(Naming,Grid, Closure).
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

uncast(_Obj,Closure,In,Out):- call(Closure,In,Out).
named_gridoid(ID,G):- plain_var(ID),!,known_gridoid(ID,G).
%named_gridoid(ID,G):- plain_var(ID),!,dumpST,throw(var_named_test(ID,G)).
named_gridoid(ID,G):- known_gridoid(ID,G).

known_gridoid(ID,GO):- known_gridoid0(ID,G),to_real_grid(G,GO).
known_gridoid0(ID,G):- is_grid_id(G,ID).
known_gridoid0(ID,G):- is_shared_saved(ID,G).
known_gridoid0(ID,G):- is_unshared_saved(ID,G).
known_gridoid0(ID,G):- learned_color_inner_shape(ID,magenta,BG,G,_),get_bgc(BG).
known_gridoid0(ID,G):- (atom(ID);string(ID)),notrace(catch(atom_to_term(ID,Term,_),_,fail)),Term\==ID,!,known_gridoid0(Term,G).
known_gridoid0(ID,G):- ID= TstName*ExampleNum*IO, fix_test_name(TstName,Name,_),kaggle_arc_io(Name,ExampleNum,IO,G).
known_gridoid0(ID,G):- nonvar(ID),ID=(_*_),fix_test_name(ID,Name,ExampleNum),kaggle_arc_io(Name,ExampleNum,_IO,G).
known_gridoid0(ID,G):- fix_test_name(ID,Name,ExampleNum),nop(ignore(ExampleNum=tst+0)),kaggle_arc_io(Name,ExampleNum,_IO,G).

to_real_grid(G,GO):- notrace((unnumbervars(G,G1),get_bgc(BG),subst(G1,bg,BG,GO))). % ,ignore([[BG|_]|_]=GO).

kaggle_arc_io(Name,ExampleNum,IO,G):- kaggle_arc(Name,ExampleNum,In,Out), ((IO=in,G=In);(IO=out,G=Out)).

into_gridnameA(G,TstName):- known_gridoid(TstName,G).

grid_to_id(Grid,ID):- atom(Grid),!,ID=Grid.
grid_to_id(Grid,ID):- var(Grid),!,known_gridoid(Grid,ID).
grid_to_id(Grid,ID):- \+ ground(Grid), copy_term(Grid,GGrid),numbervars(GGrid,1,_),!,grid_to_id(GGrid,ID).
grid_to_id(Grid,ID):- known_gridoid0(ID,GVar),Grid=@=GVar,!.
grid_to_id(Grid,ID):- must_be_free(ID),makeup_gridname(ID), set_grid_id(Grid,ID),!.

makeup_gridname(GridName):- gensym('GridName_',ID), GridName = ID*('ExampleNum'+0)*io.


into_obj(G,O):- no_repeats(O,into_obj0(G,O)).

o2g(Obj,Glyph):- object_glyph(Obj,Glyph).
o2c(Obj,Glyph):- color(Obj,Glyph).
o2ansi(Obj,S):- o2c(Obj,C),o2g(Obj,G),atomic_list_concat([' ',G,' '],O),!,sformat(F,'~q',[O]),wots(S,color_print(C,F)).
:- dynamic(g2o/2).
into_obj0(G,E):- plain_var(G),!,enum_object(E),G=E.
into_obj0(obj(O),obj(O)):- is_list(O),!.
into_obj0(objFn(G),O):-!, into_obj(G,O),!.
into_obj0(G,O):- g2o(G,O),!.
into_obj0(G,O):- (atom(G);string(G)),!,Chars=[_,_|_],atom_chars(G,Chars),!,member(C,Chars),into_obj(C,O).
into_obj0(G,O):- is_grid(G),!,grid_to_individual(G,O).
into_obj0(G,O):- into_group(G,OL),must([O]=OL).

% this is bad  ?- into_grid('E',ID),grid_to_id(G,ID).  ?- into_grid('Z',ID),grid_to_id(G,ID).

into_group(GI,G):- into_group(GI,G, _ ).

into_group(P,G,(=)):- is_group(P),!,G=P.
into_group(G,G,(=)) :- G==[],!.
into_group(G, G, _):- plain_var(G),!, %throw(var_into_group(G)),
 why_grouped(G, _Why).

into_group(G,I, into_grid):- is_grid(G),!,compute_shared_indivs(G,I).
into_group(P,G, into_obj):- is_object(P),!,G=[P].
into_group(P,G, rev_lambda(why_grouped)):- named_gridoid(P,M),!,into_group(M,G).
into_group(P,G, _):- dumpST,throw(into_group(P,G)).
/*
into_group(P,G):- is_object(P),points_to_grid(P,M),!,into_group(M,G).
%into_group(G,G):- is_grid(G),!.

into_group(P,G):- is_group(P),set_grid_nums(P),
  maplist(into_group,P,Gs),!,combine_grids(overlay,Gs,G).
into_group(P,G):-
  maplist(into_group,P,Gs),!, 
  set_grid_nums(Gs), arg(1,Gs,G).
into_group(P,G):- maplist(into_group,P,Gs),!, set_grid_nums(Gs), combine_grids(overlay,Gs,G).
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

training_progs(Prog,In,Out):- plain_var(Prog),!,throw(var_training_progs(Prog,In,Out)).
training_progs(call(G),_In,_Out):-!,call(G).
training_progs([],_In,_Out):-!.
training_progs([H|Prog],In,Out):-!, training_progs(H,In,Out), training_progs(Prog,In,Out).
training_progs(Prog,_,_):- missing_arity(Prog, 2),!,arcdbg(warn(missing(training_progs(Prog)))).
training_progs(Prog,In,Out):- call(Prog,In,Out)*-> true ; arcdbg(warn(nonworking(training_progs(Prog)))).


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


throw_missed(G):-  Info = missed(G),wdmsg(Info),break, dumpST,throw_missed_pt2(G,Info).
throw_missed_pt2(_,Info):- tracing,!,throw(Info).
throw_missed_pt2(G,Info):- notrace,nortrace,trace,wdmsg(Info),break,rtrace(G),throw(Info).



% make or do plan
do_change(Change,Grid1,Grid2):- \+ is_list(Change),!,one_change(Change,Grid1,Grid2).
do_change(Change,Grid1,Grid2):- do_change_nd(Change,Grid1,Grid2).

do_change_nd([],Grid1,Grid1).
do_change_nd([H|T],Grid1,Grid2):- one_change(H,Grid1,GridM),do_change_nd(T,GridM,Grid2).

one_change(same,Grid1,Grid2):- is_grid(Grid2),Grid1=Grid2,!.
one_change(colorChange(C1,C2),Grid1,Grid2):- 
  first_color(Grid1,C1),ignore((is_grid(Grid2),first_color(Grid2,C2))),
  subst_w_attv(Grid1,C1,C2,Grid2).
one_change(blank1Color(C1),Grid1,Grid2):- 
  first_color(Grid1,C1),copy_cells(==(C1),free_cell,Grid1,Grid2).
one_change(same_size,Grid1,Grid2):- plain_var(Grid2),grid_size(Grid1,H,V),grid_size(Grid2,H,V),!.

subst_w_attv(I,F,R,O):- quietly((subst(I,F,R,O))),!.
%subst_w_attv(I,F,R,O):- map_pred(subst_and_attv(F,R),I,O).
%subst_and_attv(F,R,I,O):- F == R, O = I, !.

/*
nth_fact(P, I):- clause(P, true, Ref), nth_clause(P, I, Ref).


% make or do plan
do_change(Change, Grid1, Grid2):- \+ is_list(Change), !, one_change(Change, Grid1, Grid2).
do_change(Change, Grid1, Grid2):- do_change_nd(Change, Grid1, Grid2).

do_change_nd([], Grid1, Grid1).
do_change_nd([H|T], Grid1, Grid2):- one_change(H, Grid1, GridM), do_change_nd(T, GridM, Grid2).

one_change(same, Grid1, Grid2):- is_grid(Grid2), Grid1=Grid2, !.
one_change(colorChange(C1, C2), Grid1, Grid2):- 
 first_color(Grid1, C1), ignore((is_grid(Grid2), first_color(Grid2, C2))), subst(Grid1, C1, C2, Grid2).
one_change(blank1Color(C1), Grid1, Grid2):- 
 first_color(Grid1, C1), copy_cells(==(C1), free_cell, Grid1, Grid2).
one_change(same_size, Grid1, Grid2):- plain_var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C1), !.
one_change(resize(C1, C2), Grid1, Grid2):- plain_var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C2).

*/

arc_expand_arg(objFn(X),Var,into_obj(X,Var)).
arc_expand_arg(gridFn(X),Var,into_grid(X,Var)).
arc_expand_arg(groupFn(X),Var,into_group(X,Var)).

goal_expansion_query(Goal,Out):-
   compound(Goal), predicate_property(Goal,meta_predicate(_)),!,
   arg(N,Goal,P), compound(P), goal_expansion_query(P,MOut), 
   MOut\=@=P, setarg(N,Goal,MOut), expand_goal(Goal, Out).

goal_expansion_query(Goal,Out):- compound(Goal),
   get_setarg_p1(I,Goal,P1), compound(I), arc_expand_arg(I,Var,Exp),
   call(P1,Var),expand_goal((Exp,Goal),Out).


:- fixup_exports.

goal_expansion(Goal,I,Out,O):-  var(I), \+ source_location(_,_),nb_current('$goal', Term),% writeq(Term=@=Goal),nl,
  Goal=@=Term,
  (goal_expansion_query(Goal,Out)-> Goal\=@=Out),I=O.

% ?- print_grid(gridFn(X)).
%:- export(is_toplevel_query/2).
%:- b_setval('$goal', []).
:- nb_linkval('$goal', []).
%:- b_setval('$goal_expanded', []).
:- nb_linkval('$goal_expanded', []).
expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
    % Have vars to expand and varnames are empty
    nb_linkval('$goal', Goal),
    quietly((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])), % this prevents the loop
    nb_linkval('$variable_names', Bindings),
    debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    writeq(Goal+Bindings),nl,
    expand_query(Goal, Expanded, Bindings, ExpandedBindings),
    nb_linkval('$goal_expanded', Expanded).    


