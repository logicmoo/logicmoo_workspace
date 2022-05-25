

:- discontiguous(decl_pt/1).
check_args(P,MC):- functor(P,F,A),functor(T,F,A),functor(C,F,A),decl_pt(T),check_args(P,1,A,A,T,C,MC).

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


into_type(Type,G,fake(Type,G)):- var(G),throw(var_into_type(Type,G)).
into_type(+,X,X).
into_type(num,X,X):- assertion(number(X)).
into_type(dir,X,X):- assertion(nav(X,_,_)).
into_type(grid,X,O):- into_grid(X,O).
into_type(object,X,O):- is_object(X)-> X=O ; into_object(X,O).
into_type(group,X,O):- into_group(X,O).



when_config(This,Goal):-test_config(This)-> call(Goal) ; true.
test_config(This):- current_test_name(Name),test_info(Name,InfoL),!,contains_nonvar(This,InfoL).

test_cond_or(This,_That):- test_config(This),!.
test_cond_or(This, That):- term_variables(This,[That|_]),!.

run_dsl(Prog,In,Out):- run_dsl(enact,Prog,In,Out).

run_dsl(Mode,Prog,In,Out):- var(Prog),!,throw(var_solving_progs(Mode,Prog,In,Out)).
run_dsl(Mode,lmDSL(Prog),In,Out):- !, run_dsl(Mode,Prog,In,Out).
run_dsl(_Mode,call(G),In,Out):-!,call(G),(var(Out)->Out=In; true).
run_dsl(_Mode,[],In,Out):-!, var(Out)->Out=In; true.
run_dsl(_Mode,same,In,Out):-!, duplicate_term(In,Out).
run_dsl(ennfore,color(Obj,Color),In,Out):-!, set_global_points(Color,Obj,In,Out).
run_dsl(Mode,-->(All,Exec),In,Out):-!,  run_dsl(Mode,forall(All,Exec),In,Out).
run_dsl(Mode,forall(All,Exec),In,Out):-!,  forall(run_dsl(Mode,All,In,Mid),(run_dsl(enforce,Exec,Mid,Out),nb_setval(out,Out))),nb_current(out,Out).
run_dsl(Mode,[H|Prog],In,Out):-!, run_dsl(Mode,H,In,GridM), run_dsl(Mode,Prog,GridM,Out).
run_dsl(Mode,(H,Prog),In,Out):-!, run_dsl(Mode,H,In,GridM), run_dsl(Mode,Prog,GridM,Out).
run_dsl(_Mode,Prog,In,In):- \+ missing_arity(Prog, 0), !, call(Prog).
run_dsl(Mode,Prog,In,Out):- \+ missing_arity(Prog,2), !,
 (call(Prog,In,M)*-> 
    =(M,Out) ; (arcdbg(warn(nonworking(run_dsl(Mode,Prog)))),fail)).
run_dsl(Mode,Prog,In,In):- arcdbg(warn(missing(run_dsl(Mode,Prog)))). 

named_gridoid(TstName,G):- var(TstName),!,dumpST,throw(var_named_test(TstName,G)).
named_gridoid(TstName,G):- fix_test_name(TstName,Name,_),kaggle_arc(Name,tst+0,G,_),!.
named_gridoid(TstName,G):- known_gridoid(TstName,G).

known_gridoid(TstName*ExampleNum*in,G):- fix_test_name(TstName,Name,_),!,kaggle_arc(Name,ExampleNum,G,_).
known_gridoid(TstName*ExampleNum*out,G):- fix_test_name(TstName,Name,_),!,kaggle_arc(Name,ExampleNum,_,G).
known_gridoid(TstName*T,G):- fix_test_name(TstName+T,Name,ExampleNum),kaggle_arc(Name,ExampleNum,G,_).
known_gridoid(TstName,G):- learned_color_inner_shape(TstName,magenta,BG,G,_),get_bgc(BG).
known_gridoid(TstName,G):- is_gridname(G,TstName).
known_gridoid(TstName,G):- is_shared_saved(TstName,G).
known_gridoid(TstName,G):- is_unshared_saved(TstName,G).

into_object(G,O):- is_grid(G),grid_to_individual(G,O),!.
into_object(G,O):- into_group(G,OL),must([O]=OL).

into_group(G,G):- var(G),throw(var_into_group(G)).
into_group(P,G):- is_group(P),!,G=P.
into_group(G,I):- is_grid(G),!,compute_shared_indivs(G,I).
into_group(P,G):- is_object(P),!,G=[P].
into_group(P,G):- named_gridoid(P,M),!,into_group(M,G).
into_group(P,G):- dumpST,throw(into_group(P,G)).
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

:- dynamic(iz/2).

subclazz(outline,hollow).
subclazz(outline,thick1).
subclazz(outline,rectangle).
subclazz(outline,noexit).

iz(X,Y):- nonvar(Y)->(subclazz(P,Y),iz(X,P));(nonvar(X),iz(X,P),subclazz(P,Y)).
iz(X,Y):- object_shape(X,Y).

gather_object(Obj1,Var,Expression,Grid,Grid):-
  create_bag(Obj1),
  forall(Expression,ain(part_of(Var,Obj1))).

gather_object(Obj1,Var,Expression,Grid,Grid):-
  create_bag(Obj1),ain(iz(Obj1,group)),
  forall(Expression,ain(part_of(Var,Obj1))).


wall_thickness(X,N):- iz(X,polygon),calc(wall_thickness(X,N)).

calc(_).

create_bag(Obj1):- gensym(bag_,Obj1),ain(iz(Obj1,group)).

training_progs(Prog,In,Out):- var(Prog),!,throw(var_training_progs(Prog,In,Out)).
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
  trim_grid_to_size(point,object_size),
  fill_from_point(point,color),
  create_a_ray(point,dir,len),
  object_as_own_grid(obj,gridOps),
  copy_one_object(obj,point),
  rotate_one_object(obj,nsew),
  flatten_one_object(obj),
  sort_by_gravity(nsew),
  flip_grid(hOrv),
  rotate_grid(nsew)]).


throw_missed(G):-  Info = missed(G),wdmsg(Info), dumpST,throw_missed_pt2(G,Info).
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
  subst(Grid1,C1,C2,Grid2).
one_change(blank1Color(C1),Grid1,Grid2):- 
  first_color(Grid1,C1),copy_cells(==(C1),free_cell,Grid1,Grid2).
one_change(same_size,Grid1,Grid2):- var(Grid2),grid_size(Grid1,H,V),grid_size(Grid2,H,V),!.




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
one_change(same_size, Grid1, Grid2):- var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C1), !.
one_change(resize(C1, C2), Grid1, Grid2):- var(Grid2), grid_size(Grid1, C1), grid_size(Grid2, C2).

*/
