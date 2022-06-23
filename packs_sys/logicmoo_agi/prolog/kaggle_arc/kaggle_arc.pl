/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- encoding(iso_latin_1).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).

my_is_clause(H,B):- clause(H,B,Ref),clause(HH,BB,Ref), H+B=@=HH+BB.
my_asserta_if_new((H:-B)):- !, (my_is_clause(H,B) -> true ; asserta(H:-B)).
my_asserta_if_new(HB):- my_asserta_if_new(HB:-true).
my_assertz_if_new((H:-B)):- !, (my_is_clause(H,B) -> true ; assertz(H:-B)).
my_assertz_if_new(HB):- my_assertz_if_new(HB:-true).

:- multifile(decl_sf/1).
:- discontiguous(decl_sf/1).
:- dynamic(decl_sf/1).
decl_sf(G):- ground(G), !, my_assertz_if_new(decl_sf(G)).
:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).
decl_pt(G):- ground(G), !, my_assertz_if_new(decl_pt(G)).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).

% COMMAND LINE ARC
:- if(\+ current_module(logicmoo_arc)).
  muarc_mod(user).
  :- set_prolog_flag(access_level,system).
  :- dynamic(prolog:'$exported_op'/3).
  :- assert((system:'$exported_op'(_,_,_):- fail)).
  %:- multifile('$exported_op'/3).
  :- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
  :- SL  is 2_147_483_648*8*4, set_prolog_flag(stack_limit, SL ).
  % :- (getenv('DISPLAY',_) -> guitracer ; true).
  :- set_prolog_flag(toplevel_print_anon,false).
  :- set_prolog_flag(toplevel_print_factorized,true).
    :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(4), attributes(dots)]).
    :- set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(4), attributes(dots)]).


clsmake:- cls1,!,update_changed_files,make,!.
% SWISH ARC
:- else.

clsmake:- update_changed_files,!.

  muarc_mod(muarc).
  :- if(current_module(trill)).
  :- set_prolog_flag_until_eof(trill_term_expansion,false).
  :- dynamic(muarc:ns4query/1).
  :- endif.
:- endif.

:- use_module(library(logicmoo_common)).


% we alias these so we can catch out of control list growth
my_append(A,B):- append(A,B).
my_append(A,B,C):- append(A,B,C). % ,check_len(A),check_len(C),check_len(C).
check_len(_).

must_det_ll((X,Y)):- !, must_det_ll(X),!,must_det_ll(Y).
must_det_ll((X;Y)):- !, (must_det_ll(X);must_det_ll(Y)).
must_det_ll((A ->X;Y)):- !,(call(A)->must_det_ll(X);must_det_ll(Y)).
must_det_ll(X):- call(X),!.

%must_det_l_goal_expansion(G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
%must_det_l_goal_expansion(G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.

% goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), must_det_l_goal_expansion(G,GG),I=O.

%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).

/*
goal_expansion(Goal,Out):- compound(Goal), arg(N,Goal,E), 
   compound(E), E = set(Obj,Member), setarg(N,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(E,Cmpd,SA):-  compound(Cmpd), get_setarg_p2(E,Cmpd,SA).
get_setarg_p2(E,Cmpd,SA):- arg(N,Cmpd,E), SA=setarg(N,Cmpd).
get_setarg_p2(E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(E,Arg,SA).


term_expansion_setter((Head:-Body),Out):- 
   get_setarg_p1(I,Head,P1), is_obj_setter(I,Obj,Member,Var),
   call(P1,Var),
   BodyCode = (Body, my_b_set_dict(Member,Obj,Var)),
   % goal_expansion_setter(BodyCode,Goal),
   expand_term((Head:- BodyCode),Out),!.
%term_expansion_setter((Head:-Body),(Head:-GBody)):- goal_expansion_setter(Body,GBody),!.

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

/*

set(_355218._355220)=_355272)
*/

is_obj_setter(I,_Obj,_Member,_Var):- \+ compound(I),!,fail.
is_obj_setter(set(Obj,Member),Obj,Member,_Var).
is_obj_setter(set(ObjMember),Obj,Member,_Var):- compound(ObjMember), ObjMember =.. ['.',Obj,Member],!.

goal_expansion_setter(Goal,_):- \+ compound(Goal), !, fail.
goal_expansion_setter(Goal,Out):- 
   predicate_property(Goal,meta_predicate(_)),!, fail,
   arg(N,Goal,P), goal_expansion_setter(P,MOut),
   setarg(N,Goal,MOut), !, expand_goal(Goal, Out).

goal_expansion_setter(Goal,Out):-
   arg(N,Goal,P),  is_obj_setter(P,Obj,Member,Var),
   setarg(N,Goal,Var), !, expand_goal((Goal,my_b_set_dict(Member,Obj,Var)), Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(I,Goal,P1), is_obj_setter(I,Obj,Member,Var),
   call(P1,Var),!,
   expand_goal((Goal,my_b_set_dict(Member,Obj,Var)),Out).

my_b_set_dict(Member,Obj,Var):- must_be_nonvar(Member), must_be_nonvar(Obj),  
  nb_set_dict(Member,Obj,Var),
  nb_link_dict(Member,Obj,Var),
  b_set_dict(Member,Obj,Var).

system:term_expansion((Head:-Body),I,Out,O):- nonvar(I),  compound(Head), term_expansion_setter((Head:-Body),Out),(Head:-Body)=In,In\==Out,I=O,!,
 nop((print(term_expansion_setter(In-->Out)),nl)).
goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_setter(Goal,Out),Goal\==Out,I=O,!, 
  nop((print(goal_expansion_setter(Goal-->Out)),nl)).

/*
 tests for term expander

:- style_check(-singleton).

d:- set(X.Y) = V.
d:- must_det_ll((set(X.a) = b)).
d:- must_det_ll(didit([foo|set(X.Y)])).
d:- member(set(X.Y),V).
doit(set(E.v)):- that.
:- style_check(+singleton).
*/


%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(fav2).
:- add_history1(arc2).
:- add_history1(arc).
:- add_history1(arc1).
:- add_history1(fav).
:- add_history1(fav1).
:- add_history1(fav3).


%:- learn_shapes.
:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_physics).
:- ensure_loaded(kaggle_arc_db).
:- ensure_loaded(kaggle_arc_heuristics).
:- ensure_loaded(kaggle_arc_intruder).
:- ensure_loaded(kaggle_arc_individuation).
:- ensure_loaded(kaggle_arc_interpreter).
:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_learning).
:- ensure_loaded(kaggle_arc_imagens).
:- ensure_loaded(kaggle_arc_recognise).
:- ensure_loaded(kaggle_arc_uniqueness).
:- ensure_loaded(kaggle_arc_ui_html).
:- ensure_loaded(kaggle_arc_test_easy).
:- ensure_loaded(kaggle_arc_test_old).



%:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

arc:- forall(arc11,true).
arc1:- clsmake, test_names_by_hard(X), whole_test(X).
arc2:- clsmake, test_names_by_hard_rev(X), whole_test(X).
arc11:- clsmake, test_names_by_hard(X), arc1(X).
arc22:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- clsmake, arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav3:- clsmake, arc1(t('3631a71a')*(_+_)),!.
fav:- clsmake,forall(fav11,true).
favr:- clsmake,forall(fav22,true).
fav1:- clsmake, test_names_by_hard_rev(X), whole_test(X).
fav2:- clsmake, test_names_by_fav_rev(X), whole_test(X).
fav11:- clsmake, test_names_by_fav(X), arc1(X).
fav22:- clsmake, test_names_by_fav_rev(X), arc1(X).
favL:- clsmake, get_current_test(X),!,whole_test(X).
favC:- clsmake, set_current_test(Y), UT=until_test(Y),!,
  test_names_by_hard(X),until_test(X)=UT,nb_setarg(1,UT,_),whole_test(X).

whole_test(X):- cls1, with_tty_raw(interactive_test(X)).
%whole_test(X):- cls1, noninteractive_test(X).

fav(X):- nonvar(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- time(forall(arc1(true,TestID),true)).

arc1(TName):- arc1(true,TName).
%arc1(G,TName):- arc2(G,TName,(_+0)).

arc1(G,TName):-
 set_current_test(TName),
 retractall(why_grouped(individuate(_),_)),
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,TestID,_UExampleNum),    
   clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
   clear_shape_lib(intruder),!,
   % choice point created here purposely
  nb_delete('$training_vm'),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),
  ignore((catch((call(G),
    run_arc_io(TestID,ExampleNum,In,Out)),'$aborted',true)))))).


cls1:- nop(catch(cls,_,true)).

arc_grid(Grid):- test_names_by_fav(TestID),kaggle_arc(TestID,_ExampleNum,In,Out),arg(_,v(In,Out),Grid).

:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)*(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')*(tst+0),"segv").
%is_buggy_pair(t('27a28665')*(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum):- Pair = TestID*ExampleNum, is_buggy_pair(Pair,Why),!,format("~N % Skipping ~q because: ~w ~n~n",[Pair,Why]).
run_arc_io(TestID,ExampleNum):- 
  time(train_test(TestID,ExampleNum)),
  time(solve_test(TestID,ExampleNum)).

make_indivs(Pred,In,Out,InC,OutC):-
 %locally(i_o_w(In,Out),
  make_indivs1(Pred,In,Out,InC,OutC).

make_indivs1(Pred,In,Out,InC,OutC):-
  mass(Out,OMass), mass(In,IMass), OMass>IMass,
  make_indivs_no_swap(Pred,Out,In,OutC,InC),
  save_off(Pred,In,Out,InC,OutC).

make_indivs1(Pred,In,Out,InC,OutC):- 
  make_indivs_no_swap(Pred,In,Out,InC,OutC),
  save_off(Pred,In,Out,InC,OutC).

save_off(_Pred,_In,_Out,InC,OutC):-
 ((
  get_training(Training),!,
  pred_intersection(overlap_same_obj,_InC,_OutC,_RetainedIn,_RetainedOut,Removed,Added),!,
  add_shape_lib(pair,Removed),!,  
  add_shape_lib(pair,Added),!,
  add_shape_lib(pair,RetainedIn),!,
  %rtrace,
  % % %  set(Training.grid_in) = In,!,
  %trace,

  % % %  set(Training.grid_out) = Out,
  % % %  set(Training.inC) = InC,
  % % %  set(Training.outC) = OutC,
  % % %  set(Training.added) = Added,
  % % %  set(Training.removed) = Removed,
  % % %  set(Training.kept) = RetainedIn,
  %nortrace,
  nop(set_training(Training)))),!.

make_indivs_no_swap(Pred,In,Out,InC,OutC):-
 must_det_ll((
  writeln(inC(Pred)),
  %rtrace,
  call_indv(Pred,In,InC),
  add_shape_lib(pair,InC),!,
  writeln(outC(Pred)),
  call_indv(Pred,Out,OutC),
  %add_shape_lib(out,OutC),
  writeln(inOutC(Pred)))),!.

call_indv(Pred,Out,OutC):-
 must_det_ll((
  %get_training(Training),
  % % %  set(Training.grid) = Out, % % %  set(Training.grid_o) = Out,
  %% % %  set(Training.objs) = _, % % %  set(Training.points) = _,
  %% % %  set(Training.robjs) = _, % % %  set(Training.points_o) = _,
  %grid_size(Out,H,V), % % %  set(Training.h) = H, % % %  set(Training.v) = V,
  %nortrace,
  call(Pred,Out,OutC))),!.
  %set_training(Training).


/*
show_indivs(IH,IV,OH,OV,Pred,When,PairName,In,Out,SF):-  
  ignore(IH=1),
  LW is (IH * 2 + 12),
  append_term_safe(When,Pred+PairName+in,NameIn),
  append_term_safe(When,Pred+PairName+out,NameOut),
  wots(U1, print_grid(IH,IV,NameIn,In)),
  wots(U2, print_grid(OH,OV,NameOut,Out)),
  print_side_by_side(U1,LW,U2),
  my_append(InC,OutC,InOutC),
  smallest_first(InOutC,SF),
  %largest_first(InOutC,LF),
  %show_pair_no_i(IH,IV,OH,OV,smallest_first(When,Pred),PairName,InC-SF,OutC-SF),
  %wots(U3, print_grid(IH,IV,NameIn,InC-SF)),wots(U4, print_grid(OH,OV,NameOut,OutC-SF)),print_side_by_side(U3,LW,U4),
  %max_min(IH,OH,H,_),max_min(IV,OV,V,_),
  show_pair_no_i(IH,IV,OH,OV,normal(When,Pred),PairName,InC,OutC),
  INFO = [grid_dim,mass,colors_count_size,colors],
  print_side_by_side(
     describe_feature(InC,[call(writeln('IN'))|INFO]),LW,
    describe_feature(OutC,[call(writeln('OUT'))|INFO])),!,
  show_pair_I_info(NameIn,NameOut,InC,OutC).
*/
get_training(Training):- nb_current('$training_vm',Training),is_dict(Training),!.
get_training(Training):- notrace(get_current_test(TestID)),make_training(TestID,Training), nb_linkval('$training_vm',Training),!.
set_training(Training):- nb_linkval('$training_vm',Training).

make_training(TestID,VM):- make_fti(_GH,_GV,TestID,_Grid,_Sofar,_Reserved,_Options,_Points,ArgVM),
 must_det_l((
    test_hints(TestID,ArgVM,VM0),
    vars_to_dictation([test_id=TestID,mappings=[]],VM0,VM),

    /*
     test:ID,mappings:_,
     pre_in:_, pre_out:_,
     inC:_InC,outC:_OutC,
     removed:_,added:_, kept:_,   
     grid_in:_,grid_out:_,
  */
     set(VM.mappings) =[map])), !. % pt(VM),nl.


gather_more_task_info(TestID,III):- more_task_info(TestID,III).
gather_more_task_info(TestID,III):- fav(TestID,III).
  

%show_arc_pair_progress(TestID,ExampleNum,In,Out):- show_arc_pair_progress_sol(TestID,ExampleNum,In,Out),!.
train_test:- notrace(get_current_test(TestID)),
  train_test(TestID,(trn+_)),!,
  solve_test(TestID,(trn+_)),!.

train_test(TestID,ExampleNum):- 
  locally(set_prolog_flag(gc,false),
   train_test0(TestID,ExampleNum)).


train_using_hints(TestID,DictIn,DictOut):- update_model_from_hints(TestID,trn,0,DictIn,DictOut).
update_model_from_hints(TestID,Trn,N,DictIn,DictOut):-
  (kaggle_arc(TestID,(Trn+N),In,Out),
  train_for_objects(DictIn,TestID,[Trn,'_','i',N,'_','o',N,'_'],In,Out,DictM),
  NN is N + 1),
 (kaggle_arc(TestID,(Trn+NN),In2,Out2) -> 
    (train_for_objects(DictM,TestID,[Trn,'_','i',N,'_','i',NN,'_'],In,In2,Dict0),
     train_for_objects(Dict0,TestID,[Trn,'_','o',N,'_','o',NN,'_'],Out,Out2,Dict1),
     update_model_from_hints(TestID,Trn,NN,Dict1,DictOut));
  (DictM = DictOut)),!.


train_test0(TestID,ExampleNum):-
 must_det_l((
  set_training(PrevPairEnv),
  flag(indiv,_,0),
  nb_setval(prev_pairEnv,PrevPairEnv),
  nb_delete('$training_vm'),
  get_training(Training),
  test_hints(TestID,Training,Dictation),
  set_training(Dictation),
  train_using_hints(TestID,Dictation,DictOut),
  set_training(DictOut),
  garbage_collect,
  ((forall(gather_more_task_info(TestID,III),pt(III)),nl)),!,
  ((catch(maybe_confirm_dsl(Training,TestID,ExampleNum,InC,Out),E2,wdmsg(E2)))))).

which_io(i,in). which_io(o,out).
train_for_objects(Dict0,TestID,ExampleNum,In,Out,Dict1):-
 ExampleNum = [Trn,'_',IsIO1,N,'_',IsIO2,NN,'_'],
 which_io(IsIO1,IO1),
 which_io(IsIO2,IO2),
 pt(ExampleNum:IO1:IO2),
 must_det_l((
  atomic_list_concat([Trn,'_',IsIO1,N,'_',IsIO2,NN,'_'],Prefix),
  ExampleNum = (Trn+Prefix),
  kaggle_arc(TestID,(Trn+N),IO1,Out),
  kaggle_arc(TestID,(Trn+NN),IO2,In),
  garbage_collect,
  Dict0=Dict1,
  %must_be_nonvar(Training),
  %% % %  set(Training.grid_in) = In,
  %% % %  set(Training.grid_out) = Out,
	name_the_pair(TestID,ExampleNum,In,Out,PairName),
  %% % %  set(Training.test) = PairName,
	grid_size(In,IH,IV), grid_size(Out,OH,OV),
	ignore((IH+IV \== OH+OV , writeln(oi(size(IH,IV)->size(OH,OV))))),
  ignore((forall(gather_more_task_info(TestID,III),pt(III)),nl)), 
  clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
  %print_collapsed
  % set_training(Training),!,
  show_pair_grid(IH,IV,OH,OV,original_in,original_out,PairName,In,Out),!,  
  %make_indivs(individuate(pre_pass),In,Out,PreInC,PreOutC),!,
  %% % %  set(Training.pre_out) = PreOutC,
  %% % %  set(Training.pre_in) = PreInC,  
  make_indivs(individuate(complete),In,Out,InC,OutC),!,
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  add_shape_lib(in,Removed),
  add_shape_lib(pair,RetainedIn),
  % add_shape_lib(pair,RetainedOut),
  add_shape_lib(out,Added),
  max_min(IH,OH,IOH,_),
  max_min(IV,OV,IOV,_))),
  %make_indivs(individuate_second_pass,In,Out,InP2,OutP2),
  %make_indivs(individuate_complete,InP2,OutP2,InC2,OutC2),
  dash_chars,dash_chars,dash_chars,dash_chars,
  %show_pair_grid(IH,IV,   OH, OV,original_in,original_out,PairName,In,Out),
  %show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,PreInC,PreOutC),
  show_pair_grid(IH,IV,   OH, OV,original_in,original_out,PairName,In,Out),
  show_pair_diff(IOH,IOV,IOH,IOV,removed,added,PairName,Removed,Added),

  show_pair_diff(IH,IV,   OH, OV,retained_in,retained_out,PairName,RetainedIn,RetainedOut),
  %show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,InP2,OutP2),
  %show_pair_grid(IH,IV,   OH, OV,i_pass1,o_pass1,PairName,InC2,OutC2),
  show_pair_diff(IH,IV,   OH, OV,in(individuate),out(individuate),PairName,InC,OutC),!.
  %ignore((catch(with_named_pair(solve,TestID,PairName,In,Out),E1,wdmsg(E1)))),


solve_test:- get_current_test(TestID), solve_test(TestID,(tst+_)),!.

solve_test(TestID,ExampleNum):-
 forall(kaggle_arc(TestID,ExampleNum,In,Out),
  (nop(sols_for(TestID,_Sol)),
   must_det_l((
    get_training(PrevPairEnv),
    nb_setval(prev_pairEnv,PrevPairEnv),
    nb_delete('$training_vm'),
    get_training(Training),
    flag(indiv,_,0),
    name_the_pair(TestID,ExampleNum,In,Out,PairName),
    grid_size(In,IH,IV), grid_size(Out,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(oi(size(IH,IV)->size(OH,OV))))),
    ignore((forall(gather_more_task_info(TestID,III),pt(III)),nl)), 
    clear_shape_lib(in),clear_shape_lib(out),clear_shape_lib(pair),clear_shape_lib(noise),  
    get_training(Training),
    %% % %  set(Training.test) = PairName,
    %% % %  set(Training.grid_in) = In,
    %% % %  set(Training.grid_out) = Out,
    %print_collapsed
    %set_training(Training),!,
    dash_chars, dash_chars,
    show_pair_grid(IH,IV,OH,OV,original_in,original_out,PairName,In,Out),!,  
    dash_chars, dash_chars,
    forall(gather_more_task_info(TestID,III),pt(III)),
    ignore(maybe_confirm_sol(Training,TestID,ExampleNum,In,Out)))))),!.








  /*

  nop((
       show_indivs(IH,IV,OH,OV,individuate_complete,early,PairName,In,Out,SF),
       forall((rtrace_on_error(individualizer_heuristics(PairName,In,Out,IH,IV,OH,OV))),true), 
       add_shape_lib(pairs,SF),
       show_shape_lib(in),show_shape_lib(out),show_shape_lib(pair),show_shape_lib(noise),
       show_indivs(IH,IV,OH,OV,individuate_default,later,PairName,In,Out,_))),!,
       with_named_pair(solve,TestID,PairName,In,Out),
*/
/*
  remove_global_points(UnsharedIn,In,InForgotten),
  remove_global_points(UnsharedOut,Out,OutForgottenM),
  ((mass(OutForgottenM,OM),OM==0) -> OutForgotten=OutC; OutForgotten=OutForgottenM),
  individuate(complete,InForgotten,ForgottenShapesIn),
  individuate(complete,OutForgotten,ForgottenShapesOut),

  % contains_points(InForgotten);contains_points(OutForgotten)
  %show_pair_no_i(IH,IV,OH,OV,forgotten,PairName,ForgottenShapesIn,ForgottenShapesOut),

  show_pair_no_i(IH,IV,IH,IV,forgotten_In,PairName,UnsharedIn,ForgottenShapesIn),
  show_pair_no_i(OH,OV,OH,OV,forgotten_Out,PairName,ForgottenShapesOut,OutC),
*//*
       show_indivs(In,Out),
       individuate(defaults,In,InC),
       individuate(defaults,Out,OutC),  
       writeln(outC),

       clear_shape_lib(out),
       clear_shape_lib(in),
       add_shape_lib(out,OutC),
       writeln(inOutC),
       show_pair_i(IH,IV,OH,OV,early_test,PairName,InC,OutC),
       writeln(inC),
       individuate(defaults,In,UnsharedIn),
       writeln(outC),
       individuate(defaults,Out,UnsharedOut),
       writeln(inUnsharedOut),
       show_pair_i(IH,IV,OH,OV,late_test,PairName,UnsharedIn,UnsharedOut),
       format('~N-sofar~N'),!,
       %pt(yellow,in=UnsharedIn),
       pred_intersection(compare_objs1([same]),UnsharedIn,UnsharedOut,_CommonCsIn,_CommonCsOut,_IPCs,_OPCs),
       format('~N-pred_intersection~N'),
        individuate(UnsharedOut,Out,SharedInR),
        individuate(UnsharedIn,In,SharedOutR),
        show_pair_no_i(IH,IV,OH,OV,shared,PairName,SharedInR,SharedOutR))), !.*/
  %format('~N-Rule made from~N'),
  %show_rules,
/*  RESS =.. [res,unsharedIn=UnsharedIn,
             %onlyIn= IPCs,
            commonIn=CommonCsIn,commonOut=CommonCsOut, %onlyOut=OPCs, 
             unsharedOut=UnsharedOut],
  tersify(RESS,ShortInfo),         
  format('~N-Stats:~N'),
  pt(yellow,sol=ShortInfo), !.
  */
	
/*
  
  nop((reuse_indivs(SharedIn,SharedOut,BetterA,BetterB),
  ( (SharedOut\==BetterB ; SharedIn\== BetterA) ->
    show_pair_i(IH,IV,OH,OV,better,PairName,BetterA,BetterB);
     writeln('nothing better')))),
*/
  


:- nb_linkval(test_rules,[rules]).
:- nb_linkval(pair_rules,[rules]).
  

reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  my_append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  object_indv_id(B,ID,Iv),
  setq(A,object_indv_id(ID,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (pt(same_object(GlyphA,GlyphB,How))))).



:- fixup_exports.
%:- initialization(demo,program).
%:- initialization(demo,restore_state).
%:- initialization(demo,main).
%:- initialization(demo,after_load).
:- add_history((cls,make,demo)).
