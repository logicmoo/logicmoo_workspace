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
:- multifile is_fti_step/1.
:- multifile is_fti_stepr/1.
:- discontiguous is_fti_step/1.
:- discontiguous is_fti_stepr/1.

:- discontiguous ping_indiv_grid/1.
:- multifile ping_indiv_grid/1.

:- meta_predicate(fif(0,0)).
fif(IF, THEN) :- (   call(IF) ->  call(THEN) ;   true ).

% COMMAND LINE ARC
:- if(\+ current_module(logicmoo_arc)).
  muarc_mod(user).
  :- set_prolog_flag(access_level,system).
  :- dynamic(prolog:'$exported_op'/3).
  :- assert((system:'$exported_op'(_,_,_):- fail)).
  %:- multifile('$exported_op'/3).
  :- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
  :- SL  is 2_147_483_648*8*4, set_prolog_flag(stack_limit, SL ).
  :- (getenv('DISPLAY',_) -> guitracer ; true).
  :- set_prolog_flag(toplevel_print_anon,false).
  :- set_prolog_flag(toplevel_print_factorized,true).
    :- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(10), attributes(dots)]).
    :- set_prolog_flag(debugger_write_options, [quoted(true), portray(true), max_depth(10), attributes(dots)]).
    :- set_prolog_flag(print_write_options, [quoted(true), portray(true), max_depth(10), attributes(dots)]).

%:- set_prolog_flag(verbose_load,true).  
%:- set_prolog_flag(verbose_autoload,true).
%:- set_prolog_flag(debug_on_error,true).
%:- set_prolog_flag(report_error,true).
%:- set_prolog_flag(debugger_show_context,true).
%:- set_prolog_flag(last_call_optimisation,false).
%:- set_prolog_flag(trace_gc,false).
%:- set_prolog_flag(write_attributes,dots).
%:- set_prolog_flag(on_error,status).
%:- set_prolog_flag(backtrace_depth,100).
:- noguitracer.


clsmake:- notrace((cls1,!,update_changed_files,make)),!.
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

:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll_failed(0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).


must_det_ll(G):- !, call(G).
must_det_ll(X):- tracing,!,call(X).
must_det_ll((X,Y)):- !, must_det_ll(X),!,must_det_ll(Y).
must_det_ll((A->X;Y)):- !,(must_not_error(A)->must_det_ll(X);must_det_ll(Y)).
must_det_ll((A*->X;Y)):- !,(must_not_error(A)*->must_det_ll(X);must_det_ll(Y)).
must_det_ll((X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;must_det_ll_failed(X;Y)).
must_det_ll(\+ (X)):- !, (\+ must_not_error(X) -> true ; must_det_ll_failed(\+ X)).
%must_det_ll((M:Y)):- nonvar(M), !, M:must_det_ll(Y).
must_det_ll(once(A)):- !, once(must_det_ll(A)).
must_det_ll(X):- once(must_not_error(X))->true;must_det_ll_failed(X).

must_not_error(X):- catch(X,E,(E=='$aborted'-> throw(E);dumpST,writeq(E=X),rrtrace(X))).

must_det_ll_failed(X):- (wdmsg(failed(X)),dumpST)->trace,rrtrace(X),!.
% must_det_ll(X):- must_det_ll(X),!.

rrtrace(X):- notrace,nortrace, dumpST, sleep(0.5), trace, (notrace(\+ current_prolog_flag(gui_tracer,true)) -> rtrace(X); (trace,call(X))).

remove_must_dets(G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
remove_must_dets(G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.

% goal_expansion(must_det_l(G),I,must_det_ll(G),O):- nonvar(I),source_location(_,_), nonvar(G),I=O.

%goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), remove_must_dets(G,GG),I=O.

%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).

/*
goal_expansion(Goal,Out):- compound(Goal), arg(N1,Goal,E), 
   compound(E), E = set(Obj,Member), setarg(N1,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_setarg_p2(P3,E,Cmpd,SA).
get_setarg_p2(P3,E,Cmpd,SA):- arg(N1,Cmpd,E), SA=call(P3,N1,Cmpd).
get_setarg_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(P3,E,Arg,SA).


term_expansion_setter((Head:-Body),Out):- 
   get_setarg_p1(setarg,I,Head,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),
   BodyCode = (Body, set_omember(How,Member,Obj,Var)),
   % goal_expansion_setter(BodyCode,Goal),
   expand_term((Head:- BodyCode),Out),!.
%term_expansion_setter((Head:-Body),(Head:-GBody)):- goal_expansion_setter(Body,GBody),!.

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

/*

set(_355218._355220)=_355272)
*/

is_setter_syntax(I,_Obj,_Member,_Var,_):- \+ compound(I),!,fail.
is_setter_syntax(set(Obj,Member),Obj,Member,_Var,b).
is_setter_syntax(gset(Obj,Member),Obj,Member,_Var,nb).
is_setter_syntax(hset(How,Obj,Member),Obj,Member,_Var,How).
is_setter_syntax(set(ObjMember),Obj,Member,_Var,b):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(gset(ObjMember),Obj,Member,_Var,nb):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(hset(How,ObjMember),Obj,Member,_Var,How):- obj_member_syntax(ObjMember,Obj,Member).


obj_member_syntax(ObjMember,Obj,Member):-compound(ObjMember), ObjMember =.. ['.',Obj,Member],!.


goal_expansion_setter(Goal,_):- \+ compound(Goal), !, fail.
goal_expansion_setter(set_omember(A,B,C,D),set_omember(A,B,C,D)):-!.
goal_expansion_setter(set_omember(A,B,C),set_omember(b,A,B,C)):-!.
goal_expansion_setter(Goal,Out):- 
   predicate_property(Goal,meta_predicate(_)),!, fail,
   arg(N1,Goal,P), goal_expansion_setter(P,MOut),
   setarg(N1,Goal,MOut), !, expand_goal(Goal, Out).

goal_expansion_setter(Goal,Out):-
   arg(N1,Goal,P),  is_setter_syntax(P,Obj,Member,Var,How),
   setarg(N1,Goal,Var), !, expand_goal((Goal,set_omember(How,Member,Obj,Var)), Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(setarg,I,Goal,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),!,
   expand_goal((Goal,set_omember(How,Member,Obj,Var)),Out).


my_b_set_dict(Member,Obj,Var):- set_omemberh(b,Member,Obj,Var).
%nb_set_dict(Member,Obj,Var),
set_omemberh(b,Member,Obj,Var):- !, b_set_dict(Member,Obj,Var).
%nb_link_dict(Member,Obj,Var),
set_omemberh(nb,Member,Obj,Var):- !, nb_set_dict(Member,Obj,Var).
set_omemberh(link,Member,Obj,Var):- !, nb_link_dict(Member,Obj,Var).
set_omemberh(How,Member,Obj,Var):- call(call,How,Member,Obj,Var),!.

set_omember(Member,Obj,Var):-  set_omember(b,Member,Obj,Var).

set_omember(How,Member,Obj,Var):- 
  must_be_nonvar(Member), must_be_nonvar(Obj),  must_be_nonvar(How),  !,
  set_omemberh(How,Member,Obj,Var),!.



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

:- multifile(regression_test/0).
:- dynamic(regression_test/0).

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
   clear_shape_lib(TestID),
   % choice point created here purposely
  nb_delete('$training_vm'),
  forall(kaggle_arc(TestID,ExampleNum,_In,_Out),
  ignore((catch((call(G),
    run_arc_io(TestID,ExampleNum)),'$aborted',true)))))).


cls1:- nop(catch(cls,_,true)).

arc_grid(Grid):- test_names_by_fav(TestID),kaggle_arc_io(TestID,_ExampleNum,_,Grid).

:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)*(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')*(tst+0),"segv").
%is_buggy_pair(t('27a28665')*(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum):- Pair = TestID*ExampleNum, is_buggy_pair(Pair,Why),!,format("~N1 % Skipping ~q because: ~w ~n~n",[Pair,Why]).
run_arc_io(TestID,ExampleNum):- 
  time(train_test(TestID)),
  time(solve_test(TestID,ExampleNum)).

get_training(Training):- nb_current('$training_vm',Training),compound(Training),!.
get_training(Training):- notrace(get_current_test(TestID)),make_training(TestID,Training), nb_linkval('$training_vm',Training),!.
set_training(Training):- nb_linkval('$training_vm',Training).

make_training(TestID,VM):- 
 %make_fti(_GH,_GV,TestID,_Grid,_Sofar,_Reserved,_Options,_Points,ArgVM),
 must_det_ll((
    rb_new(RBTREE),
    duplicate_term(RBTREE,DATA),
    make_training_hints(TestID,
     training{
      pairs:_, %datatree:_, 
      current:_,test_id:TestID,
      mappings:[map],
      storage:DATA},
    VM))).

    /*
     test:ID,mappings:_,
     pre_in:_, pre_out:_,
     inC:_InC,outC:_OutC,
     removed:_,added:_, kept:_,   
     grid_in:_,grid_out:_,
   set(VM.mappings) =[map])), !. % pt(VM),nl.
  */


gather_more_task_info(TestID,S):- 
  findall(III,more_task_info(TestID,III);fav(TestID,III),L),
  flatten([L],LF),list_to_set(LF,S).
  

%show_arc_pair_progress(TestID,ExampleNum,In,Out):- show_arc_pair_progress_sol(TestID,ExampleNum,In,Out),!.
train_test:- notrace(get_current_test(TestID)), train_test(TestID).

train_test(TestID):- 
  locally(set_prolog_flag(gc,true),
 must_det_ll((
  print_testinfo(TestID),
  set_training(PrevPairEnv),
  flag(indiv,_,0),
  nb_setval(prev_pairEnv,PrevPairEnv),
  nb_delete('$training_vm'),
  get_training(Training),
  my_time(make_training_hints(TestID,Training,Dictation)),
  set_training(Dictation),
  my_time(train_using_hints(TestID,Dictation,DictOut)),
  set_training(DictOut),
  garbage_collect))).

my_time(Goal):- !,call(Goal).
my_time(Goal):- statistics:time(Goal).

train_using_hints(TestID,DictIn,DictOut):- train_for_objects(TestID,trn,0,DictIn,DictOut).
train_for_objects(TestID,Trn,N1,DictIn,DictOut):-
  (kaggle_arc(TestID,(Trn+N1),In,Out),
  N2 is N1 + 1),
 (kaggle_arc(TestID,(Trn+N2),In2,Out2) -> 
    (train_for_objects_from_pair(Dict0,TestID,[Trn,'o',N1,'o',N2],Out,Out2,Dict1),
     train_for_objects_from_pair(DictIn,TestID,[Trn,'i',N1,'i',N2],In,In2,Dict0),
    train_for_objects(TestID,Trn,N2,Dict1,DictM));
  (DictM = DictIn)),!,
  train_for_objects_from_pair(DictM,TestID,[Trn,'i',N1,'o',N1],In,Out,DictOut),!.

which_io0(i,in). which_io0(o,out).
which_io(I,In):- which_io0(I,In),!.
which_io(In,In):- which_io0(_,In),!.

train_for_objects_from_pair(Dict0,TestID,Desc,In,Out,Dict9):-
  into_monochrome(In,MonoIn0), into_monochrome(Out,MonoOut0),
  copy_term(MonoIn0,MonoIn),copy_term(MonoOut0,MonoOut),
 Desc = [_Trn,IsIO1,N1,IsIO2,N2], 
 MonoDesc = ['train_mono',IsIO1,N1,IsIO2,N2], 
  train_for_objects_from_pair_1(Dict0,TestID,Desc,In,Out,Dict1),!,
  nop(train_for_objects_from_pair_1(Dict1,TestID,MonoDesc,MonoIn,MonoOut,Dict9)),!,
   ignore(Dict1=Dict9).

individuate(VM):- 
  individuate(VM.roptions, VM.grid, Objs), 
  %set(VM.objs) = Objs,
  set(VM.objs) = Objs,
  RO = VM.roptions,
  nop(fif(\+ is_list(RO), add_shape_lib(RO,Objs))).
  


train_for_objects_from_pair_1(Dict0,TestID,Desc,InA,OutA,Dict1):-
 ((
 Desc = [Trn,IsIO1,N1,IsIO2,N2], 
 which_io(IsIO1,IO1),
 which_io(IsIO2,IO2),
 atomic_list_concat([Trn,IO1,IO2],'_',ModeIn),
 atomic_list_concat([Trn,IO2,IO1],'_',ModeOut),
 atom_concat(IO1,N1,ION1),
 atom_concat(IO2,N2,ION2),
 atomic_list_concat([Trn,ION1,ION2],'_',ExampleNum),
 pt([train_for_objects_from_pair=ExampleNum,left=ION1,right=ION2]),
 garbage_collect,
  Dict0=Dict1,

   into_grid(InA,In), into_grid(OutA,Out),!,
   name_the_pair(TestID,ExampleNum,In,Out,PairName),
 	 grid_size(In,IH,IV), grid_size(Out,OH,OV),
	 ignore((IH+IV \== OH+OV , writeln(oi(size(IH,IV)->size(OH,OV))))),

   into_fti(TestID*(Trn+N1)*IO1,ModeIn,In,InVM),!,
   into_fti(TestID*(Trn+N2)*IO2,ModeOut,Out,OutVM),!,
   show_pair_grid(yellow,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,  

  individuate(InVM),!,
  individuate(OutVM),!,
  InC = InVM.objs,
  OutC = OutVM.objs,
  %wdmsg(InC=OutC),
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  /*add_shape_lib(pair,RetainedIn),
  % add_shape_lib(pair,RetainedOut),
  add_shape_lib(removed(PairName),Removed),
  add_shape_lib(added(PairName),Added),*/
  dash_chars,dash_chars,dash_chars,dash_chars,
  show_pair_grid(cyan,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,
   max_min(IH,OH,IOH,_), max_min(IV,OV,IOV,_),
  nb_setval(no_rdot,true),
  ((Removed==Added, Removed==[]) -> pt(yellow,nothing_removed_added(PairName)) ;
   show_pair_diff_code(IOH,IOV,IOH,IOV,removed(PairName),added(PairName),PairName,Removed,Added)),
  ((RetainedIn==RetainedOut, RetainedIn==[]) -> pt(yellow,nothing_retained(PairName)) ;
   show_pair_diff_code(IH,IV,   OH, OV,retained(ION1),retained(ION2),PairName,RetainedIn,RetainedOut)),
  ((InC==OutC, InC==[]) -> pt(yellow,nothing_individuated(PairName)) ;
   show_pair_diff_code(IH,IV,   OH, OV,individuated(ION1),individuated(ION2),PairName,InC,OutC)),!,

  nb_setval(no_rdot,false),
   % pt(OutC=InC),
  dash_chars,dash_chars,dash_chars,dash_chars,
  print_testinfo(TestID))).

show_pair_diff_code(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  dash_chars,dash_chars,
  pt(purple,show_objs_as_code),
  dash_chars,
  show_objs_as_code(In),
  dash_chars,
  show_objs_as_code(Out),
  dash_chars,dash_chars.


print_testinfo(TestID):-
  ignore(((gather_more_task_info(TestID,F),forall(member(I,F),pt(test_info=I))))).

trials(learn). trials(clue). trials(human). trials(sol). trials(dsl). trials(runDSL).

sols_for(Name,Trial,Sol):- trials(Trial),Entry=..[Trial,Sol],
  gather_more_task_info(Name,Sols),member(Entry,Sols).


solve_test:- get_current_test(TestID), catch(solve_test(TestID,(tst+_)),E,wdmsg(E=solve_test(TestID,(tst+_)))),!.

solve_test(Name):- 
  fix_test_name(Name,TestID,ExampleNum),!,
  solve_test(TestID,ExampleNum).

solve_test(TestID,ExampleNum):-
 forall(kaggle_arc(TestID,ExampleNum,TestIn,ExpectedOut),
        ignore(solve_test(TestID,ExampleNum,TestIn,ExpectedOut))).

solve_test(TestID,ExampleNum,TestIn,ExpectedOut):-
   must_det_ll((    
    name_the_pair(TestID,ExampleNum,TestIn,ExpectedOut,PairName))),
   must_det_ll((    
    grid_size(TestIn,IH,IV), grid_size(ExpectedOut,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(oi(size(IH,IV)->size(OH,OV))))),
    print_testinfo(TestID))), 
  must_det_ll((    
    dash_chars, dash_chars,
    show_pair_grid(green,IH,IV,OH,OV,'Test TestIn','Solution ExpectedOut (Not computed by us)',PairName,TestIn,ExpectedOut),!,  
    get_training(Training))),
    %sflag(indiv,_,0),
    into_fti(TestID*ExampleNum*in,in,TestIn,InVM),
    dicts_join(Training,InVM,TrainingVM),
    must_det_ll((    
    print(training(Training)),nl,
    dash_chars, dash_chars,    
    print_testinfo(TestID),
    ptt("BEGIN!!!"+TestID+ExampleNum),
    forall(sols_for(TestID,Trial,SolutionProgram),
     ignore((pt(cyan,trial=Trial),
       ptt(cyan,run_dsl(TestID,Trial,SolutionProgram)),
       (run_dsl(TrainingVM,SolutionProgram,TestIn,Grid)*->true;TestIn=Grid),
       into_pipe(Grid,Solution))
       *->    
       ignore((count_difs(ExpectedOut,Solution,Errors),
        print_side_by_side(print_grid(_,_,"Our Solution",Solution),print_grid(_,_,"Expected Solution",ExpectedOut)),
           (Errors==0 -> 
              (banner_lines(green),
               arcdbg(pass(TestID,ExampleNum,SolutionProgram)),
               banner_lines(green))
            ; (banner_lines(red),
             arcdbg(fail(Errors,TestID,ExampleNum,SolutionProgram)),
               test_info(TestID,InfoF),wqnl(fav(TestID*ExampleNum,InfoF)),
               banner_lines(red)))))
       ;arcdbg(warn(unrunable(TestID,ExampleNum,SolutionProgram))))),
    ptt("END!!!"+TestID+ExampleNum))).
   


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

test_regressions:- make, forall((clause(regression_test,Body),ptt(Body)),must_det_ll(Body)).
:- add_history1(test_regressions).

%:- forall(ping_indiv_grid(X),atom_concat(X,Y
:- fixup_exports.
%:- initialization(demo,program).
%:- initialization(demo,restore_state).
%:- initialization(demo,main).
%:- initialization(demo,after_load).
:- add_history1((cls,make,demo)).

:- show_tests.
:- load_last_test_name.
