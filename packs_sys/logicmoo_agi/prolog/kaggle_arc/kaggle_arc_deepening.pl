
% make_grid(3,4)
% ===================================================================
% File 'parser_candc.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_candc.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================
% end_of_file.
%:-module(arc_deepening,

/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- export(  (
            deepen_arc/1,
            call_until_arc_failed/1,
            debug_arc_if_fail/1,
            if_arc_expanded/2,
            if_arc_expanded_ge/2,
            if_arc_expanded/1)).


:- include(kaggle_arc_header).


when_arc_expanding(Goal):- nop(Goal).

:- dynamic(pdtmp:arc_expands/3).
:- thread_local(pdtmp:expand_enabled/3).

if_arc_expanded(N):- if_arc_expanded(N,unknown).
%if_arc_expanded(N,_):- N>5, !, fail.
%if_arc_expanded(_,_):- !, fail.
if_arc_expanded(N,_Name):- flag(arc_depth,W,W), W>N,!.
if_arc_expanded(N,Name):- flag(arc_depth_skipped,W,W+1),
  nop(dmsg(skipped(if_arc_expanded(N,Name)))), fail.

:-meta_predicate(call_until_arc_failed(0)).

call_until_arc_failed([H,(!)|T]):- !,call_until_arc_failed([(H,!)|T]).
call_until_arc_failed([H|T]):- !,
  call(H)*->(call_until_arc_failed(T),!);fmt(failed(H)).
call_until_arc_failed([]).

:-meta_predicate(deepen_local_0(+,0)).
deepen_local_0(Local, Call):-
  ( \+ retract(Local) -> setup_call_cleanup(true, one_must(Call,locally(Local,Call)), ignore(retract(Local)))  ;
     (setup_call_cleanup(true,
       one_must(Call,locally(Local,Call)),
        asserta(Local)))).

%t_l:old_tex

:- thread_local(t_l:useAltPOS/0).
%t_l:useAltPOS:- fail.

%:- share_mp(deepen_arc/1).
:- export(deepen_arc/1).
:-meta_predicate(deepen_arc(0)).
% temp hack
deepen_arc_old(Call):- (Call *-> true ; (deepen_arc_0(Call) *->  true ; locally(t_l:useAltPOS,deepen_arc_0(Call)))).

% Dont use this recursively
deepen_arc(Call):- flag(arc_depth,N,N),N>0, nop(dmsg(recursive(deepen_arc(Call)))),!,call(Call).
% starting fresh from here
deepen_arc(Call):- deepen_arc_fresh(Call).

deepen_arc_fresh(Call):- 
     flag(arc_depth_skipped,Was1,0),flag(arc_depth,Was2,0), 
   call_cleanup((Call *-> true ; deepen_arc_pt2(10,Call)),
     (flag(arc_depth_skipped,_,Was1),flag(arc_depth,_,Was2))).

% this must be used *atfer* Call has been tried
deepen_arc_pt2(Upto,Call):- 
  flag(arc_depth_skipped,EN,EN), EN\==0,
  flag(arc_depth,N,N+1),
  flag(arc_depth_skipped,_,0),
 % dmsg(arc_depth=N+1),  
  ((N>Upto) -> (!,fail) ; (call(Call)*->true; deepen_arc_pt2(Upto,Call))).

:- export(deepen_arc_0/1).
:-meta_predicate(deepen_arc_0(0)).
:- thread_local(t_l:usePlTalk/0).
deepen_arc_0(Call):- deepen_local_0(t_l:usePlTalk,Call).


:- create_prolog_flag(debug_arc,false,[keep(true)]).
:-meta_predicate(debug_arc_if_fail(0)).
%debug_arc_if_fail(MG):- !, must_det_ll(MG).
debug_arc_if_fail(MG):-  strip_module(MG,M,G), locally(set_prolog_flag(no_pretty,true),debug_arc_if_fail(M,G)).

%debug_arc_if_fail(M,G):- current_prolog_flag(debug_arc,true),!, debug_arc_if_fail(Fail,IsCut,6,M,G),(IsCut==!->!;true),(Fail\==fail).
%debug_arc_if_fail(M,G):- !, M:call(G).

debug_arc_if_fail(M,(G1,G2)):- !, debug_arc_if_fail(M:G1),debug_arc_if_fail(M,G2).
debug_arc_if_fail(M,G):-   
  \+ current_prolog_flag(debug_arc,true),!,
  flag(arc_reports,_,0), 
  (call(M:G)*->true;
    (locally(set_prolog_flag(debug_arc,true), must_det_ll(M:G)))).
debug_arc_if_fail(M,G):- must_det_ll(M:G).



debug_arc_if_fail(Fail2,_,_,_,_):- Fail2==fail,!.
debug_arc_if_fail(_,!,_,_,!):-!.
debug_arc_if_fail(Fail,IsCut,N,M,(G1,G2)):- !, debug_arc_if_fail(Fail,IsCut,N,M,G1), debug_arc_if_fail(Fail,IsCut,N,M,G2).
debug_arc_if_fail(Fail2,IsCut,N,M,G):- 
 \+ current_prolog_flag(debug_arc,true),!, 
  (call(M:G)*->true;
    locally(set_prolog_flag(debug_arc,true), 
      (debug_arc_if_fail(Fail,IsCut,N,M,G)*->Fail\==fail;Fail2=fail))).
debug_arc_if_fail(Fail,_,N,M,G):- N>0, predicate_property(M:G,number_of_clause(_)), O is N -1, !, 
  (clause(M:G,Body),debug_arc_if_fail(Fail,IsCut,O,M,Body),(IsCut==!->!;true),Fail\==fail).
debug_arc_if_fail(Fail,_IsCut,_,M,G):-  must_det_ll(M:G)*->true;Fail=fail.

/*

deepen_arc_0(Call):-
  ( \+ retract(t_l:usePlTalk) -> setup_call_cleanup(true, one_must(Call,locally(t_l:usePlTalk,Call)), ignore(retract(t_l:usePlTalk)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(t_l:usePlTalk,Call)), 
        asserta(t_l:usePlTalk)))).
*/
source_loc_key(O):- prolog_load_context(term,Term),term_loc_atom(Term,T),!,gensym(T,O).
source_loc_key(O):- source_location(S,_),gensym(S,O).
source_loc_key(O):- gensym(source_loc_key,O).

term_loc_atom(T,T):- atom(T).
term_loc_atom(T,A):- \+ compound(T), term_to_atom(T,A).
term_loc_atom(_:H,T):- !, term_loc_atom(H,T).
term_loc_atom(H:-_,T):- !, term_loc_atom(H,T).
term_loc_atom(P,F):- functor(P,F,_).

if_arc_expanded_ge(if_arc_expanded(N),if_arc_expanded(N,T)):- 
  source_loc_key(T),!,source_location(S,L),
  assert_if_new(pdtmp:arc_expands(T,S,L)).

system:goal_expansion(G,O,GE,O):- compound(G),current_predicate(if_arc_expanded_ge/2),if_arc_expanded_ge(G,GE).



end_of_file.










:- dynamic(is_for_ilp/4).
:- dynamic(is_accompany_changed_db/4).
clear_scene_rules(TestID):- 
  forall(is_accompany_changed_db(TestID,IO,P,PSame),
     ignore(retract(is_accompany_changed_db(TestID,IO,P,PSame)))),!,
  clear_object_dependancy(TestID).


% Define predicates that shouldn't be noticed
dont_notice(oid(_)).
dont_notice(giz(_)).
dont_notice(global2G(_,_)).
%dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(link(contains,_)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(pg(_851136,pen(_851146),rankLS,_)).
dont_notice(iz(i_o(_))).
dont_notice(P):- compound(P),arg(_,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(F):- \+ atom(F),!,fail.
dont_notice(pg(_,iz(_),rankLS,_)).
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).

% Define predicates that should be noticed
do_notice(pg(_,_,rank1,_)).
do_notice(pg(_,_,_,_)).

% Predicate to check if P should be noticed
ok_notice(P):- \+ \+ do_notice(P),!.
ok_notice(P):- \+ dont_notice(P).


dont_deduce(occurs_in_links(_,_)).
%dont_deduce(link(sees(_),_)).
dont_deduce(giz(_)).
dont_deduce(pg(_,_,_,_)).
dont_deduce(pg(_,iz(_),rankLS,_)).
dont_deduce(size2D(_)).
%dont_deduce(global2G(_,_)).
dont_deduce(vis2D(_,_)).
dont_deduce(P):- \+ compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_gridoid(P).
dont_deduce(unique_colors_count(_)).
dont_deduce(P):- compound(P),compound_name_arguments(P,_,[X]),number(X).

% Define predicates that should be deduced
do_deduce(link(sees(_),_)).
do_deduce(rot2D(_)).
do_deduce(pen(_)).
do_deduce(iz(sid(_))).
do_deduce(P):- compound(P),compound_name_arguments(P,_,[X,Y]),number(X),number(Y).
do_deduce(rotSize2D(grav,_,_)).

% Predicate to check if P should be deduced
ok_deduce(P):- \+ \+ dont_deduce(P), !, fail.
ok_deduce(P):- \+ \+ do_deduce(P),!.
%ok_deduce(P):- \+ \+ dont_notice(P),!,fail.
%ok_deduce(_).

diff_l_r(A,B,Same,InPFlat,OutPFlat):-
 must_det_ll((
  flat_props([A],PA), flat_props([B],PB),
  noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat))).

noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat):- 
  %remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  =(PA,PA1),=(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,Same,InPFlat,OutPFlat),!.
noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  intersection(PA1,PB1,Same,InPFlat,OutPFlat),!.

propchange_unnoticable(A,B):- A=@=B,!.
propchange_unnoticable(A,B):- make_unifiable_u(A,AU),make_unifiable_u(B,BU), AU\=@=BU,!,fail.
propchange_unnoticable(A,B):- hide_propchange(A,AA),hide_propchange(B,BB),AA=@=BB,!.

bg_into_var(Var,BG,Var):- BG ==bg,!.
bg_into_var(Var,BG,Var):- is_bg_color(BG),!.
bg_into_var(_,FG,FG).

number_fg_colors(In,Out):- sub_var('@',In),!,subst(In,'@','$VAR'(0),Out),!.
number_fg_colors(In,Out):- sub_var('fg',In),!,In=Out,!.
number_fg_colors(In,Out):- mapgrid(bg_into_var('$VAR'('_')),In,Mid),In\=@=Mid,!,number_fg_colors(Mid,Out).
number_fg_colors(In,Out):- sub_var(777,In),!,copy_term(In,Mid),subst001(Mid,'$VAR'(777),'@',Out),term_variables(Out,Vs),maplist('='('$VAR'('_')),Vs),!.
number_fg_colors(In,Out):- \+ \+ (sub_term(E,In),is_real_fg_color(E)),!,  
  copy_safe(In,InC),unique_fg_colors(InC,Cs),
  Cs\==[], % at least some colors
  subst_colors_with_vars(Cs,Vs,InC,Mid),    
  ground(Cs), % fully grounded test
  numbervars(Vs,777,_,[functor_name('$VAR'),singletons(false),attvar(skip)]),!,
  number_fg_colors(Mid,Out).
number_fg_colors(InOut,InOut).

hide_propchange2(In,Out):- \+ compound(In),!,Out=In.
hide_propchange2(link(PA,_),link(PA,_)).
hide_propchange2(pg(_,P,rank1,N),pg(_,P,rank1,N)).
hide_propchange2(occurs_in_links(PA,_),occurs_in_links(PA,_)).
hide_propchange2(links_count(PA,_),links_count(PA,_)).
hide_propchange2(giz(example_num(ExampleNum)),giz(example_num(ExampleNum))).
hide_propchange2(giz(gid(_)),giz(gid(_))).
hide_propchange2(giz(A),giz(B)):- make_unifiable_u(A,B).
hide_propchange2(oid(_),oid(_)).
hide_propchange2((i_o(_)),(i_o(_))).
hide_propchange2(In,Out):- once((sub_term(E,In),is_grid(E),number_fg_colors(E,G),subst001(In,E,G,Mid))),In\=@=Mid,!,hide_propchange(Mid,Out).
hide_propchange2(grid_rep(A,G),grid_rep(A,G)).
hide_propchange2(iz(X),iz(Y)):-!,hide_propchange2((X),(Y)).
hide_propchange2(IO,IO).

hide_propchange1(P):- make_unifiable_u(P,U),!,P=@=U,!.
hide_propchange1(iz(symmetry_type(_,False))):- False == false.
hide_propchange1(iz(symmetry_type(_,False))):- False == true.
hide_propchange1(iz(P)):-!,hide_propchange1(P).
hide_propchange1(P):- \+ ok_notice(P).

hide_propchange(PA,PB):- hide_propchange2(PA,PA1),PA\=@=PA1,!,hide_propchange(PA1,PB).
hide_propchange(PA,PA).

remove_o_giz(In,Out):- \+ compound(In),!,Out=In.
remove_o_giz(obj(In),obj(Out)):- nonvar(In),!,remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- is_group(In),mapgroup(remove_o_giz,In,Out).
remove_o_giz(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
%remove_o_giz(In,Out):- remove_giz(In,Out),!.
remove_o_giz(Out,Out).


% Check if two values have the same property names but are not equal
other_val(X1,X2):- X1\=@=X2, same_prop_names(X1,X2),!.
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),!,  U1 =@= U2.

% Helper predicate to create a unifiable version of a term
make_unifiable_u(Atom,U):- is_ftVar(Atom),!,Atom=U.
make_unifiable_u(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]),!.
make_unifiable_u(X1,U1):- make_unifiable_cc(X1,U1),!.
make_unifiable_u(X1,X1).

make_unifiable_ov(I,O):- make_unifiable_u(I,O),!.

make_unifiable_f(I,O):- make_unifiable_ov(I,O).
make_unifiable_f(I,O):- same_functor(I,O),!.


has_individuals(TestID):- var(TestID), !, ensure_test(TestID), has_individuals(TestID).
has_individuals(TestID):- %use_pair_info, 
 forall(current_example_nums(TestID,ExampleNum),
  (arc_cache:individuated_cache(TestID,TID,GID,_,Objs), sub_var(ExampleNum,(TID,GID)), Objs\==[])),!.

ensure_individuals(TestID):- var(TestID),!,ensure_test(TestID),ensure_individuals(TestID).
ensure_individuals(TestID):- has_individuals(TestID),!.
ensure_individuals(TestID):- 
 time((with_individuated_cache(true,
  once((with_pair_mode(whole_test, ensure_individuals1(TestID))))))), 
   has_individuals(TestID),!.

ensure_individuals1(TestID):- has_individuals(TestID),!.
ensure_individuals1(TestID):- 
 ensure_test(TestID),
  ignore((ExampleNum=trn+_)),
  print_collapsed(200, forall( kaggle_arc(TestID,ExampleNum,GridIn,GridOut),
           individuate_pair(complete,GridIn,GridOut,_InC,_OutC))), has_individuals(TestID),!.

ensure_individuals1(TestID):- 
   once((with_pair_mode(whole_test,
    once(with_luser(menu_key,'i',once(ndividuator(TestID))))))), has_individuals(TestID),!.
ensure_individuals1(TestID):- 
  once((with_pair_mode(whole_test,
     once(with_luser(menu_key,'o',once(ndividuator(TestID))))))),has_individuals(TestID),!.
ensure_individuals1(TestID):- show_prop_counts(TestID), my_assertion(has_individuals(TestID)),!.




use_pair_info.
no_pair_info:- \+ use_pair_info.

gather_set(IO,Goal):-
  copy_term(IO+Goal,NRV+Copy),
  no_repeats_var(NRV), !, 
  call(Copy),IO=NRV.


p_to_utbs(TestID,IO,P,UTBLists):-
 findall(OutFlatProps,
  gather_set(OutFlatProps,(pair_obj_props(TestID,_ExampleNum,IO,_Step,_TypeO,_A,_B,_USame,_UPA2,OutFlatProps),member(P,OutFlatProps))),UTBLists).

:- use_module(library(ordsets)).

% common_members(+ListOfLists, -Common)
common_members([FirstList|Rest], Common) :-
    maplist(list_to_ord_set, [FirstList|Rest], OrdSets),
    foldl(ord_intersection, OrdSets, FirstList, Common).

% list_to_ord_set(+List, -OrdSet)
%list_to_ord_set(List, OrdSet) :- sort(List, OrdSet).

% Example query:
% ?- common_members([[1, 2, 3], [2, 3, 4], [1, 2, 3, 4, 5]], Common).
% Common = [2, 3].

%  is_post_objs(TestID,IO,PostObjs),include(has_prop(P),PostObjs,PostObjsO).


which_props_change(TestID,IO,P):- which_props_change(TestID,IO,P,_Step).
%  arc_cache:each_object_dependancy(TestID,ExampleNum,OD),
which_props_change(TestID,IO,P,Step):-
  ensure_individuals(TestID),
  (var(IO)->gather_set(IO,pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps));true),
  %IO = in, IO = in_out,
  %gather_set(P,(pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps),member(P,OutPFlat))).
  gather_set(P,(
   nop(gather_set(Step,(pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps)))),
      pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps),ok_deduce(P))),
  p_to_utbs(TestID,IO,P,UTBLists),  
  common_members(UTBLists,Members),
  member(P,Members),
  ignore(gather_set(Step,(pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps)))).
  



io_to_cntx(in,in_out).
%io_to_cntx(in,in_out_out).
io_to_cntx(out,in_out_out).
io_to_cntx(out,s(_)).


ensure_scene_change_rules(TestID):-
 ensure_test(TestID),
 (\+ is_accompany_changed_db(TestID,_,_,_) -> compute_scene_change(TestID) ; true).
compute_scene_change(TestID):-
 ensure_test(TestID),
 with_pair_mode(whole_test,  
 must_det_ll((
  clear_scene_rules(TestID),
  show_object_dependancy(TestID), 
  forall(which_props_change(TestID,IO,P),
    forall(prop_can(TestID,IO,P,PSame),
      assert_accompany_changed_db(TestID,IO,P,PSame)))))).

%assert_become_new(Term):- \+ clause_asserted(Term),!, pp_ilp(assert_become_new=Term), asserta_new(Term).
assert_become_new(Term):- asserta_new(Term).
%assert_become_new(Term):- pp_ilp(assert_become_new=Term),!, assert_if_new(Term).


solve_via_scene_change(TestID):-  
 must_det_ll((
  ensure_test(TestID),
  cls, %make,
  clear_scene_rules(TestID),
  %detect_pair_hints(TestID),
  time(learn_grid_size(TestID)),
  ensure_scene_change_rules(TestID),
  ExampleNum=tst+_,
  forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore(time(solve_via_scene_change_rules(TestID,ExampleNum)))), 
 !)).

solve_via_scene_change_rules(TestID,ExampleNum):-
 must_det_ll((
 ensure_scene_change_rules(TestID),
  kaggle_arc(TestID,ExampleNum,In,Expected),
  banner_lines(green,4),
  % predict_grid_size_now(TestID,In,PX,PY),
  obj_group5(TestID,ExampleNum,in,ROptions,TempObjs),TempObjs\==[],
  grid_to_tid(In,TID),
  into_fti(TID,ROptions,In,VM),
  individuate(VM),
  Objs = VM.objs,
  print_object_dependancy(TestID),
  print_scene_change_rules(TestID),
  print_ss(wqs(expected_answer(ExampleNum)),Objs,Expected), 
  %wots(SS,solve_obj_group(VM,TestID,ExampleNum,ROptions,Objs,ObjsO)),

  solve_obj_group(VM,TestID,ExampleNum,ROptions,in,Objs,ObjsO),

  dash_chars,

  print_ss(wqs(solve_via_scene_change_rules(ExampleNum)),Objs,ObjsO),
  dash_chars,
  into_solid_grid(ObjsO,OurSolution1),
  maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution),
  into_solid_grid(Expected,ExpectedOut),
  count_difs(ExpectedOut,OurSolution,Errors),
  print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution))),
  (Errors == 0 ->  
    banner_lines(green,4) 
    ;(banner_lines(red,10),!,%bt,
     !,banner_lines(red,1),
      %print_scene_change_rules(TestID),banner_lines(red,1), 
      banner_lines(red,10),!,
    fail)).
   
resize_our_solution(PX,PY,OurSolution1,OurSolution):-
  once(ground(PX+PY)
     ->resize_grid(PX,PY,OurSolution1,OurSolution)
      ;notrace(=(OurSolution1,OurSolution));notrace(trim_outside2(OurSolution1,OurSolution))).

maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution):-
  predict_grid_size_now(TestID,In,PX,PY),resize_our_solution(PX,PY,OurSolution1,OurSolution),!.


print_scene_change_rules(TestID):-
 ensure_test(TestID),
 must_det_ll((
  banner_lines(cyan,4),
  %show_assumed_mapped(TestID),
  %print_object_dependancy(TestID),
  banner_lines(cyan,3),
   Ele = is_accompany_changed_db(TestID,IO,P,PSame),
    findall_vset(Ele,prop_change_verified(TestID,IO,P,PSame),Verified),
    findall_vset(Ele,is_accompany_changed_computed(TestID,IO,P,PSame),Computed),
    findall_vset(Ele,is_accompany_changed_verified(TestID,IO,P,PSame),ACVerified),
    findall_vset(Ele,prop_can(TestID,IO,P,PSame),PropCan),
    findall_vset(Ele,is_accompany_changed_db(TestID,IO,P,PSame),Database),
    append_sets([Computed,Database,Verified,PropCan,ACVerified],Set), 
    
    variant_list_to_set(Set,VSet),
    sort(VSet,SVSet),
   forall(member(Ele,SVSet), 
   ( dash_chars,
     pp_ilp(0,
      call((
       if_t(member_eq(Ele,Database),  write('*db')),
       if_t(member_eq(Ele,PropCan),   write('*can')),
       if_t(member_eq(Ele,Computed),  write('*comp')),
       if_t(member_eq(Ele,Verified),  write('*verified')),
       if_t(member_eq(Ele,ACVerified),write('*acv')),nl))),
     pp_ilp(1,Ele),dash_chars)),
     wdmsg(print_scene_change_rules(TestID)),
  banner_lines(cyan,4))).



update_accompany_changed_db(TestID,IO,P,Kept):- Kept\==[],
   forall(retract(is_accompany_changed_db(TestID,IO,P,_)),true),
   assert_accompany_changed_db(TestID,IO,P,Kept).
   
assert_accompany_changed_db(_TestID,_IO,_P,Kept):- Kept==[],!.
assert_accompany_changed_db(TestID,IO,P,Kept):- 
   assert_become_new(is_accompany_changed_db(TestID,IO,P,Kept)).

at_least_one_overlap(DSame,PSame):-
  member(DS,DSame),member(S,PSame),
  (DS=@=S;other_val(S,DS)),!.

correct_antes1(TestID,IO,P,PSame,SL):- 
  findall(S,
   (member(S,PSame),
     \+ \+ ((
       forall((is_accompany_changed_db(TestID,IO,DP,DSame),
            at_least_one_overlap(DSame,PSame)),
          ((P==DP)-> true; (member(DS,DSame),other_val(S,DS))))))),
   SL), SL\==[],!.
correct_antes1(_TestID,_IO,_P,PSame,PSame).
   
correct_antes2(TestID,IO,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_db(TestID,IO,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   intersection(PSame,USame,Kept,_,_),Kept\==[].
correct_antes2(_TestID,_IO,_P,PSame,PSame).







solve_obj_group(VM,TestID,ExampleNum,In,ROptions,Objs,ObjsO):-
  my_maplist(copy_obj(VM,TestID,ExampleNum,In,ROptions),Objs,ObjsM),
  flatten_objects(ObjsM,ObjsO).

solve_obj_group(VM,TestID,ExampleNum,IO,ROptions,Objs,OObjs):- IO == in,!,
  my_maplist(copy_obj(VM,TestID,ExampleNum,IO,ROptions),Objs,OObjs).


solve_obj_group(VM,TestID,ExampleNum,IO,ROptions,Objs,ObjsO):-
  %trace,arc_cache:map_group(TestID,ExampleNum,IO,Group),
  GRP = grp(Info,PreObjs,Out),
  findall(GRP,(pair_obj_info(TestID,_,_IO2,Info,PreObjs,Out), sub_var(IO,Info)),Groups),
  variant_list_to_set(Groups,Set),
  banner_lines(blue,2),
  forall(member(GRP,Set),pp_ilp(GRP)),
  banner_lines(blue,2),
  %trace,prop_can(TestID,IO,P,Preconds),
  %trace,
  solve_obj_set(Set,VM,TestID,ExampleNum,IO,ROptions,Objs,ObjsO),
  flatten(ObjsO,ObjsO).

/*
*/

%copy_obj(_VM,_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IO_Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  copy_obj(VM,TestID,ExampleNum,IO_Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO).

/*
*/

prop_change_verified(TestID,IO,P,PSame):- is_accompany_changed_computed(TestID,IO,P,PSame).
/*
prop_change_verified(TestID,IO,P,PSame):-
  prop_can(TestID,IO,P,PSame),
  which_props_change(TestID,IO,P,_Step),    
  once(IO==IO;io_to_cntx(IO,IO)).
*/

copy_obj(_VM,_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
copy_obj(VM,TestID,_ExampleNum,IO,_ROptions,Obj,OObj):- 
 %trace,
 must_det_ll((
   %Agenda = agenda(IO,P,PSame),
   Agenda = P,
   findall(Agenda,
   (    prop_change_verified(TestID,IO,P,PSame),
        flatten(PSame,Rest), 
        forall(member(R,Rest),has_prop(R,Obj))),PsL),
 list_to_set(PsL,Ps), 
 % print_grid(copy_obj(Ps),Obj,OObj))),!.
 edit_object(VM,Ps,Obj,OObj))),OObj\==[],!.
%copy_obj(VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Obj,OObj):- 
%  edit_object(VM,pen([cc(black,1)]),Obj,OObj).
%copy_obj(VM,_TestID,_ExampleNum,_IO_Start,_ROptions,_Obj,[]).
copy_obj(_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Obj,Obj).


edit_object(_VM,Ps,_Obj,NewObj):- Ps==[],!,NewObj=[]. %edit_object(VM,pen([cc(black,1)]),Obj,NewObj).
edit_object(VM,Ps,Obj,NewObj):- Ps==[],!,edit_object(VM,pen([cc(black,1)]),Obj,NewObj).
edit_object(VM,Ps,Obj,NewObj):-
  must_det_ll((
   wots(SS,writeln(Ps)),
   override_object_1(VM,Ps,Obj,NewObj),
   into_solid_grid([NewObj],SG),SG=_,
   dash_chars,
   print_ss(override_object(SS),[Obj],[NewObj]),
   indv_props_list(Obj,PL1),
   indv_props_list(NewObj,PL2),
   intersection(PL1,PL2,_Same,Removed,Added),
  pp(([[removed=Removed],[added=Added]])))),!,trace.
edit_object(_VM,O,I,OO):- wdmsg(cant(edit_object(O))),I=OO,!.

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(VM,agenda(IO,P,PSame),I,O):- !, pp_ilp(IO:P-PSame), override_object_1(VM,P,I,O).
override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),
  rebuild_from_globalpoints(VM,Obj,GPoints,NewObj),!.
override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.
override_object_1(_VM,O,I,OO):- wdmsg(cant(override_object(O))),I=OO,!.

is_accompany_changed_verified(TestID,IO,P,PSame):-
  is_accompany_changed_computed(TestID,IO,P,PSame), PSame\==[].

is_accompany_changed_computed(TestID,IO,P,PSame):-
   is_accompany_changed_db(TestID,IO,P,PSame) *->true ; prop_can(TestID,IO,P,PSame). 
   
has_all_props(CanL,Obj):- maplist(inv_has_prop(Obj),CanL).
inv_has_prop(Obj,Prop):- has_prop(Prop,Obj).
   
   
%ensure_prop_change(IO,P):- (var(P)->which_props_change(_TestID,IO,P);true).
/*
in+in->out
in+in+in->out
in->out   = in_out
in+out->out
out+out->out
in->missing
*/
   
mapping_step(    in_out).
mapping_step( in_in_out).
mapping_step(in_out_out).
mapping_step(   out_out).
   
   
   
/*   
   
post_to_pre_object(TestID,IO,Post,Pre):- nonvar(Pre),var(Post),!,
  object_pair(TestID,IO,Pre,pre_post,Post).

post_to_pre_object(TestID,IO,Post,Pre):- nonvar(Post),var(Pre),!,
  object_pair(TestID,IO,Post,post_pre,Pre).
  */ 
post_to_pre_object(TestID,IO,Post,Pre):- var(Pre),var(Post),!,
  pair_obj_info(TestID,_ExampleNum,_When,Info,PreObjs,PostObjs),
  once(( sub_var(IO,Info), into_list(PreObjs,PreObjsL), into_list(PostObjs,PostObjsL))),
  member(Post,PreObjsL),member(Pre,PostObjsL).
/*
post_to_pre_object(TestID,IO,Post,Pre):- var(Post),!,
  (var(Post)-> (is_post_objs(TestID,IO,PostObjs),member(Post,PostObjs)); true),  
  post_to_pre_object(TestID,IO,Post,Pre).

post_to_pre_object(TestID,IO,Post,Pre):- 
 pair_obj_info(TestID,_ExampleNum,_When,Info,PreObjs,PostObjs),
 once(( sub_var(IO,Info), into_list(PreObjs,PreObjsL), into_list(PostObjs,PostObjsL))),
 member(Post,PreObjsL),member(Pre,PostObjsL).
   
post_to_pre_object(TestID,IO,Post,Pre):- get_obj_pair(TestID,IO,I,O),O=@=Post,Pre=I.
*//*

object_pair(TestID,IO,Obj,pre_post,Other):- 
  pair_obj_info(TestID,_ExampleNum,_When,_Info,PreObjs,PostObjs),
  member(Obj,PreObjs),member(Other,PostObjs).
object_pair(TestID,IO,Obj,post_pre,Other):- !,
  pair_obj_info(TestID,_ExampleNum,_When,_Info,PreObjs,PostObjs),
  member(Obj,PostObjs),member(Other,PreObjs).
object_pair(TestID,IO,Obj,either,Other):-
     findall(O,
       ((obj_in_or_out(Obj,out)->member(InOut,[in,out]);In=out),
         enum_object_ext(TestID,InOut,O),
         from_same_pair(This,O)),OtherObjs),
     sort_by_jaccard(Post,post_to_pre_object,OtherObjs,Others),
     member(Other,Others).

     */
from_same_pair(Post,Pre):-
  has_prop(giz(example_num(trn+N)),Post),
  has_prop(giz(example_num(trn+N)),Pre).
     
     
obj_in_or_out(Pair,IO):- is_mapping(Pair),!,
    get_mapping_info(Pair,Info,_In,_Out),arg(3,Info,IO).
obj_in_or_out(Obj,IO):- must_det_ll(is_object(Obj)),has_prop(giz(g(I_O)),Obj),!,I_O=IO.
obj_in_or_out(Obj,IO):- has_prop(iz(i_o(I_O)),Obj),!,I_O=IO.
%obj_in_or_out(Obj,I_O):- is_input_object(Obj)-> IO =out ; IO =in.

is_pre_cond_obj(Obj,in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,in);obj_in_or_out(Obj,out).
is_pre_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,s(X)):- nonvar(X), is_pre_cond_obj(Obj,out).
is_pre_cond_obj(Obj,IO):- obj_in_or_out(Obj,IO).
is_pre_cond_obj(Obj,in):-is_pre_cond_obj(Obj,in_out).

% IS POST COND
%is_post_cond_obj(Obj,in_out):- obj_in_or_out(Obj,out).
%is_post_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,out).
%is_post_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,out).
%is_post_cond_obj(Obj,s(X)):- nonvar(X), is_post_cond_obj(Obj,out).
is_post_cond_obj(Obj,_):- obj_in_or_out(Obj,out).
%is_post_cond_obj(Obj,in):- is_post_cond_obj(Obj,in_out).

enum_object_ext(TestID,IO,O):-
  ensure_test(TestID),
  current_example_nums(TestID,ExampleNum),
  once((obj_group_io(TestID,ExampleNum,IO,Objs),Objs\==[])),member(O,Objs).

has_other_value(P,O):- make_unifiable_u(P,U),has_prop(U,O),U\=P.
  %(Can == [] -> (CanL=Can1,fail) ; CanL= Can).

/*
pairs_can2(TestID,IO,P,StillCan):-  
  which_props_change(TestID,IO,P),
  make_unifiable_u(P,PU1),
  make_unifiable_u(P,PU2),  
  %findall(Keep,
    pair_obj_props(TestID,ExampleNum1,IO,_Step1,_TypeO1,_I1,O1,C1,FO1),member(P,FO1,P),     
    ((pair_obj_props(TestID,ExampleNum2,IO,_Step2,_TypeO2,_I2,O2,C2,FO2),member(P,FO2,P),O1\=O2)*->true;C1=C2),
    pair_obj_props(TestID,ExampleNum1,IO,_Step3,_TypeO3,_I3,O3,D1,FO3),member(PU1,FO3),other_val(PU1,P),
    ((pair_obj_props(TestID,ExampleNum2,IO,_Step4,_TypeO4,_I4,O4,D2,FO4),member(PU2,FO4),other_val(PU2,P),O3\=O4)*->true;D1=D2),
     intersection(C1,C2,StrongMaybeQual,DontCare1,DontCare2),
     intersection(D1,D2,MaybeDisq1,NonDisq2,NonDisq3),
     intersection(C1,D2,NonDisq1,MaybeQual5,MaybeDisq6),
     intersection(C2,D1,NonDisq4,MaybeQual8,MaybeDisq9),
     append([C1,C2],Can2), append([D1,D2],Cant), 
     append([MaybeQual5,MaybeQual8],MaybeQual),
     append([MaybeDisq1,MaybeDisq6,MaybeDisq9],MaybeDisq),
     intersection(MaybeDisq,MaybeQual,DontCare3,StillMaybeDisq,StillMaybeQual),
     append([DontCare1,DontCare2,DontCare3],DontMatter),
     append([NonDisq1,NonDisq2,NonDisq3,NonDisq4],NonDisq),
     intersection(DontMatter,StillMaybeDisq,_,_,Still2MaybeDisq),
     intersection(DontMatter,StillMaybeQual,_,_,Still2MaybeQual),
     intersection(NonDisq,Still2MaybeDisq,_,_,Still3MaybeDisq),
     intersection(Cant,Can2,_,StillCan2,_),
     intersection(StillCan2,Can,StillCan,_,_),
     Keep=[p=StillCan,n=Still3MaybeDisq],
     pp(Keep).
     */

induction_update(TestID,IO):-  
 must_det_ll((
 forall(pair_obj_props(TestID,_VxampleNum1,_IO,_Step1,_TypeO1,_I1,_O1,C1,_HD1,FO1), 
  forall(
   (member(P1,FO1),ok_deduce(P1),make_unifiable_ov(P1,P2)),
   (ignore( \+ \+ update_neg_obj_props(TestID,IO,FO1,P1,P2,C1)),
    ignore( \+ \+ update_pos_obj_props(TestID,IO,FO1,P1,P2,C1))))))),!.

nomralize_p1s(TestID,IO):-
 must_det_ll((
  %retractall(arc_cache:causes(TestID,IO,_,_,0)),
  %retractall(arc_cache:causes(TestID,IO,_,_,ignore(_,_))),
  findall_vset(P,arc_cache:causes(TestID,IO,P,_,_),Ps),
  forall(member(P,Ps),
   (arc_cache:causes(TestID,IO,P,V1,N),
    make_unifiable_ov(V1,V2),
    findall_vset(V2,arc_cache:causes(TestID,IO,P,V2,_),L),
    ignore((L=[_,_|_],some_min_unifier(L,U),
       retractall(arc_cache:causes(TestID,IO,P,V2,_)),
       asserta(arc_cache:causes(TestID,IO,P,U,N)))))))).


p1_induction(_TestID,_IO,_Likely,_P, V):- \+ ok_notice(V),!.
p1_induction(_TestID,_IO,_Likely, P,_V):- \+ ok_deduce(P),!.
p1_induction(TestID,  IO, Likely,P2,VV):- is_list(VV),!,maplist(p1_induction(TestID,IO,Likely,P2),VV).
p1_induction(TestID,  IO, Likely,P,V):- \+ arc_cache:causes(TestID,IO,P,V,_),assert(arc_cache:causes(TestID,IO,P,V,Likely)).
p1_induction(TestID,  IO, Likely,P,V):- arc_cache:causes(TestID,IO,P,V,Was), p1_induction(TestID,IO,Was,Likely,P,V),!.
p1_induction(_TestID,_IO, Same,Same,_,_):- \+ number(Same), !.
p1_induction(_TestID,_IO, ignore(_,_), _Same,_,_):-!.
p1_induction(TestID,  IO, never,W,P,V):- !, change_p1(TestID,IO,P,V,ignore(never,W)).
p1_induction(TestID,  IO, W,never,P,V):- !, change_p1(TestID,IO,P,V,ignore(W,never)).
%p1_induction(TestID,  IO, N,M,P,V):- max_min(M,N,Max,_),!, change_p1(TestID,IO,P,V,Max).
p1_induction(TestID,  IO, N,M,P,V):- !, catch(NM is N * M,_,trace),change_p1(TestID,IO,P,V,NM).

prop_can1(TestID,IO,P,S):-
  findall(N,(arc_cache:causes(TestID,IO,P,V,N),number(N)),L),
  L \== [],
  %length(L,Len),sumlist(L,Sum), Av is (Sum/Len)-1,
  Av = 2,
  findall_vset(V,(arc_cache:causes(TestID,IO,P,V,N),number(N),N>=Av),S).

update_neg_obj_props(TestID,IO,_FO1,P1,P2,C1):-
 forall((pair_obj_props(TestID,_VxampleNum2, IO,_Step2,_TypeO2,_I2,_O2,D2,_HD2,FO2),member(P2,FO2),other_val(P1,P2)),
  once((intersection(C1,D2,Ignore,P1Pos,P2Pos),
        Never = never, Likely = 1, MoreLikely = 2,
   p1_induction(TestID,IO,Never,P1,Ignore), 
   p1_induction(TestID,IO,Never,P2,Ignore),
   p1_induction(TestID,IO,Likely,P1,P1Pos), 
   p1_induction(TestID,IO,Likely,P2,P2Pos), 
   same_or_differnt(P1Pos,P2Pos,P1VPos,P2VPos,Uniq1,Uniq2),
   p1_induction(TestID,IO,MoreLikely,P1,P1VPos), 
   p1_induction(TestID,IO,MoreLikely,P2,P2VPos),      
   %p1_induction(TestID,IO,Never,P1,Uniq1), 
   %p1_induction(TestID,IO,Never,P2,Uniq2),
   true))).

update_pos_obj_props(TestID,IO,FO1,P1,P2,C1):-
 forall((pair_obj_props(TestID,_VxampleNum2, IO,_Step2,_TypeO2,_I2,_O2,D2,_HD2,FO2),FO2\=@=FO1,member(P2,FO2),P1=@=P2),
  once((intersection(C1,D2,Pos,Ignore1,Ignore2),
        Likely = 1, MoreLikely = 2,
   p1_induction(TestID,IO,MoreLikely,P1,Pos),
   p1_induction(TestID,IO,MoreLikely,P2,Pos),
   p1_induction(TestID,IO,Likely,P1,Ignore1),
   p1_induction(TestID,IO,Likely,P2,Ignore2)))).

same_or_differnt([],P2Pos,[],[],[],P2Pos).
same_or_differnt(P1Pos,[],[],[],P1Pos,[]).
same_or_differnt(P1Pos,P2Pos,[P1V|P1VPos],[P2V|P2VPos],Uniq1,Uniq2):-
  select(P1V,P1Pos,P1Rest),
  make_unifiable_ov(P1V,P2V),
  select(P2V,P2Pos,P2Rest),!,
  same_or_differnt(P1Rest,P2Rest,P1VPos,P2VPos,Uniq1,Uniq2).


change_p1(TestID,IO,P,V,S):- 
  retractall(arc_cache:causes(TestID,IO,P,V,_)), asserta(arc_cache:causes(TestID,IO,P,V,S)).

prop_can(TestID,IO,P,S):-
  which_props_change(TestID,IO,P),
  prop_can1(TestID,IO,P,S).



/*
three_differnt_val

  findall(D2,(pair_obj_props(TestID,ExampleNum1,IO,_Step3,_TypeO3,_I3,_O3,D2,FO3), member(PU1,FO3),other_val(PU1,P)),NOTOK),
   
   ,
   intersection(POS,NEG,DontCare,RPos,RNeg),
   common_members(OK,POS),
   common_members(NOTOK,NEG),
  
   pp(rpos=RPos).
*/

current_example_nums(TestID,ExampleNum):- 
  (var(TestID)->get_current_test(TestID);true),
  ignore((ExampleNum=trn+_)), kaggle_arc(TestID,ExampleNum,_,_). 



save_how_io(HowIn,HowOut):- 
  get_current_test(TestID),save_how_io(TestID,HowIn,HowOut).
save_how_io(TestID,HowIn,HowOut):- 
  assert_test_property(TestID,common,indiv_how(in),HowIn),
  assert_test_property(TestID,common,indiv_how(out),HowOut),!.



obj_group_pair(TestID,ExampleNum,InC,OutC):-
   current_example_nums(TestID,ExampleNum),
   %no_repeats_var(OutC), % set_example_num(ExampleNum),
   once((obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut)).

objs_other_than_example(TestID,ExampleNum,InOut,Others):-
  findall(O,(current_example_nums(TestID,OExampleNum),
    ExampleNum\==OExampleNum,
    obj_group_io(TestID,OExampleNum,InOut,Objs),
    member(O,Objs)),Others).

all_io_objs(TestID,InOut,Others):-
  findall(O,(current_example_nums(TestID,ExampleNum), 
   obj_group_io(TestID,ExampleNum,InOut,Objs), member(O,Objs)),Others).


obj_group_io(TestID,ExampleNum,InOut,Objs):-
 arc_test_property(TestID,common,indiv_how(InOut),How),!,
 current_example_nums(TestID,ExampleNum), 
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,InOut,How,Objs))).

obj_group_io(TestID,ExampleNum,InOut,Objs):- 
 current_example_nums(TestID,ExampleNum),
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,InOut,_,Objs))).

obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):- var(TestID),
  ensure_test(TestID),!,obj_group5(TestID,ExampleNum,InOut,ROptions,Objs).  
obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):- var(ROptions),
 arc_test_property(TestID,common,indiv_how(InOut),ROptions),!,
 obj_group5(TestID,ExampleNum,InOut,ROptions,Objs).
obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):-
 kaggle_arc_io(TestID,ExampleNum,InOut,Grid),
  set_example_num(ExampleNum),
 other_grid(Grid,Other),
 with_other_grid(Other,
  
  ((fail, arc_cache:individuated_cache(TestID,TID,GOID,ROptions,Objs), Objs\==[],
  once((testid_name_num_io_0(TID,_,Example,Num,InOut),
        testid_name_num_io_0(GOID,_,Example,Num,InOut))))*-> true ; grid_to_objs(Grid,ROptions,Objs))).


%show_object_dependancy(_TestID):-  !.
% =============================================================
show_object_dependancy(TestID):-  
% =============================================================
 ensure_test(TestID),
 learn_object_dependancy(TestID),
 print_object_dependancy(TestID).

:- dynamic(arc_cache:prop_dep/9).
:- dynamic(arc_cache:causes/5).

pair_obj_props(TestID,ExampleNum,IO,Step,TypeO,A,B,USame,InFlatProps,OutFlatProps):-
  Info = info(Step,_IsSwapped,IO,TypeO,TestID,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,A,B,USame,InFlatProps,OutFlatProps).

pair_obj_info(TestID,ExampleNum,IO,Info,A,B):-
  arc_cache:prop_dep(TestID,ExampleNum,IO,Info,A,B,_USame,_UPA2,_UPB2).



% =============================================================
learn_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
  must_det_ll((
  ensure_individuals(TestID),
  ignore((ExampleNum=trn+_)),
  retractall(arc_cache:causes(TestID,_,_,_,_)),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     learn_object_dependancy(TestID,ExampleNum)),
  induction_update(TestID,in_out),
  induction_update(TestID,in_out_out),
  induction_update(TestID,s(in_out_out)),
  nomralize_p1s(TestID,in_out),
  nomralize_p1s(TestID,in_out_out),
  nomralize_p1s(TestID,s(in_out_out)))).

learn_object_dependancy(TestID,ExampleNum):-
  current_example_nums(TestID,ExampleNum),
  must_det_ll((obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
  maybe_learn_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs))).

maybe_learn_object_dependancy(TestID,ExampleNum,_LHSObjs,_RHSObjs):- 
  arc_cache:prop_dep(TestID,ExampleNum,_,_,_,_,_,_,_),!.

maybe_learn_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs):-
  RHSObjs\==[],LHSObjs\==[],
  learn_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs).

learn_object_dependancy(TestID,ExampleNum,LHSObjs,RHSObjs):- 
 must_det_ll((
  Step=0,IO=in_out,IsSwapped=false,
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,IO,[],LHSObjs,RHSObjs,Groups),
  assert_map_pairs(TestID,ExampleNum,IO,Groups))).

assert_map_pairs(TestID,ExampleNum,IO,Group):- is_list(Group),!,maplist(assert_map_pairs(TestID,ExampleNum,IO),Group).
assert_map_pairs(TestID,ExampleNum,IO,grp(Info,In,Out)):-
  into_list(In,InL),into_list(Out,OutL),
  once((diff_l_r(InL,OutL,Same,InPFlat,OutPFlat),
   unnumbervars(('$VAR'(0),'$VAR'('_'),Same,InPFlat,OutPFlat),UNV))),
   UNV = (_FG1,_BG1,USame,InFlatProps,OutFlatProps),
  %pp_ilp(grp(Info,InL,OutL)),!,
  assertz_new(arc_cache:prop_dep(TestID,ExampleNum,IO,Info,InL,OutL,USame,InFlatProps,OutFlatProps)),!.

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
 dash_chars,dash_chars,
 retractall(arc_cache:causes(TestID,_,_,_,0)),
 retractall(arc_cache:causes(TestID,_,_,_,0.0)),
 retractall(arc_cache:causes(TestID,_,_,_,never)),
 retractall(arc_cache:causes(TestID,_,_,_,ignore(_,_))),
 listing(arc_cache:causes/5),
 dash_chars,dash_chars,
 findall_vset(grp(Info,Pre,Post),pair_obj_info(TestID,_,_,Info,Pre,Post),Set),
 maplist(pp_ilp,Set),
 dash_chars,dash_chars.

  
/*
show_cp_diff(PA,[PB]):- !, show_cp_diff(PA,PB).
show_cp_diff([PA],PB):- !, show_cp_diff(PA,PB).
show_cp_diff([],_).
show_cp_diff([P|A],PB):- !, show_cp_diff(P,PB),show_cp_diff(A,PB).
*/
show_cp_diff(A,B):-
 must_det_ll((
  flat_props([A],PA), flat_props([B],PB),
  diff_l_r(PA,PB,Same,InPFlat,OutPFlat),  
  %flat_props([B],PB), intersection(Same,PB,S,SS,_), append(S,SS,SSame),
  %maplist(print_diffs(1),Same),
  length(Same,SL), pp(sames=SL),  
  length(InPFlat,LenA),pp(removed(LenA)=InPFlat),
  length(OutPFlat,LenB),pp(added(LenB)=OutPFlat),
  !)).


pp_ilp(Grp):-pp_ilp(1,Grp),!.

pp_ilp(_,_):- format('~N'),nl,fail.
pp_ilp(D,T):-  is_ftVar(T),!,prefix_spaces(D,print(T)),!.
pp_ilp(D,call(T)):- !, prefix_spaces(D,call(T)).
% pp_ilp(D,Grp):- is_mapping(Grp), prefix_spaces(D,print(Grp)),!.
pp_ilp(D,Grp):- is_mapping(Grp), !,
 must_det_ll((
  get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<grp ~w>\n',[Info]))),
    print_io_terms(D+7,In,Out),
    prefix_spaces(D+8,show_cp_diff(In,Out)),
  prefix_spaces(D,(write('</grp>\n'),dash_chars)))).

pp_ilp(D,Grid):- is_group(Grid),!, 
  must_det_ll((length(Grid,Len),
   prefix_spaces(D,(format('<group ~w>\n',[len=Len]))),
   prefix_spaces(D,mapgroup(pp_ilp(D+7),Grid)),!,nl,
   prefix_spaces(D,(format('</group>\n',[]))))),!.

pp_ilp(D,Grid):- is_grid(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_object(Grid),!,prefix_spaces(D,print_grid([Grid])),!,nl.

pp_ilp(D,is_accompany_changed_db(_TestID,IO,P,PSame)):- !,
 must_det_ll((
  once(io_to_cntx(IO,CTX);IO=CTX),
  once(list_to_conjuncts(PSame,Conj);PSame=Conj),
  prefix_spaces(D,(print((CTX:P)),prefix_spaces(D+10,(portray_clause((:-Conj)))))))),!.


%pp_ilp(D,(H:-Conj)):- prefix_spaces(D,pp(H:-Conj)),!.
pp_ilp(D,(H:-Conj)):- prefix_spaces(D,(portray_clause(H:-Conj))),!.


%pp_ilp(D,T):- true,!, prefix_spaces(D,print(T)),!.

pp_ilp(D,Grid):- is_group(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.

pp_ilp(D,List):- is_list(List), \+ is_grid(List),maplist(pp_ilp(D+3),List).
%pp_ilp(D,T):- into_solid_grid_strings(T,G),!, prefix_spaces(D,print(G)),!.
pp_ilp(D,T):- prefix_spaces(D,print(T)),!.

is_grid_or_group(Grid):- is_grid(Grid),!.
is_grid_or_group(Grid):- is_group(Grid),!.

print_io_terms(D,In,Out):-
  once(into_solid_grid_strings_1(In,ITerm)),
  once(into_solid_grid_strings_1(Out,OTerm)),
  once(ITerm\=@=In;Out\=@=OTerm),!, print_io_terms(D,ITerm,OTerm).

print_io_terms(D,loc2D(X,Y,ITerm),loc2D(OX,OY,OTerm)):- 
    \+ is_mapping(ITerm), \+ is_mapping(OTerm),
    prefix_spaces(D,print_ss("",ITerm,loc2D(X,Y),OTerm,loc2D(OX,OY))),!.

print_io_terms(D,ITerm,OTerm):-  
    is_grid_or_group(ITerm),is_grid_or_group(OTerm),
    prefix_spaces(D,print_ss("",ITerm,OTerm)),!.

print_io_terms(D,ITerm,OTerm):-
    prefix_spaces(D,pp_ilp(ITerm)),
    prefix_spaces(D,pp_ilp(OTerm)),!.

print_io_terms(D,ITerm,OTerm):- 
    prefix_spaces(D,print_ss("",call(pp_ilp(ITerm)),call(pp_ilp(OTerm)))),!.

%prefix_spaces(D,G):- fail, DD is D, wots(Tabs,(write('\t'),print_spaces(DD),write('.\t'))), wots(SS,G),!, print_prepended(Tabs,SS).
prefix_spaces(D,G):- DD is D, wots(Tabs,(write('\t'),print_spaces(DD),write('\t'))),prepend_each_line(Tabs,G).

into_solid_grid_strings_1(X,Y):- into_solid_grid_strings(X,Y),!.

/*into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),Obj\=@=T,is_mapping(Obj),
  into_solid_grid_strings(Obj,Grid),!,
  subst001(T,Obj,Grid,MidTerm),



  into_solid_grid_strings(MidTerm,WithGrids).*/
prin_to_string(T,Text):- term_contains_ansi(T),Text=T,!.
prin_to_string(T,Text):- wots(Text,print(T)). 

into_solid_grid_strings(T,Text):- is_ftVar(T),Text=T,!.
%into_solid_grid_strings(T,Text):- \+ compound(T),T=Text,!.
%into_solid_grid_strings(T,Text):- term_contains_ansi(T),Text=T,!.
%into_solid_grid_strings(T,Text):- as_is(T),T=Text,!.
%into_solid_grid_strings(T,Text):- is_object(T),object_color_glyph_long(T,Text),!.
%into_solid_grid_strings(T,Text):- is_object(T),as_grid_string(T,Text),!.
%into_solid_grid_strings(T,Text):- is_object(T),into_solid_grid_str(T,Text),!.
%into_solid_grid_strings(g rp(T),gr p(Text)):- is_list(T), wots(Text,print_ss(T)),!.
%into_solid_grid_strings(g rp(T),g rp(Text)):- is_list(T), maplist(into_solid_grid_strings,T,Text),!.
%into_solid_grid_strings(g rp(T),g rp(Text)):- is_list(T), prin_to_string(T,Text),!.
into_solid_grid_strings([T],WithGrids):- is_grid(T), !, into_solid_grid_strings(T,WithGrids).
into_solid_grid_strings([T],WithGrids):- \+ is_grid([T]), !, into_solid_grid_strings(T,WithGrids).
into_solid_grid_strings(T,WithGrids):-
  sub_term(TObj,T), compound(TObj), \+ is_list(TObj),
  arg(_,TObj,Obj), is_object(Obj), 
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
into_solid_grid_strings(T,WithGrids):- fail,
  sub_term(Obj,T),is_grid(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
/*
into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),is_mapping(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
*/
%into_solid_grid_strings(MidTerm,WithGrids):- into_solid_grid_str(MidTerm,WithGrids). 
into_solid_grid_strings(WithGrids,WithGrids).
%  \+ arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),

need_positional_context(H,V):- (H=<3;V=<3),!.
need_positional_context(H,V):- (H=<12,V=<12),!.
need_positional_context(_H,_V).


into_solid_grid_str([Obj,Obj2],SS):- fail, is_object(Obj),is_object(Obj2),
 into_solid_grid_str(Obj,Grid1),
 into_solid_grid_str(Obj2,Grid2),
 wots(SS,print_ss(Grid1,Grid2)),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),
 vis2D(Obj,H,V), vis2D(Obj,H,V),has_prop(giz(g(IO)),Obj),
 (need_positional_context(H,V)->global_grid(Obj,GG);=(Obj,GG)),
  as_grid_string(GG,Grid), =((loc2D(IO,X-Y,Grid)),SS),!.

%into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),into_solid_grid(Obj,Grid), =((loc2D(X-Y,Grid)),SS),!.
into_solid_grid_str(Grid,GridStr):- into_solid_grid(Grid,Solid),Solid\=@=Grid,into_solid_grid_str(Grid,GridStr). %,wots(GridStr,(nl,print_grid(Grid))).
%into_solid_grid_str(Grid,(GridStr)):- as_grid_string(Grid,GridStr),!.%print_wots(GridStr,(nl,print_grid(Grid))).
into_solid_grid_str(O,O).

% =============================================================
clear_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((clear_object_dependancy(TestID,ExampleNum)))).
clear_object_dependancy(TestID,ExampleNum):-  
  forall(arc_cache:prop_dep(TestID,ExampleNum,IO,Info,Right,Left,A,B,C),
    retract(arc_cache:prop_dep(TestID,ExampleNum,IO,Info,Right,Left,A,B,C))).





% sort_by_generation(Grps,SortedByGen):-predsort(sort_on(by_generation),Grps,SortedByGen).
sort_by_generation(Grps,Grps).

maybe_remove_bg(RHSObjs,RHSObjs1):- my_partition(is_fg_object,RHSObjs,RHSObjs1,Rest),RHSObjs1\==[],Rest\==[],!.
%maybe_remove_bg(RHSObjs,RHSObjs1):- include(is_fg_object,RHSObjs,RHSObjs1),RHSObjs1\=@=RHSObjs,!.
maybe_remove_bg(RHSObjs,RHSObjs).

fg_to_bgc(FG,black):- is_fg_color(FG),!.
fg_to_bgc(FG,FG):- \+ compound(FG),!.

into_delete(_Info,Obj,Obj):- is_mapping(Obj),!.
into_delete(_Info,_Obj,[]). % grp(Info,[Obj],[])).

into_delete(_TestID,_ExampleNum,_IsSwapped,_Step,_Ctx,_Prev,_Info,Obj,Obj):- is_mapping(Obj),!.
into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,_Info,Obj,Pairs):- map_pred(fg_to_bgc, Obj,NewObj),
  make_pairs(TestID,ExampleNum,Ctx,IsSwapped,Step,in_out,Prev,Obj,NewObj,Pairs),
  !. %edit_object(pen([cc(black,1)]))  % grp(Info,[Obj],[])).

is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- is_functor(grp,Grp).

get_mapping_info(grp(Info,In,Out),Info,In,Out).
get_mapping_info_list(GRP,Info,InOutO):-
  get_mapping_info(GRP,Info,In,Out),
  into_list(In,InL),into_list(Out,OutL),!,
  append_LR(OutL,InL,InOutL),!,
  must_det_ll((InOutL=InOutO)).


append_LR(Prev,Mappings,RestLR):- 
  flatten([Prev,Mappings],RestLR),!.

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  maybe_remove_bg(RHSObjs,RHSObjs1), \=@=(RHSObjs,RHSObjs1),!,
  must_det_ll((calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs1,RestLR))).


calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   Info = info(Step,IsSwapped,Ctx,leftover,TestID,ExampleNum),
   RHSObjs==[], !, must_det_ll((maplist(into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,Info),
     LHSObjs,Mappings),append_LR(Prev,Mappings,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,_Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], !, must_det_ll((
    incr_cntx(Ctx,IncrCtx),
    %incr_step(Step,IncrStep),
    calc_o_d_recursively(TestID,ExampleNum,IsSwapped,10,IncrCtx,Prev,Prev,RHSObjs,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,[Right],[Pairs|RestLR]):- fail,
 must_det_ll((
  make_pairs(TestID,ExampleNum,assumed,IsSwapped,Step,Ctx,[],[Prev,LHSObjs],Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,NewPrev,LHSObjs,[],RestLR))).


calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,[Pairs|RestLR]):-
 select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
 must_det_ll((
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  incr_step(Step,IncrStep),
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,IncrStep,Ctx,NewPrev,LHSRest,RHSRest,RestLR))).
 
calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,[Pairs|RestLR]):-
 must_det_ll((
  select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  incr_step(Step,IncrStep),
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,IncrStep,Ctx,NewPrev,LHSRest,RHSRest,RestLR))).



%incr_cntx(Ctx,NewCtx):- atom(Ctx),!, atom_concat(Ctx,'_out',NewCtx).
incr_cntx(Ctx,Next):- number(Ctx),!, plus(Ctx,1,Next).
incr_cntx(Ctx,Next):- Ctx == in_out,!, Next=in_out_out.
incr_cntx(W+Ctx,W+Next):- incr_cntx(Ctx,Next).
incr_cntx(Ctx,s(Ctx)).
incr_step(Ctx,Next):- incr_cntx(Ctx,Next).
swap_tf(Ctx,s(Ctx)).

%select_some(0,[],L,L).
select_some(1,[E],L,R):- select(E,L,R).  
select_some(2,[A,B],L,R):- select(A,L,R1),select(B,R1,R),A@<B.
select_some(3,[A,B,C],L,R):- select_some(2,[A,B],L,R1),select(C,R1,R),B@<C.
select_some(N,[A,B,C,D|More],L,R):- length(L,Max),between(4,Max,N),select_some(3,[A,B,C],L,R1),
  plus(M,3,N),select_some(M,[D|More],R1,R),C@<D.

in_to_ins(Ins,N,InsList):-
 findall(E,select_some(N,E,Ins,_),InsList).

%select_pair(perfect,_Prev,[A],[B],A,B,[],[]):-!.
select_pair(perfect,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  \+ is_mapping(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(perfect_w_prev,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,[Prev|LHSObjs],RestLeft),
  \+ is_mapping(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(perfect_combo,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-  
  into_list(LHSObjs,LHSObjsL),variant_list_to_set(LHSObjsL,LHSObjsSet),
  in_to_ins(LHSObjsSet,2,LHSObjs_Combos),
  select(Left,LHSObjs_Combos,LHSObjs_Combos_Rest),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),  
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(LHSObjs_Combos_Rest,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs_Combos,[LeftMaybe|_]))),
  LeftMaybe = Left,!.


select_pair(need_prev,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  sort_by_jaccard(Prev,Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(from_left,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

select_pair(from_right,Prev,LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

remove_object(RHSObjs,[Left|More],RHSObjsMI):- 
  remove_object(RHSObjs,Left,Rest),!,remove_object(Rest,More,RHSObjsMI).
remove_object(RHSObjs,Left,RHSObjsMI):- select(Left,RHSObjs,RHSObjsMI),!.
remove_object(RHSObjs,_,RHSObjs).

prime_factor(N, D) :-
    find_prime_factor(N, 2, D).

find_prime_factor(N, D, D) :- 0 is N mod D.
find_prime_factor(N, D, R) :- D < N,
    (0 is N mod D
    -> (N1 is N/D, find_prime_factor(N1, D, R))
    ;  (D1 is D + 1, find_prime_factor(N, D1, R))
    ).

split_sorted(Objs,SplitLHS,SplitRHS):- 
  my_partition(is_bg_object,Objs,SplitLHS,SplitRHS), SplitLHS\==[], SplitRHS\==[].

split_sorted(Objs,SplitLHS,SplitRHS):-
 length(Objs,Len),
 prime_factor(Len,Prime),
 split_sorted_by_len(Objs,Len,Prime,SplitLHS,SplitRHS).

split_sorted_by_len(Objs,_Len,Prime,SplitLHS,SplitRHS):- 
 variance_counts(Objs,PropObjsounts),
 pp(PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,E)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

split_sorted_by_len(Objs, Len,Prime,SplitLHS,SplitRHS):- 
 Half is Len div Prime,
 count_each_value(Objs,PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,Half)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

into_prop(CC,P):- sub_term(E,CC),compound(E),is_prop1(E),!,E=P.

%make_pairs(TestID,ExampleNum,Type,s(IsSwapped),Step,Ctx,Prev,LHS,RHS,GRP):- nonvar(IsSwapped),!,
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,RHS,LHS,GRP).
%make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,LHS,RHS,GRP):- Prev\==[], !, 
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],Prev,LHS,NLHS),
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],NLHS,RHS,GRP).
make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,_Prev,LHS,RHS,GRP):-
  Info = info(Step,IsSwapped,Ctx,TypeO,TestID,ExampleNum),
  listify(LHS,LHSL),maplist(obj_in_or_out,LHSL,LCtx),atomic_list_concat(LCtx,'_',LP),
  listify(RHS,RHSL),maplist(obj_in_or_out,RHSL,RCtx),atomic_list_concat([Type,LP|RCtx],'_',TypeO),
  
  %into_list(LHS,LLHS),
  %append_LR(Prev,LHS,PLHS),
  GRP = grp(Info,LHS,RHS).




saved_group(Why,IndvS):-
  is_why_grouped(_TestID,_Count,Why,IndvS).

is_why_grouped(TestID,Count,Why,IndvSO):-
  is_why_grouped_g(TestID,Count,Why,IndvSG),
  once(maplist(must_oid_to_object,IndvSG,IndvS)),
  IndvSO=IndvS.

must_oid_to_object(ID,O):- must_det_ll(oid_to_obj(ID,O)).

save_grouped(Why,G):-
  into_group(G,GS),
  get_current_test(TestID),
  length(GS,Len),
  mapgroup(register_obj,GS),
  maplist(obj_to_oid_u,GS,GGG),
  %maplist(obj_to_oid,GS,OIDs),
  my_asserta_if_new(is_why_grouped_g(TestID,Len,Why,GGG)).

obj_to_oid_u(Obj,OID):- obj_to_oid(Obj,OID).

normal_group_form(Group,Group):-!.

:- dynamic(is_why_grouped_g/4).
why_grouped(Why,Group):-
  ensure_test(TestID),
  why_grouped(TestID,Why,Group).

why_grouped(TestID,Why,Group):- 
  (is_why_grouped(TestID,_,Why,Group)*->true; 
     ((is_list(Group)->length(Group,Len);true),is_why_grouped(TestID,Len,Why,Grp),same_members(=@=,Group,Grp))).

same_members(P2,G1,G2):- 
  select(E1,G1,GG1),select(E2,G2,GG2),
  call(P2,E1,E2), same_members(P2,GG1,GG2).

%select_group(TestID,Group,How):- no_repeats(Group,select_group0(TestID,Group,How)).
select_group(TestID,Group,How):- select_group0(TestID,Group,How).
select_group0(TestID,Group,How):-
  ((is_why_grouped(TestID,_,How1,Group1), % dif(Group1,Group2), 
    is_why_grouped(TestID,_,How2,Group2),
    Group1\==[], Group2\==[],
    Group1\==Group2,
    once((sub_term(E,How1),sub_var(E,How2))),
    %length(Group1,G1), length(Group2,G2), G1>G2,
  once((sub_term(E,How1),sub_var(E,How2))),
  %member(M1,Group1),member(M2,Group2),M1=M2,
  append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
  How = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,How,Group).

select_group0(TestID,Group,obj_cache):- findall(O,obj_cache(TestID,O,_),GroupJ), GroupJ\==[], sort_safe(GroupJ,Group).



  





















compare_objects([],[]):-!.
compare_objects(Objs,Interesting):- 
  maplist(indv_props_for_noteablity,Objs,ObjProps),
  flatten(ObjProps,FlatProps),
  maplist(functorize_props,FlatProps,Functors),
  sort_safe(Functors,SortedFunctors),
  gather_props(SortedFunctors,FlatProps,ListOfLists),
  maplist(compare_values,ListOfLists,Diffs),
  include(\=([]),Diffs,Interesting).
  
functorize_props(iz(P),FA):- !, functorize_props(P,FA).
functorize_props(P,F/A):- functor(P,F,A).
gather_props([F/A|SortedFunctors],FlatProps,[(F-Candidates)|ListOfLists]):- 
  functor(Match,F,A), findall(Match,(member(Match,FlatProps);member(iz(Match),FlatProps)),Candidates),
  gather_props(SortedFunctors,FlatProps,ListOfLists).
gather_props([],_,[]).


compare_values(F-P,Notable):- predsort_using_only(number_varz,P,S),length(P,N),length(S,NS),
  is_notable(F-NS/N,Notable).

:- dynamic(repress_non_notables/0).
is_changeable_param(repress_non_notables/0).
repress_non_notables.

:- dynamic(never_noteable/1).
is_changeable_param(never_noteable/1).
never_noteable(colors_cc).
never_noteable(globalpoints).
never_noteable(P):- compound(P),functor(P,F,_),never_noteable(F).

is_prop_for_noteablity(P):- compound(P),functor(P,F,_),is_prop_for_noteablity(F),!.
is_prop_for_noteablity(P):- \+ never_noteable(P),!.

is_notable(_F-N/N,[]):- repress_non_notables, !.  
is_notable(_F-1/_,[]):- repress_non_notables, !.
is_notable(F-_,[]):- never_noteable(F),!.
is_notable(F-N/N,all_diff(F)):-!.
is_notable(F-1/_,all_same(F)):-!.
is_notable(F-S/N,notable(F,S/N)):-!.
%is_notable(F-S/N,Notable):- F-S/N = Notable.

   number_varz(I,C):- copy_term(I,C),numbervars(C,0,_,[attvar(skip)]).

:- style_check(+singleton).

found_in_w(Trait,List,L):- 
  findall(E,(member(_-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait = E) ),L).

found_in_o(Trait,List,L):- 
 findall(Obj,(member(Obj-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait =@= E)),L).


%each_1trait(Obj,self(Obj)).
each_1trait(Var,T):- var(Var),!, enum_object(Var),each_1trait(Var,T).
each_1trait(obj(L),T):- !, each_1trait(L,T).
each_1trait(iz(L),T):-  !, each_1trait(L,T).
each_1trait(L,T):- is_list(L),!,member(E,L),each_1trait(E,T).

each_1trait(T,T):- \+ too_verbose(T). 

each_trait(Obj,Obj-S):- findall(T,each_1trait(Obj,T),L),list_to_set(L,S).

get_peers(Obj,Peers):- 
  get_current_test(TestID),select_group(TestID,Group,_How), select(Obj,Group,Peers).
peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(is_peerless_prop(PeersU),Props,PeerlessProps))).
not_peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(not_peerless_prop(PeersU),Props,PeerlessProps))).

is_peerless_prop(Peers,P):- \+ sub_var(P,Peers).
not_peerless_prop(Peers,P):- sub_var(P,Peers).


too_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_unique(F).
%too_unique(obj_to_oid).
too_unique(globalpoints).
%too_unique(o).
too_unique(link).
too_unique(obj_to_oid).
too_unique(/*b*/iz).
%good_overlap(colorlesspoints).

good_overlap(P):- compound(P),!,compound_name_arity(P,F,_),!,good_overlap(F).
good_overlap(localpoints).
good_overlap(rot2D).

too_non_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_non_unique(F).
too_non_unique(grid_size).
too_non_unique(grid_sz).
%too_non_unique(/*b*/iz).
too_non_unique(grid).
too_non_unique(changes).

%too_non_unique(mass).

length_criteria(List,P):- compound(P), P=..[F,n,L],C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), P=..[F,L], C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), length(List,I), !, call(call,P,I).
length_criteria(List,N):- length(List,N).

tesT_compare_objects:- compare_objects([
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),
      vis2D(1,1),rot2D(sameR),loc2D(4,9),changes([]),iz(type(dots)),iz(type(dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,9),% obj_to_oid(t(af902bf9)>(tst+0)*in,37),globalpoints([yellow-point_04_09]),
      grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(4,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,6),obj_to_oid(t(af902bf9)>(tst+0)*in,39),globalpoints([yellow-point_04_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(1,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(1,6),obj_to_oid(t(af902bf9)>(tst+0)*in,40),globalpoints([yellow-point_01_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,5),obj_to_oid(t(af902bf9)>(tst+0)*in,41),globalpoints([yellow-point_10_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,5),obj_to_oid(t(af902bf9)>(tst+0)*in,42),globalpoints([yellow-point_06_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,1),obj_to_oid(t(af902bf9)>(tst+0)*in,43),globalpoints([yellow-point_10_01]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,1),obj_to_oid(t(af902bf9)>(tst+0)*in,44),globalpoints([yellow-point_06_01]),grid_size(10,10),iz(important)])],
    OUTPUT),
  print(OUTPUT).

is_fti_step(most_unique).
most_unique(symmetry_type,VM):-
  List = VM.objs,
  last(List,Obj),
  set(VM.solution)= Obj.

maplist_e(P2,A,B):- is_list(A),!,mapgroup(P2,A,B).
maplist_e(P2,A,B):- call(P2,A,B).

obj_exclude(Obj,Group,Others):- var(Obj),!,select(Obj,Group,Others).
obj_exclude(Obj,Group,Others):- select(O,Group,Others),(O==Obj *-> true; Group=Others).


  
/*

into_lst(ObjsL,[]):- ObjsL==[],!.
into_lst(ObjsL,[ObjsL]):- \+ compound(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_gridoid(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_grid(ObjsL),!.
into_lst(ObjsL,Lst):- is_list(ObjsL),!,maplist(into_lst,ObjsL,LstL),append(LstL,Lst).
into_lst(Grp,Lst):- is_mapping(Grp), get_mapping_info_list(Grp,_,List),!,into_lst(List,Lst).
into_lst(Grp,Lst):- arg(_,Grp,List),is_list(List),!,into_lst(List,Lst).
into_lst(ObjsL,[ObjsL]).

%solve_obj(_VM,_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,IO,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IO_Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,IO,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  solve_obj(VM,TestID,ExampleNum,IO_Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO).


*/
