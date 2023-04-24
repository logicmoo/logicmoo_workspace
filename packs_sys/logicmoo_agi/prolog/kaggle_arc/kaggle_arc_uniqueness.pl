/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- include(kaggle_arc_header).

:- dynamic(is_for_ilp/4).
:- dynamic(is_accompany_changed_db/4).
clear_scene_rules(TestID):- 
  forall(is_accompany_changed_db(TestID,IO,P,PSame),
     ignore(retract(is_accompany_changed_db(TestID,IO,P,PSame)))),!,
  clear_object_dependancy(TestID).

count_of(G,N):- findall(G,G,L),variant_list_to_set(L,S),length(S,N).

dont_notice(oid(_)).
dont_notice(giz(_)).
dont_notice(global2G(_,_)).
%dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(link(contains,_)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(iz(i_o(_))).
dont_notice(P):- compound(P),arg(_,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(F):- \+ atom(F),!,fail.
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).

do_notice(pg(_,_,rank1,_)).
do_notice(pg(_,_,_,_)).

ok_notice(P):- \+ \+ do_notice(P),!.
ok_notice(P):- \+ dont_notice(P).


dont_deduce(link(sees(_),_)).
dont_deduce(giz(_)).
dont_deduce(size2D(_)).
dont_deduce(global2G(_,_)).
dont_deduce(vis2D(_,_)).
dont_deduce(P):- \+ compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_gridoid(P).
dont_deduce(unique_colors_count(_)).
dont_deduce(P):- compound(P),compound_name_arguments(P,_,[X]),number(X).

do_deduce(link(sees(_),_)).
do_deduce(rot2D(_)).
do_deduce(pen(_)).
do_deduce(iz(sid(_))).
do_deduce(P):- compound(P),compound_name_arguments(P,_,[X,Y]),number(X),number(Y).
do_deduce(rotSize2D(grav,_,_)).

ok_deduce(P):- \+ \+ dont_deduce(P), !, fail.
ok_deduce(P):- \+ \+ do_deduce(P),!.
%ok_deduce(P):- \+ \+ dont_notice(P),!,fail.
%ok_deduce(_).

other_val(X1,X2):- X1\=@=X2, same_prop_names(X1,X2),!.
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),  U1 =@= U2.

make_unifiable_u(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]).
make_unifiable_u(X1,U1):- make_unifiable_cc(X1,U1),!.



has_propcounts(TestID):- 
 forall(current_example_nums(TestID,ExampleNum),
  ( \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(in,IO)),
    \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(out,IO)))).

%ensure_propcounts(_TestID):-!.
ensure_propcounts(TestID):- var(TestID),!,ensure_test(TestID),ensure_propcounts(TestID).
ensure_propcounts(TestID):- has_propcounts(TestID),!.
ensure_propcounts(TestID):- 
 wots(_HIDE,once((with_pair_mode(whole_test,
    once(with_luser(menu_key,'i',once(ndividuator(TestID)))))))),
  once((with_pair_mode(whole_test,
     once(with_luser(menu_key,'o',once(ndividuator(TestID))))))),
 has_propcounts(TestID),!.
ensure_propcounts(TestID):- show_prop_counts(TestID), my_assertion(has_propcounts(TestID)),!.

%props_change(TestID,IO,P):- fail.
%  arc_cache:each_object_dependancy(TestID,ExampleNum,OD),

props_change(TestID,IO,P):-
  ensure_propcounts(TestID),
  %ensure_prop_change(E),
  findall(Q-I_or_O,counts_change(TestID,_,I_or_O,Q,_,_),L),list_to_set(L,S),!,member(P-IO,S),ok_deduce(P).
%ensure_prop_change(IO,P):- (var(P)->props_change(_TestID,IO,P);true).

in_out_atoms(in,out).

counts_change(TestID,ExampleNum,In,P,N2,N1):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, Out, count, N1, P), ok_deduce(P),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, In, count, N2, P) -> true ; N2=0), N1\==N2.

counts_change(TestID,ExampleNum,Out,P,N1,N2):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, In, count, N1, P), ok_deduce(P),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, Out, count, N2, P) -> true ; N2=0), N1\==N2.

ensure_scene_change_rules(TestID):-
 ensure_test(TestID),
 (\+ is_accompany_changed_db(TestID,_,_,_) -> compute_scene_change(TestID) ; true).
compute_scene_change(TestID):-
 ensure_test(TestID),
 with_pair_mode(whole_test,  
 must_det_ll((
  clear_scene_rules(TestID),
  banner_lines(red,4),
  compute_scene_change_pass1(TestID),  
  banner_lines(orange,4),
  compute_scene_change_pass2(TestID),
  banner_lines(yellow,4),
  compute_scene_change_pass3(TestID),
  banner_lines(blue,4),
  compute_scene_change_pass4(TestID)))).


compute_scene_change_pass1(TestID):- 
  show_object_dependancy(TestID),!.
  %learn_object_dependancy(TestID).

compute_scene_change_pass2(TestID):- 
  forall(props_change(TestID,IO,P),
    forall(prop_can(TestID,IO,P,PSame),
      assert_accompany_changed_db(TestID,IO,P,PSame))).

%assert_become_new(Term):- \+ clause_asserted(Term),!, pp_ilp(assert_become_new=Term), asserta_new(Term).
assert_become_new(Term):- asserta_new(Term).
%assert_become_new(Term):- pp_ilp(assert_become_new=Term),!, assert_if_new(Term).


solve_via_scene_change(TestID):-  
 %cls, %make,
 must_det_ll((
  ensure_test(TestID),
  clear_scene_rules(TestID),
  %detect_pair_hints(TestID),
  learn_grid_size(TestID),
  ensure_scene_change_rules(TestID),
  print_scene_change_rules(TestID),
  %ExampleNum=_+_,
  forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore(time(solve_via_scene_change_rules(TestID,ExampleNum)))), 
 !)).

solve_via_scene_change_rules(TestID,ExampleNum):-
 must_det_ll((
  kaggle_arc(TestID,ExampleNum,In,Expected),
  banner_lines(green,4),
  obj_group5(TestID,ExampleNum,in,ROptions,TempObjs),TempObjs\==[],
  grid_to_tid(In,TID),
  into_fti(TID,ROptions,In,VM),
  individuate(VM),
  Objs = VM.objs)),
  %wots(SS,solve_obj_group(VM,TestID,ExampleNum,ROptions,Objs,ObjsO)),
  solve_obj_group(VM,TestID,ExampleNum,ROptions,in,Objs,ObjsO),
  dash_chars,

 must_det_ll((
  print_ss(wqs(solve_via_scene_change_rules(ExampleNum)),Objs,ObjsO),
  dash_chars,
  into_solid_grid(ObjsO,OurSolution1),
  once(((notrace((predict_grid_size_now(TestID,In,PH,PV),ground(PH+PV)))
     ->resize_grid(PH,PV,OurSolution1,OurSolution)
      ;notrace(=(OurSolution1,OurSolution));notrace(trim_outside2(OurSolution1,OurSolution))))),
  into_solid_grid(Expected,ExpectedOut),
  count_difs(ExpectedOut,OurSolution,Errors),
  print_ss(wqs(solve_via_scene_change_rules(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution))),
  (Errors == 0 ->  
    banner_lines(green,4) 
    ;(banner_lines(red,10),!,bt,!,banner_lines(red,1),
      %print_scene_change_rules(TestID),banner_lines(red,1), 
      banner_lines(red,10),!,
    fail)).
   


print_scene_change_rules(TestID):-
 ensure_test(TestID),
 must_det_ll((
  banner_lines(cyan,4),
  %show_assumed_mapped(TestID),
  %print_object_dependancy(TestID),
  banner_lines(cyan,3),
   Ele = ac2(IO,P,PSame),
   findall(Ele,is_accompany_changed_computed(TestID,IO,P,PSame),List),
   sort(List,SetR),reverse(SetR,Set),
   forall(member(Ele,Set),
     pp_ilp(is_accompany_changed_db(TestID,IO,P,PSame))),
  banner_lines(cyan,4))).


compute_scene_change_pass3(TestID):-
   findall(IO-P,is_accompany_changed_db(TestID,IO,P,_),Ps),
   variant_list_to_set(Ps,Set),
   maplist(compute_scene_change_pass3a(TestID),Set),
   maplist(compute_scene_change_pass3b(TestID),Set),
   maplist(compute_scene_change_pass3c(TestID),Set).

compute_scene_change_pass4(TestID):-
   compute_scene_change_pass3(TestID).

compute_scene_change_pass3a(TestID,IO-P):- 
   findall(PSame,is_accompany_changed_db(TestID,IO,P,PSame),List),
   List=[_,_|_],
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
    update_accompany_changed_db(TestID,IO,P,SameS).
compute_scene_change_pass3a(_,_).

compute_scene_change_pass3b(TestID,IO-P):-
   findall(PSame,is_accompany_changed_db(TestID,IO,P,PSame),List),
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
   correct_antes1(TestID,IO,P,SameS,Kept), Kept\==[],!, % pp(P=compute_scene_change_pass3([SameS,Kept])),
  update_accompany_changed_db(TestID,IO,P,Kept).
compute_scene_change_pass3b(_,_). 

compute_scene_change_pass3c(TestID,IO-P):-
   is_accompany_changed_db(TestID,IO,P,PSame),
   correct_antes2(TestID,IO,P,PSame,Kept),
   update_accompany_changed_db(TestID,IO,P,Kept).
compute_scene_change_pass3c(_,_).



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
       forall((is_accompany_changed_db(TestID,IO,DP,DSame),at_least_one_overlap(DSame,PSame)),
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

solve_obj_group(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):-
 solve_obj_group(VM,TestID,ExampleNum,in,ROptions,Objs,Objs1),
 Objs1 \==[],
 solve_obj_group(VM,TestID,ExampleNum,in_out,ROptions,Objs1,Objs2),
 solve_obj_group(VM,TestID,ExampleNum,in_out_out,ROptions,Objs2,Objs3),!,
 ObjsO= Objs3.

solve_obj_group(VM,TestID,ExampleNum,IO,ROptions,Objs,OObjs):- IO == in,!,
  my_maplist(solve_obj(VM,TestID,ExampleNum,IO,ROptions),Objs,OObjs).

solve_obj_group(VM,TestID,ExampleNum,IO,ROptions,Objs,ObjsO):-
  %trace,arc_cache:map_group(TestID,ExampleNum,IO,Group),
  GRP = grp(Info,PreObjs,Out),
  findall(GRP,(arc_cache:map_pairs(TestID,_,_IO2,Info,PreObjs,Out), sub_var(IO,Info)),Groups),
  variant_list_to_set(Groups,Set),
  banner_lines(blue,2),
  forall(member(GRP,Set),pp_ilp(GRP)),
  banner_lines(blue,2),
  %trace,prop_can(TestID,IO,P,Preconds),
  %trace,
  solve_obj_set(Set,VM,TestID,ExampleNum,IO,ROptions,Objs,ObjsO),
  flatten(ObjsO,ObjsO).

%solve_obj(_VM,_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IO_Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  solve_obj(VM,TestID,ExampleNum,IO_Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IO_Start,ROptions,Objs,ObjsO).


solve_obj(_VM,_TestID,_ExampleNum,_IO,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
solve_obj(VM,TestID,_ExampleNum,_IO_Start,_ROptions,Obj,OObj):- 
 must_det_ll((
   %Agenda = agenda(IO,P,PSame),
   Agenda = P,
   IO=_,
   findall(Agenda,
   (is_accompany_changed_verified(TestID,IO,P,PSame), 
        flatten(PSame,Rest), 
        forall(member(R,Rest),has_prop(R,Obj))),PsL),
 list_to_set(PsL,Ps), 
 edit_object(VM,Ps,Obj,OObj))).
%solve_obj(VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Obj,OObj):-
%  edit_object(VM,pen([cc(black,1)]),Obj,OObj).
%solve_obj(VM,_TestID,_ExampleNum,_IO_Start,_ROptions,_Obj,[]).
solve_obj(_VM,_TestID,_ExampleNum,_IO_Start,_ROptions,Obj,Obj).


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
  pp(([[removed=Removed],[added=Added]])))).

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(VM,agenda(IO,P,PSame),I,O):- !, pp_ilp(IO:P-PSame), override_object_1(VM,P,I,O).
override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),rebuild_from_globalpoints(VM,Obj,GPoints,NewObj).
override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.

is_accompany_changed_verified(TestID,IO,P,PSame):-
  is_accompany_changed_computed(TestID,IO,P,PSame), PSame\==[].

is_accompany_changed_computed(TestID,IO,P,PSame):-
   is_accompany_changed_db(TestID,IO,P,PSame) *->true ; prop_can(TestID,IO,P,PSame). 
   
prop_can(TestID,IO,P,Can):-    
  props_change(TestID,IO,P),
  once((prop_cant(TestID,IO,P,Cant),
  prop_can1(TestID,IO,P,Can1),
  intersection(Can1,Cant,_,Can,_))).
  %(Can == [] -> (CanL=Can1,fail) ; CanL= Can).

prop_can1(TestID,IO,P,Can):-  
  props_change(TestID,IO,P),
     findall(O,
       ((member(InOut,[in,out]),enum_object_ext(TestID,InOut,O),is_post_cond_obj(IO,O),has_prop(cc(bg,0),O),
         has_prop(P,O))),PostObjs),
  post_cond_obj_to_pre_cond_obj(TestID,IO,P,PostObjs,PreObjs),
  [Pre|PreRest]=PreObjs,
  indv_props_list(Pre,List),
  findall(U,(member(U,List),U\=@=P,ok_notice(U),forall(member(E,PreRest),has_prop(U,E))),Can).

post_to_pre_object(TestID,IO,_P,Post,Pre):- get_obj_pair(TestID,IO,I,O),O=@=Post,!,Pre=I.
post_to_pre_object(TestID,IO,_P,Post,Pre):- 
    %props_change(TestID,IO,P),
     findall(O,
       ((member(InOut,[in,out]),enum_object_ext(TestID,InOut,O),is_pre_cond_obj(IO,O),has_prop(cc(bg,0),O)
         )),PreObjs),
     find_prox_mappings([],Post,post_to_pre_object,PreObjs,[Pre|_RHSRest]),!.


is_pre_cond_obj(IO,O):- has_prop(giz(g(IO)),O),!.
is_pre_cond_obj(in_out,IO):- !,is_pre_cond_obj(in,IO).
is_pre_cond_obj(s(X),O):- nonvar(X), is_pre_cond_obj(out,O).

is_post_cond_obj(IO,O):- \+ is_pre_cond_obj(IO,O).

post_cond_obj_to_pre_cond_obj(TestID,IO,P,PostObjs,PreObjs):-
  findall(Pre,(member(Post,PostObjs),post_to_pre_object(TestID,IO,P,Post,Pre)),PreObjsL),
  list_to_set(PreObjsL,PreObjs).

prop_cant(TestID,IO,P,Set):-
  props_change(TestID,IO,P),
  findall(O,
    ((member(InOut,[in,out]),enum_object_ext(TestID,InOut,O),is_post_cond_obj(IO,O),has_prop(cc(bg,0),O),
      not_has_prop(P,O))),PostObjs),
  post_cond_obj_to_pre_cond_obj(TestID,IO,P,PostObjs,PreObjs),
  findall(Cant,
    ((member(O,PreObjs),indv_props_list(O,List),member(Cant,List),ok_notice(Cant))),Flat),
   list_to_set(Flat,Set).

enum_object_ext(TestID,IO,O):-
  ensure_test(TestID),
  current_example_nums(TestID,ExampleNum),
  once((obj_group_io(TestID,ExampleNum,IO,Objs),Objs\==[])),member(O,Objs).



%accompany_changed_compute_pass2(TestID,IO,P,SameS):- prop_can(TestID,IO,P,SameS).

%xlisting(propcounts+variance_had_count_set+(pen([cc(yellow,1)]);links_count(contains,4))-'$spft$').
/*
   propcounts( TestID,
           ExampleNum, IO,
           variance_had_count_set(2,0),
           P,
           PL,
           [ 1-(2-links_count(contains,4)),
             1-(1-links_count(contains,3))]),
*/
/*
contains_same([],_):- !.
contains_same([E|L],P):- sub_var(E,P),!,contains_same(L,P).

find_peers_with_same(TestID,IO,P,PSame,NewSame):- select(S,PSame,Next),S=@=P,!,find_peers_with_same(TestID,IO,P,Next,NewSame).
find_peers_with_same(TestID,IO,P,PSame,NewSame):- 
   sub_term(Color,P),is_real_color(Color), sub_term(N,P),number(N),
   my_partition(contains_same([Color]),PSame,SameW,SameWO),SameW\==[], SameWO\==[],!,
   find_peers_with_same(TestID,IO,P,SameWO,NewSame).
find_peers_with_same(_,_,PSame,PSame):-!.
   
   

   

merge_xtra_props_ac1([ac1(PO)|AC3],PSame):- !, merge_xtra_props_ac1_3(PO,AC3,PSame), PSame\==[].
merge_xtra_props_ac1_3(PO,[ac1(PO2)|MORE],OUT):-
  intersection(PO,PO2,IPO),
  merge_xtra_props_ac1_3(IPO,MORE,OUT).
merge_xtra_props_ac1_3(PO,[],PO).

merge_xtra_props_ac2([ac2(_,PSame)],PSame):-!.
merge_xtra_props_ac2(AC2,PSame):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum,PO2),AC3,AC4),
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],PSame).
merge_xtra_props_ac2(AC2,PSame):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum2,PO2),AC3,AC4),
 ExampleNum \== ExampleNum2,
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],PSame).

merge_xtra_props_ac2([ac2(ExampleNum,PO1)|AC3],[ac2(ExampleNum,PO1)|PSame]):-
  merge_xtra_props_ac2(AC3,PSame),!.
merge_xtra_props_ac2(PSame,PSame):-!.


changing_props(TestID,X1,X2):- 
 ensure_test(TestID),
 findall(X1-InOut,props_change(TestID,InOut,X1),X1L),
 variant_list_to_set(X1L,X1S),
 member(X1-IO,X1S),
 member(X2-IO,X1S),
% X1@>X2,
 other_val(X1,X2). 

common_props([O|Objs],Props):-
   indv_props_list(O,List),
   findall(P,(member(P,List),\+ dont_notice(P),forall(member(E,Objs),has_prop(P,E))),Props).

*/

current_example_nums(TestID,ExampleNum):- 
  (var(TestID)->get_current_test(TestID);true),
  ignore((ExampleNum=trn+_)), kaggle_arc(TestID,ExampleNum,_,_). 



save_how_io(HowIn,HowOut):- 
  get_current_test(TestID),save_how_io(TestID,HowIn,HowOut).
save_how_io(TestID,HowIn,HowOut):- 
  assert_test_property(TestID,common,indiv_how(in),HowIn),
  assert_test_property(TestID,common,indiv_how(out),HowOut),!.



obj_group_gg(TestID,ExampleNum,InC,OutC):-
   current_example_nums(TestID,ExampleNum),
   no_repeats_var(OutC), % set_example_num(ExampleNum),
   obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut.

objs_other_than_example(TestID,ExampleNum,IO,Others):-
  findall(O,(current_example_nums(TestID,OExampleNum),
    ExampleNum\==OExampleNum,
    obj_group_io(TestID,OExampleNum,IO,Objs),
    member(O,Objs)),Others).

all_io_objs(TestID,IO,Others):-
  findall(O,(current_example_nums(TestID,ExampleNum), 
   obj_group_io(TestID,ExampleNum,IO,Objs), member(O,Objs)),Others).

with_individuated_cache(TF,Goal):- locally(nb_setval(use_individuated_cache,TF),Goal).

obj_group_io(TestID,ExampleNum,IO,Objs):-
 arc_test_property(TestID,common,indiv_how(IO),How),!,
 current_example_nums(TestID,ExampleNum), 
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,IO,How,Objs))).

obj_group_io(TestID,ExampleNum,IO,Objs):- 
 current_example_nums(TestID,ExampleNum),
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,IO,_,Objs))).

obj_group5(TestID,ExampleNum,IO,ROptions,Objs):- var(TestID),
  ensure_test(TestID),!,obj_group5(TestID,ExampleNum,IO,ROptions,Objs).  
obj_group5(TestID,ExampleNum,IO,ROptions,Objs):- var(ROptions),
 arc_test_property(TestID,common,indiv_how(IO),ROptions),!,
 obj_group5(TestID,ExampleNum,IO,ROptions,Objs).
obj_group5(TestID,ExampleNum,IO,ROptions,Objs):-
 kaggle_arc_io(TestID,ExampleNum,IO,Grid),
  set_example_num(ExampleNum),
 other_grid(Grid,Other),
 with_other_grid(Other,
  
  ((fail, arc_cache:individuated_cache(TestID,TID,GOID,ROptions,Objs), Objs\==[],
  once((testid_name_num_io_0(TID,_,Example,Num,IO),
        testid_name_num_io_0(GOID,_,Example,Num,IO))))*-> true ; grid_to_objs(Grid,ROptions,Objs))).


%show_object_dependancy(_TestID):-  !.
% =============================================================
show_object_dependancy(TestID):-  
% =============================================================
 ensure_test(TestID),
 learn_object_dependancy(TestID),
 print_object_dependancy(TestID).

:- dynamic(arc_cache:map_pairs/6).
:- dynamic(arc_cache:map_group/4).

% =============================================================
learn_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 ignore((ExampleNum=trn+_)),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     learn_object_dependancy(TestID,ExampleNum)),!,
 merge_object_dependancy(TestID).

learn_object_dependancy(TestID,ExampleNum):-
 ignore((ExampleNum=trn+_)), kaggle_arc(TestID,ExampleNum,_,_),
 must_det_ll(( obj_group_gg(TestID,ExampleNum,LHSObjs,RHSObjs),
   maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs))).

maybe_learn_object_dependancy(TestID,ExampleNum,_RHSObjs,_LHSObjs):-
  arc_cache:map_group(TestID,ExampleNum,_,_),!.

maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  RHSObjs\==[],LHSObjs\==[],
  learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

learn_object_dependancy(TestID,ExampleNum,_RHSObjs,_LHSObjs):- arc_cache:map_group(TestID,ExampleNum,_,_),!.
learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):- 
 must_det_ll((
  Step=0,Ctx=in_out,IsSwapped=false,
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,[],LHSObjs,RHSObjs,GroupsR),
  reverse(GroupsR,Groups),
  variant_list_to_set(Groups,SetOfGroups),
  maplist(assert_map_groups(TestID,ExampleNum,in),SetOfGroups))),!.

%assert_map_groups(TestID,ExampleNum,IO,LeftRight):- !, nop(assert_map_groups(TestID,ExampleNum,IO,LeftRight)),!.
% The object dependancy is a list of lists of rules
%assert_map_groups(TestID,ExampleNum,IO,Group):- arc_cache:map_group(TestID,ExampleNum,IO,Group),!.
assert_map_groups(TestID,ExampleNum,IO,Group):- 
  assert_if_new(arc_cache:map_group(TestID,ExampleNum,IO,Group)),!.

merge_object_dependancy(_TestID):- !.
merge_object_dependancy(TestID):-
  findall(Group,arc_cache:map_group(TestID,_ExampleNum,_IO,Group),GroupS),
  pp_ilp(Group),
  itrace,some_min_unifier(GroupS,Group),
  trace.

assert_map_pairs(TestID,ExampleNum,IO,Group):-
 %writeq(aSSSSSSSSSSSSSSSSSSSSSSSSSS_Sassert_map_groups(TestID,ExampleNum,IO,Group)),
 must_det_ll((
  asserta_if_new(arc_cache:map_group(TestID,ExampleNum,IO,Group)),
  forall((sub_term(Pair,Group),is_mapping(Pair)),
    once((must_det_ll((get_mapping_info_list(Pair,Info,In,Out),
                 into_list(In,PreObjs), into_list(Out,PostObjs),                
                 asserta_if_new(arc_cache:map_pairs(TestID,ExampleNum,IO,Info,PreObjs,PostObjs))))))))).

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
 dash_chars,
 forall(arc_cache:map_group(TestID,E,IO,Group),
  once(((dash_chars,dash_chars,pp_ilp(Group),dash_chars,dash_chars,nop(pp(map_group(TestID,E,IO))))))),
 dash_chars,
 
%  forall(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),
%    pp_ilp(map_group(TestID,ExampleNum,IO,LeftRight))),
  forall(arc_cache:map_pairs(TestID,E,_IO2,Info,Pre,Post),
      (dash_chars, writeln(map_pairs(TestID,E,IO,Info)),once(print_ss(Info,Pre,Post)),
        nl,writeln(map_pairs(TestID,E,IO,Info)),nl,dash_chars)),
  dash_chars,!.

get_obj_pair(TestID,IO,I,O):- 
 arc_cache:map_pairs(TestID,_ExampleNum,_When,Info,PreObjs,PostObjs),
 once(( sub_var(IO,Info), into_list(PreObjs,PreObjsL), into_list(PostObjs,PostObjsL))),
 member(I,PreObjsL),member(O,PostObjsL).




pp_ilp(Grp):-pp_ilp(1,Grp).

pp_ilp(_,_):- format('~N'),nl,fail.
pp_ilp(D,Grp):- is_mapping(Grp),
 must_det_ll((
  get_mapping_info_list(Grp,Info,In,Out),
  once(into_solid_grid_strings(In,ITerm)),
  once(into_solid_grid_strings(Out,OTerm)),
  prefix_spaces(D,(dash_chars,format('<grp ~w>\n',[Info]))),
    pp_ilp(D+5,ITerm),
    pp_ilp(D+5,OTerm),
  prefix_spaces(D,(write('</grp>\n'),dash_chars)))).

pp_ilp(D,is_accompany_changed_db(_TestID,IO,P,PSame)):- 
 list_to_conjuncts(PSame,Conj),pp_ilp(D,(IO:P):-Conj),writeln('.'),!.
pp_ilp(D,Grid):- is_grid(Grid),prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,List):- is_list(List), \+ is_grid(List),maplist(pp_ilp(D+3),List).
pp_ilp(D,T):- into_solid_grid_strings(T,G),!, prefix_spaces(D,print(G)),!.
pp_ilp(D,T):- prefix_spaces(D,print(T)),!.


prefix_spaces(D,G):- DD is D, wots(Tabs,(print('>'),print_spaces(DD),print('.'))),prepend_each_line(Tabs,G).


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
into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),%Obj\=@=T,
  is_object(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),is_grid(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
into_solid_grid_strings(WithGrids,WithGrids).
%  \+ arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),

need_positional_context(H,V):- (H=<3;V=<3),!.
need_positional_context(H,V):- (H=<12,V=<12),!.
need_positional_context(_H,_V).


into_solid_grid_str([Obj,Obj2],SS):- fail, is_object(Obj),is_object(Obj2),
 into_solid_grid_str(Obj,Grid1),wots(SS1,print(Grid1)),
 into_solid_grid_str(Obj2,Grid2),wots(SS2,print(Grid2)),
 wots(SS,print_side_by_side(SS1,SS2)),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),
 vis2D(Obj,H,V), vis2D(Obj,H,V),has_prop(giz(g(IO)),Obj),
 (need_positional_context(H,V)->global_grid(Obj,GG);=(Obj,GG)),
  into_solid_grid(GG,Grid), =((loc2D(IO,X-Y,Grid)),SS),!.

%into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),into_solid_grid(Obj,Grid), =((loc2D(X-Y,Grid)),SS),!.
into_solid_grid_str(Obj,Grid):- into_solid_grid(Obj,Grid),Obj\==Grid,!. %,wots(GridStr,(nl,print_grid(Grid))).
into_solid_grid_str(Obj,(GridStr)):- into_solid_grid(Obj,Grid),!,wots(GridStr,(nl,print_grid(Grid))).


% =============================================================
clear_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((clear_object_dependancy(TestID,ExampleNum)))).
clear_object_dependancy(TestID,ExampleNum):-  
  forall(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight),
    retract(arc_cache:map_group(TestID,ExampleNum,IO,LeftRight))),
  forall(arc_cache:map_pairs(TestID,ExampleNum,IO,Info,Right,Left),
    retract(arc_cache:map_pairs(TestID,ExampleNum,IO,Info,Right,Left))).






% sort_by_generation(Grps,SortedByGen):-predsort(sort_on(by_generation),Grps,SortedByGen).
sort_by_generation(Grps,Grps).

maybe_remove_bg(RHSObjs,RHSObjs1):- my_partition(is_fg_object,RHSObjs,RHSObjs1,Rest),RHSObjs1\==[],Rest\==[],!.
%maybe_remove_bg(RHSObjs,RHSObjs1):- include(is_fg_object,RHSObjs,RHSObjs1),RHSObjs1\=@=RHSObjs,!.
maybe_remove_bg(RHSObjs,RHSObjs).

into_delete(_Info,Obj,Obj):- is_mapping(Obj),!.
into_delete(_Info,_Obj,[]). % grp(Info,[Obj],[])).

is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- is_functor(grp,Grp).

get_mapping_info_list(grp(Info,In,Out),Info,In,Out).
get_mapping_info_list(GRP,Info,InOut):-
  get_mapping_info_list(GRP,Info,In,Out),
  into_list(In,InL),into_list(Out,OutL),!,
  append_LR(InL,OutL,InOutL),!,
  must_det_ll((InOutL=InOut)).


append_LR(Prev,Mappings,RestLR):- 
  flatten([Prev,Mappings],RestLR),!.

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  maybe_remove_bg(RHSObjs,RHSObjs1), \=@=(RHSObjs,RHSObjs1),!,
  must_det_ll((calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs1,RestLR))).


calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   Info = info(TestID,ExampleNum,Step,IsSwapped,Ctx,leftover),
   RHSObjs==[], !, must_det_ll((maplist(into_delete(Info),LHSObjs,Mappings),append_LR(Prev,Mappings,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], !, must_det_ll((incr_step(Step,IncrStep),
    calc_o_d_recursively(TestID,ExampleNum,IsSwapped,IncrStep,Ctx,Prev,Prev,RHSObjs,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
 must_det_ll((
  select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,NewPrev,LHSRest,RHSRest,RestLR))).
 
%incr_cntx(Ctx,NewCtx):- Ctx == in_out,!, NewCtx=out_out.
incr_cntx(Ctx,NewCtx):- atom(Ctx),!, atom_concat(Ctx,'_out',NewCtx).
incr_cntx(Ctx,s(Ctx)).
incr_step(Ctx,s(Ctx)).
swap_tf(Ctx,s(Ctx)).

select_pair(perfect,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Left,map_right_to_left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  find_prox_mappings(Right,map_right_to_left,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(need_prev,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Prev,Left,map_right_to_left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  find_prox_mappings(Prev,Right,map_right_to_left,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(from_left,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Prev,Left,map_right_to_left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

select_pair(from_right,Prev,LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  find_prox_mappings(Prev,Left,map_right_to_left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.


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
  Info = info(TestID,ExampleNum,Step,IsSwapped,Ctx,Type),

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
  my_append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
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
assert_map_pair_list(_TestID,_ExampleNum,_IO,[]):-!.
assert_map_pair_list(TestID,ExampleNum,IO,[Right,Left]):- is_object(Left), is_object(Right), !, 
   assert_ map _pairs(TestID,ExampleNum,IO,Right,Left),!.
assert_map_pair_list(TestID,ExampleNum,IO,[Left|Right]):- into_lst(Left,L1),[Left]\=@=L1,
  append(L1,Right,LR),!,assert_map_pair_list(TestID,ExampleNum,IO,LR).
assert_map_pair_list(TestID,ExampleNum,IO,[Right,Left,M|More]):- 
  assert_map_pair_list(TestID,ExampleNum,IO,[Right,Left]),!,
  assert_map_pair_list(TestID,ExampleNum,IO,[Right,M|More]).

assert_map_ pairs(TestID,ExampleNum,IO,Right,Left):-
  %print_ss(map_pair(TestID,ExampleNum,IO),Right,Left),
  assert_become_new(arc_cache:map_ pairs(TestID,ExampleNum,IO,Right,Left)),!.

into_lst(ObjsL,[]):- ObjsL==[],!.
into_lst(ObjsL,[ObjsL]):- \+ compound(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_gridoid(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_grid(ObjsL),!.
into_lst(ObjsL,Lst):- is_list(ObjsL),!,maplist(into_lst,ObjsL,LstL),append(LstL,Lst).
into_lst(Grp,Lst):- is_mapping(Grp), get_mapping_info_list(Grp,_,List),!,into_lst(List,Lst).
into_lst(Grp,Lst):- arg(_,Grp,List),is_list(List),!,into_lst(List,Lst).
into_lst(ObjsL,[ObjsL]).


*/
