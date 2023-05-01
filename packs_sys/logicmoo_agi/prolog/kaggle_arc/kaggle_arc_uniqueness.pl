/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- include(kaggle_arc_header).

:- dynamic(is_for_ilp/4).
:- dynamic(is_accompany_changed_db/4).
clear_scene_rules(TestID):- 
  forall(is_accompany_changed_db(TestID,Ctx,P,PSame),
     ignore(retract(is_accompany_changed_db(TestID,Ctx,P,PSame)))),!,
  clear_object_dependancy(TestID).


% Define predicates that shouldn't be noticed
dont_notice(oid(_)).
dont_notice(links_count(sees, _)).
dont_notice(giz(_)).
dont_notice(pg(_, iz(_), rankLS, largest)).
dont_notice(iz(symmetry_type(rollD, _))).
%dont_notice(global2G(_,_)).
%dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(link(contains,_)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(pg(_851136,pen(_851146),rankLS,_)).
dont_notice(iz(i_o(_))).
dont_notice(pg(_,iz(_),rankLS,_)).
dont_notice(iz(stype(_))).
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


dont_deduce(link(sees(_),_)).
%dont_deduce(giz(_)).
%dont_deduce(pg(_,_,_,_)).
dont_deduce(pg(_,iz(_),rankLS,_)).
dont_deduce(pg(_,_,rankLS,_)).
dont_deduce(size2D(_)).
%dont_deduce(global2G(_,_)).
dont_deduce(vis2D(_,_)).
dont_deduce(P):- \+ compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_grid(G),!.
dont_deduce(P):- sub_term(G,P),compound(G),is_object(G),!.
dont_deduce(grid(_)).
dont_deduce(iz(_)).

%dont_deduce(P):- compound(P),compound_name_arguments(P,_,[X]),number(X).
dont_deduce(grid_ops(comp,_)). % rot2D(rot90),changes([]),iz(fg_or_bg(iz_fg)),links_count(contained_by,0),links_count(contains,0),cc(plain_var,0),cc(bg,0),global2G(9,9),iz(sizeGX(1)),unique_colors_count(1),empty_area(0),iz(algo_sid(comp,sid_12)),iz(algo_sid(norm,sid_12)),iz(symmetry_type(flipDHV,false)),iz(symmetry_type(rot180,true)),iz(symmetry_type(flipV,true)),iz(symmetry_type(flipH,true)),iz(symmetry_type(rot270,false)),iz(symmetry_type(rot90,false)),iz(symmetry_type(flipD,false)),iz(symmetry_type(sym_h_xor_v,false)),iz(symmetry_type(sym_hv,true)),iz(filltype(solid)),iz(colormass),iz(media(shaped)),iz(info(birth(colormass))),pg(_1467088,mass(_1467098),rankLS,largest),pg(_1467108,iz(sizeGX(_1467122)),rankLS,smallest),pg(_1467132,iz(sizeGY(_1467146)),rankLS,largest),pg(_1467156,iz(cenGX(_1467170)),rankLS,largest),pg(_1467180,iz(cenGY(_1467194)),rankLS,largest),pg(_1467204,unique_colors_count(_1467214),rankLS,smallest),pg(_1467224,empty_area(_1467234),rankLS,smallest).
dont_deduce(grid_rep(norm,_)). % pen([cc(blue,1)]),pg(_1489874,mass(_1489884),rank1,4).
dont_deduce(iz(stype(_))). % rot2D(rot90),grid_ops(comp,[]),changes([]),iz(fg_or_bg(iz_fg)),links_count(contained_by,0),links_count(contains,0),cc(plain_var,0),cc(bg,0),global2G(9,9),iz(sizeGX(1)),unique_colors_count(1),empty_area(0),iz(algo_sid(comp,sid_12)),iz(algo_sid(norm,sid_12)),iz(symmetry_type(flipDHV,false)),iz(symmetry_type(rot180,true)),iz(symmetry_type(flipV,true)),iz(symmetry_type(flipH,true)),iz(symmetry_type(rot270,false)),iz(symmetry_type(rot90,false)),iz(symmetry_type(flipD,false)),iz(symmetry_type(sym_h_xor_v,false)),iz(symmetry_type(sym_hv,true)),iz(filltype(solid)),iz(colormass),iz(media(shaped)),iz(info(birth(colormass))),pg(_1473178,mass(_1473188),rankLS,largest),pg(_1473198,iz(sizeGX(_1473212)),rankLS,smallest),pg(_1473222,iz(sizeGY(_1473236)),rankLS,largest),pg(_1473246,iz(cenGX(_1473260)),rankLS,largest),pg(_1473270,iz(cenGY(_1473284)),rankLS,largest),pg(_1473294,unique_colors_count(_1473304),rankLS,smallest),pg(_1473314,empty_area(_1473324),rankLS,smallest).
dont_deduce(iz(symmetry_type(_,_))). % rot2D(rot90),grid_ops(comp,[]),changes([]),iz(fg_or_bg(iz_fg)),links_count(contained_by,0),links_count(contains,0),cc(plain_var,0),cc(bg,0),global2G(9,9),iz(sizeGX(1)),unique_colors_count(1),empty_area(0),iz(algo_sid(comp,sid_12)),iz(algo_sid(norm,sid_12)),iz(symmetry_type(flipDHV,false)),iz(symmetry_type(rot180,true)),iz(symmetry_type(flipV,true)),iz(symmetry_type(flipH,true)),iz(symmetry_type(rot270,false)),iz(symmetry_type(rot90,false)),iz(symmetry_type(sym_h_xor_v,false)),iz(symmetry_type(sym_hv,true)),iz(filltype(solid)),iz(colormass),iz(media(shaped)),iz(info(birth(colormass))),pg(_1477530,mass(_1477540),rankLS,largest),pg(_1477550,iz(sizeGX(_1477564)),rankLS,smallest),pg(_1477574,iz(sizeGY(_1477588)),rankLS,largest),pg(_1477598,iz(cenGX(_1477612)),rankLS,largest),pg(_1477622,iz(cenGY(_1477636)),rankLS,largest),pg(_1477646,unique_colors_count(_1477656),rankLS,smallest),pg(_1477666,empty_area(_1477676),rankLS,smallest).
%dont_deduce(mass(2)). % center2G(2,9),vis2D(1,2),loc2D(2,8),grid_ops(norm,[rot90]),link(sees([cc(e,2)]),o_?_459_t_08ed6ac7_trn_1_out),cc(fg,2),iz(sizeGY(2)),iz(cenGY(9)),rotSize2D(grav,2,1),area(2),iz(sid(sid_12)),\+link(sees([cc(w,2)]),o_i_109_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_641_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_337_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_Z_24_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_?_459_t_08ed6ac7_trn_1_out).
dont_deduce(oid(_)). % center2G(2,9),vis2D(1,2),loc2D(2,8),mass(2),grid_ops(norm,[rot90]),link(sees([cc(e,2)]),o_?_459_t_08ed6ac7_trn_1_out),cc(fg,2),iz(sizeGY(2)),iz(cenGY(9)),rotSize2D(grav,2,1),area(2),iz(sid(sid_12)),\+link(sees([cc(w,2)]),o_i_109_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_641_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_337_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_532_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_Z_24_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_?_459_t_08ed6ac7_trn_1_out).
dont_deduce(cc(plain_var,0)).
dont_deduce(links_count(_,_)).
dont_deduce(empty_area(_)).
dont_deduce(unique_colors_count(_)).


% Define predicates that should be deduced
do_deduce(link(sees(_),_)).
do_deduce(rot2D(_)).
do_deduce(pen(_)).
do_deduce(iz(sid(_))).
do_deduce(iz(X)):- !,do_deduce(X),!.
do_deduce(P):- compound(P),compound_name_arguments(P,_,[X,Y]),number(X),number(Y).
do_deduce(rotSize2D(grav,_,_)).

% Predicate to check if P should be deduced
ok_deduce(P):- \+ \+ dont_deduce(P), !, fail.
ok_deduce(P):- \+ \+ do_deduce(P),!.
ok_deduce(_).
%ok_deduce(P):- \+ \+ dont_notice(P),!,fail.



in_out_atoms(in,out).







% Check if two values have the same property names but are not equal

other_val(X1,X2):- negated_s_lit(X1,P1), 
  ( negated_s_lit(X2,P2) -> other_val(P1,P2) ; other_val(X2,P1)).
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



io_to_cntx(in,in_out).
%io_to_cntx(in,in_out_out).
io_to_cntx(out,in_out_out).
io_to_cntx(out,s(_)).
io_to_cntx(X,X).


%assert_become_new(Term):- \+ clause_asserted(Term),!, pp_ilp(assert_become_new=Term), asserta_new(Term).
assert_become_new(Term):- asserta_new(Term).
%assert_become_new(Term):- pp_ilp(assert_become_new=Term),!, assert_if_new(Term).


solve_via_scene_change(TestID):-  
 must_det_ll((
  ensure_test(TestID), %make,
  clear_scene_rules(TestID),
  %detect_pair_hints(TestID),
  time(learn_grid_size(TestID)),
  %%ensure_scene_change_rules(TestID),
  save_test_hints_now(TestID),
  ExampleNum=tst+_,
  forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore(time(solve_via_scene_change_rules(TestID,ExampleNum)))), 
 !)).

solve_via_scene_change_rules(TestID,ExampleNum):-
 must_det_ll((
 %%ensure_scene_change_rules(TestID),
  kaggle_arc(TestID,ExampleNum,In,Expected),
  banner_lines(green,4),
  % predict_grid_size_now(TestID,In,PX,PY),
  obj_group5(TestID,ExampleNum,in,ROptions,TempObjs),TempObjs\==[],
  grid_to_tid(In,TID),
  into_fti(TID,ROptions,In,VM),
  individuate(VM),
  Objs = VM.objs,
  %print_object_dependancy(TestID),
  show_object_dependancy(TestID),
 %% print_scene_change_rules(solve_via_scene_change_rules,TestID),
  print_ss(wqs(expected_answer(ExampleNum)),Objs,Expected),
  dash_chars)),!,

  once(enter_solve_obj(VM,TestID,ExampleNum,ROptions,Objs,ObjsO)),

 must_det_ll((
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


enter_solve_obj(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):-
 must_det_ll((
  solve_obj_group(VM,TestID,ExampleNum,ROptions,in_out,Objs,ObjsM),
  flatten_objects(ObjsM,ObjsMF),
  solve_obj_group(VM,TestID,ExampleNum,ROptions,in_out_out,ObjsMF,ObjsO))),
 ObjsO \==[],!.

score_rule(Ways,Obj,Rule,Score):- is_object(Rule), \+ is_object(Obj),!,score_rule(Ways,Rule,Obj,Score).
score_rule(Ways,Obj,implies(obj_atoms(PCond),edit(P)),Score):- 
 indv_props_list(Obj,Props), \+ member(P,Props),
 %\+ \+ ((member(E,Props),member(E,PCond))),
 once( ( \+ is_bg_object(Obj) ); sub_var(black,PCond)),
 score_rule(Ways,Obj,PCond,P,Score).

has_all_props(CanL,Obj):- maplist(inv_has_prop(Obj),CanL).
inv_has_prop(Obj,Prop):- has_prop(Prop,Obj).

score_rule(exact,Obj,PCond,_P,Score):-  has_all_props(PCond,Obj),!,Score=1000.
score_rule(_Ways,Obj,PCond,_P,Score):-
   obj_atoms(Obj,A),
   obj_atoms(PCond,B),
     intersection(A,B,Good,_Extra,_Bad),
     length(Good,Score).
  
match_ok(_,B):- plain_var(B),!.
match_ok(A,B):- \+ \+ A = B.

two_way_mapping(Ways,Obj,_Objs,Rules,Rule,Rules):-
  match_ok(Ways,exact),!,
  Res = res(Score,Rule),
  findall(Res,(member(Rule,Rules),score_rule(Ways,Obj,Rule,Score)),Results),
  sort(Results,ResultsSorted),
  last(ResultsSorted,Res),
  select(Rule,Rules,_RulesRest),!.
  %select(Obj,Objs,ObjsRest).

two_way_mapping(Ways,Obj,Objs,Rules,Rule,RulesRest):-
  \+ match_ok(Ways,exact),
   once((sort_by_jaccard(Obj,Rules,[Rule|RulesRest]),
   sort_by_jaccard(Rule,Objs,[PickedObj|_ObjsRest]))), 
    ((PickedObj == Obj)-> nop(match_ok(Ways,two_ways)) ; match_ok(Ways,one_way)),
  write_atoms_info(Ways,PickedObj),
  write_atoms_info(paired2,Rule),
  %maplist(write_atoms_info(leftover1),RulesRest),
  %maplist(write_atoms_info(leftover2),ObjsRest),
  !.
      
write_atoms_info(N,E):- obj_atoms(E,Atoms),!,%sort(Atoms,AE),
  nl,writeln(N=Atoms).

apply_rules_to_objects(_,_,_,[],[]):-!.
apply_rules_to_objects(_,_,[],_,[]):-!.


apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):- 
   match_ok(Mapping,one_to_one),
   \+ match_ok(Ways,exact),

   two_way_mapping(two_way,Obj,Objs,Rules,Rule,RulesRest),

   select(Obj,Objs,ObjsRest),
   apply_rules_to_objects(Ways,Mapping,RulesRest,ObjsRest,More).

apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):-
   match_ok(Mapping,each_object_once),
   select(Obj,Objs,ObjsRest),
  two_way_mapping(Ways,Obj,Objs,Rules,Rule,_),
   apply_rules_to_objects(Ways,Mapping,Rules,ObjsRest,More).

apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):-
   match_ok(Mapping,each_rule_once),
   select(Rule,Rules,RulesRest),
   two_way_mapping(Ways,Rule,Rules,Objs,Obj,ObjRest),!,
   apply_rules_to_objects(Ways,Mapping,RulesRest,ObjRest,More).

apply_rules_to_objects(Ways,Mapping,[_|Rules],Objs,More):- 
 match_ok(Mapping,each_rule_once),!,
 apply_rules_to_objects(Ways,Mapping,Rules,Objs,More).

apply_rules_to_objects(Ways,Mapping,Rules,[_|Objs],More):-
 match_ok(Mapping,each_object_once),!,
 apply_rules_to_objects(Ways,Mapping,Rules,Objs,More).

apply_rules_to_objects(_Ways,_Mapping,_Rules,_Objs,[]).


solve_obj_group(VM,TestID,_ExampleNum,_ROptions,Ctx,Objs,ObjsO):-
  Rule = implies(obj_atoms(PCond),edit(P)),
  io_to_cntx(IN_OUT,Ctx), 
  findall_vset(Rule,(is_accompany_changed_db(TestID,IN_OUT,P,PSame), list_to_set(PSame,PCond)), Rules),
  member(Ways-Strategy,[exact-_,two_way-one_to_one,_-_]), 
  apply_rules_to_objects(Ways,Strategy,Rules,Objs,Todo),
  pp_ilp((see_Strategy(Ways-Strategy)=Todo)), Todo\==[],
  once((maplist(run_todo_output(VM),Todo,ObjsM),flatten_objects(ObjsM,ObjsO))), ObjsO\==[],!.
/*
solve_obj_group(_VM,TestID,_ExampleNum,Ctx,_ROptions,Objs,ObjsO):-
 must_det_ll((
  %PreObjsL,PostObjsL,_USame,_UPA2,_UPB2
  %map_pairs_info_io(TestID,_ExampleNum,Ctx,_Step,_TypeO,PreObjsL,PostObjsL,_USame,_UPA2,_UPB2),   
  CLS = prop_to_can(TestID,Ctx,SomeP,O,Can1,Cant,Preconds),
  findall(CLS,prop_to_can(TestID,Ctx,SomeP,O,Can1,Cant,Preconds),FwdRules),% prop_can(TestID,Ctx,SomeP,Preconds)
  maplist(apply_to_objs(Ctx,Objs),FwdRules,ObjsOFL),append(ObjsOFL,ObjsOF),
  flatten([ObjsOF],ObjsO),
  print_ss(Ctx,Objs,ObjsO))), ObjsO \==[],!.*/
solve_obj_group(_VM,_TestID,_ExampleNum,_Ctx,_ROptions,Objs,Objs).
/*
apply_to_objs(Ctx,Objs,CLS1,ObjsO):-
 must_det_ll((
  CLS1 = prop_to_can(_TestID,Ctx,SomeP,O,_Can1,Cant,Preconds),
  CLS2 = cl(Preconds,O,SomeP),
  include(can_cant_props(Preconds,Cant),Objs,SelectedObjects),
  %maybe_apply
  findall(NewObjs,
     (%member(CLS1,FwdRules),  
      maybe_apply(CLS2,SelectedObjects,NewObjs)), NewObjL),
  flatten([Objs,NewObjL],NewObjs),
  variant_list_to_set(NewObjs,ObjsO))),!.
*/
/*
solve_obj_group(_VM,TestID,_ExampleNum,Ctx,_ROptions,Objs,ObjsO):-
  CLS = cl(Preconds,O,Cant),
  findall(CLS,% prop_can(TestID,Ctx,SomeP,Preconds)
    (prop_to_can(TestID,IN_OUT,P,_O,_Can1,Cant,Can),
     include(can_cant_props(Can,Cant),Objs,SelctedObjects),

   prop_to_can(TestID,Ctx,SomeP,O,_Can1,Cant,Preconds),FwdRules),
  findall(NewObjs,
     (member(CLS,FwdRules),  maybe_apply(CLS,Objs,NewObjs)),
     NewObjL),
  flatten([Objs,NewObjL],NewObjs),
  variant_list_to_set(NewObjs,ObjsO),!.
maybe_apply(CLS,Objs,NewObj):-   
  CLS = cl(Preconds,O,_SomeP),
  maplist(has_all_props(Preconds),Objs),
  NewObj=O,!.
maybe_apply(CLS,Objs,NewObj):-   
  CLS = cl(Preconds,O,_SomeP),
  select(Obj,Objs,Rest),member(Obj2,Rest),
  flat_props([Obj,Obj2],Props),
  intersection(Props,Preconds,Matched,Missing,_Extra),
  pp_ilp(matched=Matched),
  Missing==[],
  NewObj=O,!.

%copy_obj(Rules,Objs,_TestID,_ExampleNum,_IN_OUT,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
copy_obj(Rules,Objs,VM,_TestID,_EN,_Ctx,_ROptions,Obj,OObj):- 
 must_det_ll(( %print_grid(copy_obj,Obj),
    sort_by_jaccard(Obj,Rules,[Rule|_]),
    pp_ilp(Rule),
    print_grid(Obj),
    sort_by_jaccard(Rule,Objs,[_PickedObj|_]),
    edit_object(VM,Rule,Obj,OObj))).
    %print_grid(copy_obj(Rules,Objs,Ps),Obj,OObj))),!.
%copy_obj(Rules,Objs,_VM,_TestID,_ExampleNum,_Ctx,_ROptions,Obj,Obj).
   %map_pairs_info_io(TestID,_EN,Ctx,_Step,_TypeO,_A,_B,USame,UPA2,UPB2)
   

%              findall(grp(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IN_OUT2,Info,Pre,Post),List),
%              variant_list_to_set(List,Set)
 
copy_obj(_VM,_TestID,_ExampleNum,_IN_OUT,_ROptions,Obj,Obj).
%solve_obj(VM,_TestID,_ExampleNum,IN_OUT,_ROptions,Obj,OObj):-
%  edit_object(VM,pen([cc(black,1)]),Obj,OObj).
%solve_obj(VM,_TestID,_ExampleNum,IN_OUT,_ROptions,_Obj,[]).
*/

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
  pp_ilp(([[removed=Removed],[added=Added]])))).

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),  
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),rebuild_from_globalpoints(VM,Obj,GPoints,NewObj).
override_object_1(VM,Term,I,O):- sub_term(Sub,Term),
   compound(Sub),Sub=edit(P), !, pp_ilp(Term), override_object_1(VM,P,I,O).
override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.

run_todo_output(VM,apply(Rule,Obj),NewObj):-
  edit_object(VM,Rule,Obj,NewObj),!.


mapping_step(    in_out).
mapping_step( in_in_out).
mapping_step(in_out_out).
mapping_step(   out_out).


p_of_post(P,Post):- indv_props_list(Post,Props),member(P,Props).



from_same_pair(Post,Pre):-
  has_prop(giz(example_num(trn+N)),Post),
  has_prop(giz(example_num(trn+N)),Pre).
     
     
obj_in_or_out(Pair,IN_OUT):- is_mapping(Pair),!,
    get_mapping_info(Pair,Info,_In,_Out),arg(3,Info,IN_OUT).
obj_in_or_out(Obj,IN_OUT):- must_det_ll(is_object(Obj)),has_prop(giz(g(I_O)),Obj),!,I_O=IN_OUT.
obj_in_or_out(Obj,IN_OUT):- has_prop(iz(i_o(I_O)),Obj),!,I_O=IN_OUT.
%obj_in_or_out(Obj,I_O):- is_input_object(Obj)-> IN_OUT =out ; IN_OUT =in.

is_pre_cond_obj(Obj,in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,in);obj_in_or_out(Obj,out).
is_pre_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,s(X)):- nonvar(X), is_pre_cond_obj(Obj,out).
is_pre_cond_obj(Obj,IN_OUT):- obj_in_or_out(Obj,IN_OUT).
is_pre_cond_obj(Obj,in):- is_pre_cond_obj(Obj,in_out).


is_post_cond_obj(Obj,in_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,s(X)):- nonvar(X), is_post_cond_obj(Obj,out).
is_post_cond_obj(Obj,out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in):- is_post_cond_obj(Obj,in_out).








common_props([O|Objs],Props):-
   indv_props_list(O,List),
   findall(P,(member(P,List),\+ dont_notice(P),forall(member(E,Objs),has_prop(P,E))),Props).

current_example_nums(TestID,ExampleNum):- 
  (var(TestID)->get_current_test(TestID);true),
  ignore((ExampleNum=trn+_)), kaggle_arc(TestID,ExampleNum,_,_). 



save_how_io(HowIn,HowOut):- 
  get_current_test(TestID),save_how_io(TestID,HowIn,HowOut).
save_how_io(TestID,HowIn,HowOut):- 
  assert_test_property(TestID,common,indiv_how(in),HowIn),
  assert_test_property(TestID,common,indiv_how(out),HowOut),!.

obj_group_gg(TestID,ExampleNum,InC,OutC):- obj_group_pair(TestID,ExampleNum,InC,OutC).

obj_group_pair(TestID,ExampleNum,InC,OutC):-
   current_example_nums(TestID,ExampleNum),
   no_repeats_var(OutC), % set_example_num(ExampleNum),
   obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut.

/*
objs_other_than_example(TestID,ExampleNum,InOut,Others):-
  findall(O,(current_example_nums(TestID,OExampleNum),
    ExampleNum\==OExampleNum,
    obj_group_io(TestID,OExampleNum,InOut,Objs),
    member(O,Objs)),Others).
*/
all_io_objs(TestID,InOut,Others):-
  findall(O,(current_example_nums(TestID,ExampleNum), 
   obj_group_io(TestID,ExampleNum,InOut,Objs), member(O,Objs)),Others).

with_individuated_cache(TF,Goal):- locally(nb_setval(use_individuated_cache,TF),Goal).

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
  arc_cache:individuated_cache(TestID,TID,_GOID,ROptions,Objs), 
  once((sub_var(ExampleNum,TID),sub_var(InOut,TID))),!.

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


% =============================================================
learn_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
  must_det_ll((
  ensure_individuals(TestID),
 ignore((ExampleNum=trn+_)),
	retractall(arc_cache:causes(TestID,_,_,_,_)),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
	learn_object_dependancy(TestID,ExampleNum)))).
learn_object_dependancy(TestID,ExampleNum):-
 current_example_nums( TestID,ExampleNum),
 must_det_ll(( obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
   maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs))).

maybe_learn_object_dependancy(TestID,ExampleNum,_RHSObjs,_LHSObjs):-
  arc_cache:prop_dep(TestID,ExampleNum,_,_,_,_,_,_,_),!.

maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  RHSObjs\==[],LHSObjs\==[],
  learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):- 
 must_det_ll((
  Step=0,Ctx=in_out,IsSwapped=false,
  
  sort_by_jaccard(one(RHSObjs),LHSObjs,LHSObjsOrdered),
  sort_by_jaccard(one(LHSObjs),RHSObjs,RHSObjsOrdered),
  %prinnt_sbs_call(LHSObjsOrdered,RHSObjsOrdered),  
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,[],LHSObjsOrdered,RHSObjsOrdered,Groups),
  assert_map_pairs(TestID,ExampleNum,Ctx,Groups))).

prinnt_sbs_call([],[]):- dash_chars,!.
prinnt_sbs_call([G1|WP1],[G2|WP2]):- !,
  length(WP1,L1),length(WP2,L2),
   print_ss(blue,G1,L1,G2,L2),
   prinnt_sbs_call(WP1,WP2),!.
prinnt_sbs_call(R,[]):- !, dash_chars,!, wdmsg(input), print_side_by_side_l(1,R),!,dash_chars,dash_chars.
prinnt_sbs_call([],R):- !, dash_chars,!, wdmsg(output), print_side_by_side_l(1,R),!,dash_chars,dash_chars.
  

prinnt_sbs_call(WP1,WP2):- 
 must_det_ll((
    wots(S1,maplist(print_grid_nl,WP1)),
    wots(S2,maplist(print_grid_nl,WP2)),
    atomic_list_concat(SS10,'\n',S1),
    atomic_list_concat(SS20,'\n',S2),
    max_width(SS10,SS1,100), 
    max_width(SS20,SS2,100),
    %SS10=SS1,SS20=SS2,
    make_longest_len(SS1,SS2,SSS1,SSS2),
    print_to_string11(write,0,SSS1,SSS1A,Lad1),Lad is Lad1,
    maplist(print_to_string_using_up(Lad,''),SSS1A,SSS1B), 
    print_side_by_side0(SSS1B,_,SSS2))).

print_grid_nl(G):- nl,print_grid(G),nl.

assert_map_pairs(TestID,ExampleNum,Ctx,Group):- is_list(Group),!,maplist(assert_map_pairs(TestID,ExampleNum,Ctx),Group).
assert_map_pairs(TestID,ExampleNum,Ctx,grp(Info,In,Out)):-
  into_list(In,InL),into_list(Out,OutL),
  once((diff_l_r(InL,OutL,Same,InPFlat,OutPFlat),
   unnumbervars(('$VAR'(0),'$VAR'('_'),Same,InPFlat,OutPFlat),UNV))),
   UNV = (_FG1,_BG1,USame,InFlatProps,OutFlatProps),
  %pp_ilp(grp(Info,InL,OutL)),!,
  assertz_new(arc_cache:map_pairs(TestID,ExampleNum,Ctx,Info,InL,OutL)),
  maplist(obj_to_oid,InL,InLOID),
  maplist(obj_to_oid,OutL,OutLOID),
  assertz_new(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,InLOID,OutLOID,USame,InFlatProps,OutFlatProps)),!.
assert_map_pairs(_TestID,_ExampleNum,_Ctx,call(Rule)):-!,must_det_ll(Rule),!.

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
  /*if_t(( \+ arc_cache:map_pairs(TestID,_,_,_,_,_)),
   ( dash_chars,forall(arc_cache:map_group(TestID,_,_IN_OUT,Group),
    once(((dash_chars,dash_chars,pp_ilp(Group),dash_chars,dash_chars)))))),
  dash_chars,*/
% findall_vset(grp(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IN_OUT2,Info,Pre,Post),Set1),
% maplist(pp_ilp,Set1),
 dash_chars,dash_chars,
 %pp_ilp_vset(grp(Info,Pre,Post),pair_obj_info(TestID,_,_,Info,Pre,Post)),

 More = call(show_cp_diff(USame,InPFlat,OutPFlat)),
 pp_ilp_vset(grp(Info,LHS,RHS)+More,
   (pair_obj_props321(TestID,_ExampleNum,_Ctx,Info,_Step,_TypeO,LHS,RHS,USame,InPFlat,OutPFlat))),
 %if_t(Set1 =@= Set2,  wdmsg('Set 2 the same')),
 %if_t(Set1 \=@= Set2,
 dash_chars,dash_chars.

pp_ilp_vset(T,G):- findall(T,G,L),list_to_set(L,S),maplist(pp_ilp,S).
:- dynamic(arc_cache:map_pairs/6).
:- dynamic(arc_cache:prop_dep/9).
:- dynamic(arc_cache:causes/5).

pair_obj_props(TestID,ExampleNum,Ctx,Info,Step,TypeO,LHS,RHS,USame,InFlatProps,OutFlatProps):-
  Info = info(Step,_IsSwapped,Ctx,TypeO,TestID,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,USame,InFlatProps,OutFlatProps).

pair_obj_info(TestID,ExampleNum,Ctx,Info,LHS,RHS):-
  Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,_USame,_InFlatProps,_OutFlatProps).

ok_intersect(L1,L2):-
  member(E1,L1),member(E2,L2),
  other_val(E1,E2),!,fail.
ok_intersect(_,_).

pair_obj_props321(TestID,Ex,Ctx,Info,Step,Type,LHSO,RHSO,S,L,R):- 
 (pair_obj_props4(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props3(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props2(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
    pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)))),
  into_solid_objs(LHS,LHSO),into_solid_objs(RHS,RHSO).

into_solid_objs(RHS,RHSO):- flatten([RHS],RHSM), maplist(into_solid_grid_strings,RHSM,RHSO).


pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,[z_c1(1,2,3,4,5)|S],L,R):-
  Info = info(Step,_IsSwapped,Ctx,Type,TestID,Ex),
  pair_obj_props(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R).

pair_obj_props3(TestID,Ex,Ctx,Info,Step2,Type,LHS,RHS,[z_z_z_c3(1,2,3,4,5)|S],L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,Ex2),
  dif(Ex1,Ex2),dif(Ex,Ex2),dif(Ex1,Ex),
  pair_obj_props1(TestID,Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props2(TestID,Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  ignore(Step1=Step2), ok_intersect(R1,R2), something_common(R1,R2),
  once((maplist(merge_vals,[Type1,LHS1,RHS1],[Type2,LHS2,RHS2],[Type,LHS,RHS]))),
  the_min_unifier(R1,R2,R),the_min_unifier(S1,S2,S),append_sets(L1,L2,L).

pair_obj_props4(TestID,Ex,Ctx,Info,Step2,Type,LHS,RHS,[z_z_z_z_c4(1,2,3,4,5)|S],L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,Ex2),
  dif(Ex1,Ex2),dif(Ex,Ex2),dif(Ex1,Ex),
  pair_obj_props1(TestID,Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props3(TestID,Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  ignore(Step1=Step2), ok_intersect(R1,R2), something_common(R1,R2),
  once((maplist(merge_vals,[Type1,LHS1,RHS1],[Type2,LHS2,RHS2],[Type,LHS,RHS]))),
  the_min_unifier(R1,R2,R),the_min_unifier(S1,S2,S),append_sets(L1,L2,L).

pair_obj_props2(TestID,Ex,Ctx,Info,Step1,Type,LHS,RHS,[z_z_c2(1,2,3,4,5)|S],L,R):-
  Info = info(Step2,_IsSwapped,Ctx,Type,TestID,Ex2),
  dif(Ex1,Ex2),dif(Ex,Ex2),dif(Ex1,Ex),
  pair_obj_props1(TestID,Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props1(TestID,Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  ignore(Step1=Step2), ok_intersect(R1,R2), something_common(R1,R2),
  once((maplist(merge_vals,[Type1,LHS1,RHS1],[Type2,LHS2,RHS2],[Type,LHS,RHS]))),
  the_min_unifier(R1,R2,R),the_min_unifier(S1,S2,S),append_sets(L1,L2,L).

merge_vals(A,B,C):-flatten_set([A,B],C),!. 

something_common(R1,R2):- good_for_rhs(E1), member(E1,R1), member(E1,R2),!.

good_for_rhs(pen(_)).
good_for_rhs(loc2D(_,_)).
good_for_rhs(rot2D(_)).
good_for_rhs(iz(sid(_))).

the_min_unifier(S1,S2,[E|S]):- select(E1,S1,S1R),make_unifiable(E1,E2),select(E2,S2,S2R),
   min_unifier(E1,E2,E),!,
   the_min_unifier(S1R,S2R,S).
the_min_unifier(S1,S2,S):- append(S1,S2,S).
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
  show_cp_diff(Same,InPFlat,OutPFlat))).

show_cp_diff(Same,InPFlat,OutPFlat):-
  %flat_props([B],PB), intersection(Same,PB,S,SS,_), append(S,SS,SSame),
  %maplist(print_diffs(1),Same),
 must_det_ll((
  (length(InPFlat,LenA), pp_ilp(removed(LenA)=InPFlat),
   length(Same,SL),      pp_ilp(sames(SL)=Same),
   length(OutPFlat,LenB),pp_ilp(added(LenB)=OutPFlat)),
  !)).


pp_ilp(Grp):-pp_ilp(1,Grp),!.

pp_ilp(_,_):- format('~N'),nl,fail.
pp_ilp(D,T):-  is_ftVar(T),!,prefix_spaces(D,print(T)),!.
pp_ilp(D,call(T)):- !, prefix_spaces(D,call(T)).
% pp_ilp(D,Grp):- is_mapping(Grp), prefix_spaces(D,print(Grp)),!.
pp_ilp(D,Grp):- is_mapping(Grp), !,
 must_det_ll((
  get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<grp  ~w >\n',[Info]))),
    print_io_terms(D+7,In,Out),
    prefix_spaces(D+8,show_cp_diff(In,Out)),
  prefix_spaces(D,(write('</grp>\n'),dash_chars)))).

pp_ilp(D,Grp):- compound(Grp), 
  (In-Out = Grp), Info=lr,!,
 must_det_ll((
  get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<grp  ~w >\n',[Info]))),
    print_io_terms(D+7,In,Out),
    prefix_spaces(D+8,show_cp_diff(In,Out)),
  prefix_spaces(D,(write('</grp>\n'),dash_chars)))).


pp_ilp(D,X=Y):- !, 
  must_det_ll((
   prefix_spaces(D, (print(X),write(' = '))),nl,
   prefix_spaces(D,pp_ilp(1,Y)))).

pp_ilp(D,apply(implies(C,P),Obj)):- !, pp_ilp(D,grp(implies(C,P),[],Obj)).
  

pp_ilp(D,Grid):- is_obj_props(Grid),!,sort(Grid,R),reverse(R,S),prefix_spaces(D,pp(S)).

pp_ilp(D,Grid):- is_group(Grid),!,
  must_det_ll((length(Grid,Len),
   prefix_spaces(D,(format('<group ~w>\n',[len=Len]))),
   prefix_spaces(D,mapgroup(pp_ilp(D+7),Grid)),!,nl,
   prefix_spaces(D,(format('</group>\n',[]))))),!.

pp_ilp(D,A+B):-  !, prefix_spaces(D,(pp_ilp(A),nl,pp_ilp(B))).
pp_ilp(D,Grid):- is_grid(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_object(Grid),!,prefix_spaces(D,print_grid([Grid])),!,nl.

pp_ilp(D,is_accompany_changed_db(_TestID,IO,P,PSame)):- !,
 must_det_ll((
  once(io_to_cntx(IO,CTX);IO=CTX),
  once(list_to_conjuncts(PSame,Conj);PSame=Conj),
  prefix_spaces(D,(print((CTX:P)),prefix_spaces(D+10,(portray_clause((:-Conj)))))))),!.


%pp_ilp(D,(H:-Conj)):- prefix_spaces(D,pp_ilp(H:-Conj)),!.
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

print_io_terms(D,ITerm,OTerm):-  
    is_grid_or_group(ITerm),is_grid_or_group(OTerm),
    prefix_spaces(D,print_ss("",ITerm,OTerm)),!.

print_io_terms(D,loc2D(X,Y,IITerm),loc2D(OX,OY,OOTerm)):- 
    \+ is_mapping(IITerm), \+ is_mapping(OOTerm),
    into_obj(IITerm,ITerm),  into_obj(OOTerm,OTerm),
    prefix_spaces(D,print_ss("",ITerm,loc2D(X,Y),OTerm,loc2D(OX,OY))),!.

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
into_solid_grid_strings(A,Y):-atom(A),into_obj(A,X),!,into_solid_grid_strings(X,Y).
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
%  \+ arc_cache:map_group(TestID,ExampleNum,IN_OUT,LeftRight),

need_positional_context(H,V):- (H=<3;V=<3),!.
need_positional_context(H,V):- (H=<12,V=<12),!.
need_positional_context(_H,_V).


into_solid_grid_str([Obj,Obj2],SS):- fail, is_object(Obj),is_object(Obj2),
 into_solid_grid_str(Obj,Grid1),
 into_solid_grid_str(Obj2,Grid2),
 wots(SS,print_ss(Grid1,Grid2)),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),
 vis2D(Obj,H,V), vis2D(Obj,H,V),has_prop(giz(g(IN_OUT)),Obj),
 (need_positional_context(H,V)->global_grid(Obj,GG);=(Obj,GG)),
  as_grid_string(GG,Grid), =((loc2D(IN_OUT,X-Y,Grid)),SS),!.

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
 forall(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,Right,Left,A,B,C),
    retract(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,Right,Left,A,B,C))),!.





  
% sort_by_generation(Grps,SortedByGen):-predsort(sort_on(by_generation),Grps,SortedByGen).
sort_by_generation(Grps,Grps).

maybe_remove_bg(RHSObjs,RHSObjs1):- my_partition(is_fg_object,RHSObjs,RHSObjs1,Rest),RHSObjs1\==[],Rest\==[],!.
%maybe_remove_bg(RHSObjs,RHSObjs1):- include(is_fg_object,RHSObjs,RHSObjs1),RHSObjs1\=@=RHSObjs,!.
maybe_remove_bg(RHSObjs,RHSObjs).

fg_to_bgc(FG,black):- is_fg_color(FG),!.
fg_to_bgc(FG,FG):- \+ compound(FG),!.


into_delete(_TestID,_ExampleNum,_IsSwapped,_Step,_Ctx,_Prev,_Info,Obj,Obj):- is_mapping(Obj),!.
into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,_Info,Obj,Pairs):- map_pred(fg_to_bgc, Obj,NewObj),
  make_pairs(TestID,ExampleNum,Ctx,IsSwapped,Step,in_out,Prev,Obj,NewObj,Pairs),
  !. %edit_object(pen([cc(black,1)]))  % grp(Info,[Obj],[])).

is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- is_functor(grp,Grp).

get_mapping_info(grp(Info,In,Out),Info,In,Out).
get_mapping_info_list(GRP,Info,InOut):-
  get_mapping_info(GRP,Info,In,Out),
  into_list(In,InL),into_list(Out,OutL),!,
  append_LR(OutL,InL,InOutL),!,
  must_det_ll((InOutL=InOut)).

best_match_rl(RHS,Left,grp(lr,Left,Right)):-
  sort_by_jaccard(Left,RHS,[Right|_RRest]).
best_match_lr(LHS,Right,grp(lr,Left,Right)):-
  sort_by_jaccard(Right,LHS,[Left|_RRest]).
% Generate all pairs of objects from two sets
% pairs(+Set1, +Set2, -Pairs)

pairs_agree(LHS,RHS,PairsR):-
   maplist(best_match_lr(RHS),LHS,PairsR),
   maplist(best_match_rl(LHS),RHS,PairsL),
   PairsR=PairsL.

pairs_agree_or_select(LHS,RHS,PairsR) :- 
  pairs_agree(LHS,RHS,PairsR)*->true;pairs_of_any(LHS,RHS,PairsR).

n_or_more(3,[_,_,_|_]).
n_or_more(2,[_,_|_]).
n_or_more(1,[_|_]).

pairs_of_any([LG1,LG2],RHS,SoFar,PairsR) :- 
    n_or_more(3,RHS),
    append(RG1,RG2,RHS), n_or_more(1,RG1),n_or_more(1,RG2),
    pairs_agree([[LG1],[LG2]],[RG1,RG2],[grp(lr,L1,R1),grp(lr,L2,R2)]),
    pairs_of_any(L1,R1,LR1), pairs_of_any(L2,R2,LR2),
    append_LR([SoFar,LR1,LR2],PairsR).

pairs_of_any(LHS,RHS,SoFar,PairsR) :- n_or_more(2,LHS),n_or_more(2,RHS),
    append(LG1,LG2,LHS), n_or_more(1,LG1),n_or_more(1,LG2),
    append(RG1,RG2,RHS), n_or_more(1,RG1),n_or_more(1,RG2),
    pairs_agree([LG1,LG2],[RG1,RG2],[grp(lr,L1,R1),grp(lr,L2,R2)]),
    pairs_of_any(L1,R1,LR1), pairs_of_any(L2,R2,LR2),
    append_LR([SoFar,LR1,LR2],PairsR).

pairs_of_any(LHS,RHS,SoFar,PairLR):-
  pairs_agree_l_r(LHS,RHS,Agreed,RemainingL,RemainingR),
  append_LR(Agreed,SoFar,AgreedSoFar),
  pairs_of_any(RemainingL,RemainingR,AgreedSoFar,PairLR).

pairs_of_any([],RHS,SoFar,PairLR):- 
  pairs_of_any(SoFar,RHS,SoFar,PairLR).

pairs_of_any(LHSObjs,[],SoFar,PairLR):- 
  flatten(LHSObjs,LHSObjsF),
  my_partition(is_mapping,LHSObjsF,Mappings,Objects),
  into_list([SoFar|Mappings],Kept),
  intersection(Kept,Objects,_,_,DeletedObjs),
  append_LR([Kept,grp(delete,DeletedObjs,[])],PairLR).

pair_combinations([], _, []).
pair_combinations([H1|T1], List2, Pairs) :-
    pair_combinations(T1, List2, RemainingPairs),
    findall([H1, H2], (member(H2, List2), \+ member([H2, H1], RemainingPairs)), CurrentPairs),
    append(CurrentPairs, RemainingPairs, Pairs).


pairs_lr(LHS,RHS,PairsLR):- maplist(best_match_rl(RHS),LHS,PairsLR).




/*
In Prolog: I have two groups of objects where each object is denoted by `obj([center2D(2,6),mass(8),occurs_in_links(contains,1),pen([cc(blue,1)]),shape([square])])`
Each object looks at the other group of objects and keeps what its most simular to.. whenever an object from each group picks each other it forms a new pair .. remove those objects and keep going
there is no more objecs on one side any previous matches get added back and a new set of pairs .. this goes on until there is no opbjects remaining on either side.
sometimes if there are an exact dividable number of objects on one side from the other try permutations of groups from the larger side
*/

% Predicate to find the most similar object in the other group for each object
most_similar(_, [], -1).
most_similar(Obj, [Other|Rest], MostSimilar) :-
    similarity(Obj, Other, Sim),
    most_similar(Obj, Rest, MostSimilarRest),
  (Sim > MostSimilarRest -> MostSimilar = Sim ; MostSimilar = MostSimilarRest).



%?- pair_combinations([o1, o2, o3], [o4, o5, o6], Pairs).
%Pairs = [[o1, o4], [o1, o5], [o1, o6], [o2, o4], [o2, o5], [o2, o6], [o3, o4], [o3, o5], [o3, o6]].


pairs_agree_l_r(LHS,RHS,Agreed,RemainingL,RemainingR):-
   maplist(best_match_rl(RHS),LHS,PairsR),
   maplist(best_match_lr(LHS),RHS,PairsL),
   once((
   intersection(PairsL,PairsR,Agreed,PairsLOnly,PairsROnly),
   Agreed \==[],
   %maplist(length,[Agreed,PairsLOnly,PairsROnly],[NAgreed,NPairsLOnly,NPairsROnly]),
   %NAgreed>NPairsLOnly,%NPairsLOnly>RPairsLOnly,
   %NAgreed>NPairsROnly,
   maplist(arg(1),PairsROnly,RemainingR),
   maplist(arg(1),PairsLOnly,RemainingL))).

combine_training(TestID,A,B,In012,Out012):-   
  obj_group_pair(TestID,_+A,In0,Out0),
  obj_group_pair(TestID,_+B,In1,Out1), A<B,
  pairs_agree_or_select(In0,In1,In01),
  pairs_agree_or_select(Out0,Out1,Out01),
  dif(C,A),dif(C,B), 
  dif(D,A),dif(D,B), dif(D,C),
  ignore((obj_group_pair(TestID,_+C,In2,_),pairs_agree_or_select(In01,In2,In012))),
  ignore((obj_group_pair(TestID,_+D,_,Out2), pairs_agree_or_select(Out01,Out2,Out012))),  
  ignore(In012=In01),ignore(Out012=Out01).





append_LR(Prev,Mappings,RestLR):- 
  flatten([Prev,Mappings],RestLR),!.

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  maybe_remove_bg(RHSObjs,RHSObjs1), \=@=(RHSObjs,RHSObjs1),!,
  must_det_ll((calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs1,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  LHSObjs==[], RHSObjs == [], !, 
  Info = info(Step,IsSwapped,Ctx,leftover,TestID,ExampleNum),
  append_LR([call(assert_test_property(TestID,ExampleNum,deps,perfect_balance(Info)))],Prev,RestLR).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   Info = info(Step,IsSwapped,Ctx,leftover,TestID,ExampleNum),
   RHSObjs==[], !, 
    must_det_ll((maplist(into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,Info),
     LHSObjs,Mappings),append_LR(Prev,[call(assert_test_property(TestID,ExampleNum,deps,ignore_rest(Info))),Mappings],RestLR))).

calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjsNil,RHSObjs,RestLR):- 
   LHSObjsNil==[], !, 
    incr_cntx(Ctx,IncrCtx),
    incr_step(Step,IncrStep),
    %incr_step(Step,IncrStep),
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
    member(Type=LHSObjs,[perfect_combo=PrevLHS,perfect_combo=PrevRHS]),
      select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
      must_det_ll((
      remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
      remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
      make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,IncrCtx,Prev,Left,Right,Pairs),
      append_LR(Prev,Pairs,NewPrev),
      
      calc_o_d_recursively(TestID,ExampleNum,IsSwapped,IncrStep,IncrCtx,NewPrev,LHSRest,RHSRest,RestLR))).

/*
calc_o_d_recursively(TestID,ExampleNum,IsSwapped,_Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], !, must_det_ll((
    incr_cntx(Ctx,IncrCtx),
    %incr_step(Step,IncrStep),
    into_list(Prev,NewLHS),
    calc_o_d_recursively(TestID,ExampleNum,IsSwapped,10,IncrCtx,Prev,NewLHS,RHSObjs,RestLR))).
*/
calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,LHSObjs,[Right],RestLR):- 
  sort_by_jaccard(Right,LHSObjs,[A,B|C]),
  make_pairs(TestID,ExampleNum,assumed,IsSwapped,Step,Ctx,[],[B,A],Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  calc_o_d_recursively(TestID,ExampleNum,IsSwapped,Step,Ctx,NewPrev,C,[],RestLR),!.


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
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  bonus_sort_by_jaccard(Prev,Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(from_left,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

select_pair(from_right,Prev,LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
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
 pp_ilp(PropObjsounts),
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

cto_aa(A,AA):- atom(A),!,AA=A.
cto_aa(s(A),AA):- nonvar(A), !, cto_aa(A,AAA),atom_concat(s_,AAA,AA).
cto_aa(A,AA):- format(atom(AA),'~w',[A]).

%make_pairs(TestID,ExampleNum,Type,s(IsSwapped),Step,Ctx,Prev,LHS,RHS,GRP):- nonvar(IsSwapped),!,
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,RHS,LHS,GRP).
%make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,LHS,RHS,GRP):- Prev\==[], !, 
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],Prev,LHS,NLHS),
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],NLHS,RHS,GRP).
make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,_Prev,LHS,RHS,GRP):-
  Info = info(Step,IsSwapped,Ctx,TypeO,TestID,ExampleNum),
  must_det_ll((
 listify(LHS,LHSL),maplist(obj_in_or_out,LHSL,LCtx),maplist(cto_aa,LCtx,LCtxA),atomic_list_concat(LCtxA,'_',LP),
 listify(RHS,RHSL),maplist(obj_in_or_out,RHSL,RCtx),maplist(cto_aa,[Type,LP|RCtx],AA),atomic_list_concat(AA,'_',TypeO))),
  
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

into_lst(ObjsL,[]):- ObjsL==[],!.
into_lst(ObjsL,[ObjsL]):- \+ compound(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_gridoid(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_grid(ObjsL),!.
into_lst(ObjsL,Lst):- is_list(ObjsL),!,maplist(into_lst,ObjsL,LstL),append(LstL,Lst).
into_lst(Grp,Lst):- is_mapping(Grp), get_mapping_info_list(Grp,_,List),!,into_lst(List,Lst).
into_lst(Grp,Lst):- arg(_,Grp,List),is_list(List),!,into_lst(List,Lst).
into_lst(ObjsL,[ObjsL]).

%solve_obj(_VM,_TestID,_ExampleNum,_IN_OUT,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,IN_OUT,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IN_OUT_Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IN_OUT_Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IN_OUT_Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,IN_OUT,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IN_OUT_Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  solve_obj(VM,TestID,ExampleNum,IN_OUT_Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IN_OUT_Start,ROptions,Objs,ObjsO).


*/

has_individuals(TestID):- var(TestID), !, ensure_test(TestID), has_individuals_real(TestID).
has_individuals(TestID):- has_individuals_real(TestID),!.
has_individuals(TestID):- warn_skip(has_individuals(TestID)),!.
has_individuals_real(TestID):-  
 forall(current_example_nums(TestID,ExampleNum),
  (arc_cache:individuated_cache(TestID,TID,GID,_,Objs), sub_var(ExampleNum,(TID,GID)), Objs\==[])),!.
 
ensure_individuals(TestID):- var(TestID),!,ensure_test(TestID),ensure_individuals(TestID).
ensure_individuals(TestID):- has_individuals_real(TestID),!.
ensure_individuals(TestID):- load_file_dyn_pfc(TestID),has_individuals_real(TestID),!.
ensure_individuals(TestID):- 
 time((with_individuated_cache(true,
  once((with_pair_mode(whole_test, ensure_individuals1(TestID))))))), 
 save_test_hints_now(TestID).

% ensure_individuals1 tries the ensure_individuals2 
ensure_individuals1(TestID):- has_individuals_real(TestID),!.
ensure_individuals1(TestID):- 
  ensure_test(TestID),
    ignore(once((with_pair_mode(whole_test, 
          ensure_individuals2(TestID)),
    has_individuals_real(TestID)))),!.
 
ensure_individuals2(TestID):- ignore((ExampleNum=trn+_)),
  print_collapsed(200, forall( kaggle_arc(TestID,ExampleNum,GridIn,GridOut),
           individuate_pair(complete,GridIn,GridOut,_InC,_OutC))).
ensure_individuals2(TestID):- warn_skip(ensure_individuals2(TestID)),!.

ensure_individuals2(TestID):- once(with_luser(menu_key,'i',once(ndividuator(TestID)))).
ensure_individuals2(TestID):- once(with_luser(menu_key,'o',once(ndividuator(TestID)))).
ensure_individuals2(TestID):- calc_propcounts(TestID).


use_pair_info.
no_pair_info:- \+ use_pair_info.

gather_set(Ctx,Goal):-
  copy_term(Ctx+Goal,NRV+Copy),
  no_repeats_var(NRV), !, 
  call(Copy),Ctx=NRV.

p_to_utbs(TestID,Ctx,P,UTBLists):-
 findall(UPB2,
  gather_set(UPB2,(map_pairs_info_io(TestID,_ExampleNum,Ctx,_Step,_TypeO,_A,_B,_USame,_UPA2,UPB2),member(P,UPB2))),UTBLists).

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

%  is_post_objs(TestID,IN_OUT,PostObjs),include(has_prop(P),PostObjs,PostObjsO).


%map_pairs_info(TestID,Ctx,P,Step):- !, map_pairs_info_io(TestID,_ExampleNum,Ctx,Step,_TypeO,_A,_B,_USame,_InFlatProps,UPB2),member(P,UPB2),nop(ok_deduce(P)).
ensure_props_change(TestID,IO,P):-  props_change(TestID,IO,P).

map_pairs_info(TestID,IO,P,Step):-
  no_repeats_var(IOP),
  ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
  map_pairs_info2(TestID,IO,P,Step),
  IOP=IO+P.

:- abolish(good_conseq/4).
:- dynamic(good_conseq/4).

map_pairs_info2(TestID,IO,P,_Step):- nonvar(P),nonvar(IO),good_conseq(TestID,IO,P,YN),!,YN=yes.
map_pairs_info2(TestID,IO,P,Step):- 
 var(P), \+ \+ is_accompany_changed_db(TestID,IO,_,_), 
  is_accompany_changed_db(TestID,IO,P,_),map_pairs_info2(TestID,IO,P,Step).
  
  
map_pairs_info2(TestID,IO,P,Step):-
 ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
 no_repeats_var(IOP),
 (map_pairs_info3(TestID,IO,P,Step)*->asserta(good_conseq(TestID,IO,P,yes));(asserta(good_conseq(TestID,IO,P,no)),fail)),
 IOP=IO+P.

%map_pairs_info2(TestID,IO,P,Step):- nonvar(P),!.

map_pairs_info3(TestID,IO,P,Step):- 
  %ensure_individuals(TestID),
  %ensure_propcounts(TestID),
  %learn_object_dependancy(TestID),
  
  (var(IO)->gather_set(Ctx,pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps));true),
  %IN_OUT = in, Ctx = in_out,
  %gather_set(P,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,PB2))).
  gather_set(P,(
      %nop(gather_set(Step,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,UPB2)))),
      map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,UPB2),member(P,UPB2),ok_deduce(P))),
  %p_to_utbs(TestID,Ctx,P,UTBLists),  
  %common_members(UTBLists,Members),
  %member(P,Members),

  ignore(gather_set(Step,(pair_obj_props(TestID,ExampleNum,
      Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps)))),
  io_to_cntx(IO,Ctx).
  

map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2):-
 pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2).


diff_l_r(InL,OutL,Same,InPFlat,OutPFlat):-
 must_det_ll((
  (( \+ length(InL,1), OutL=[Out] ) -> sort_by_jaccard(Out,InL,[UseL|_]);UseL=InL),
  flat_props([UseL],PA), flat_props([OutL],PB),
  noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat))).

noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  %=(PA,PA1),=(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,Same,InPFlat,OutPFlat),!.
noteable_propdiffs(PA,PB,Same,InPFlat,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  intersection(PA1,PB1,Same,InPFlat,OutPFlat),!.

propchange_unnoticable(InL,OutL):- InL=@=OutL,!.
propchange_unnoticable(InL,OutL):- make_unifiable_u(InL,AU),make_unifiable_u(OutL,BU), AU\=@=BU,!,fail.
propchange_unnoticable(InL,OutL):- hide_propchange(InL,AA),hide_propchange(OutL,BB),AA=@=BB,!.


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
%hide_propchange2(links_count(PA,_),links_count(PA,_)).
hide_propchange2(giz(example_num(ExampleNum)),giz(example_num(ExampleNum))).
hide_propchange2(giz(gid(_)),giz(gid(_))).
hide_propchange2(giz(InL),giz(OutL)):- make_unifiable_u(InL,OutL).
hide_propchange2(oid(_),oid(_)).
hide_propchange2((i_o(_)),(i_o(_))).
hide_propchange2(In,Out):- once((sub_term(E,In),is_grid(E),number_fg_colors(E,G),subst001(In,E,G,Mid))),In\=@=Mid,!,hide_propchange(Mid,Out).
hide_propchange2(grid_rep(InL,G),grid_rep(InL,G)).
hide_propchange2(iz(X),iz(Y)):-!,hide_propchange2((X),(Y)).
hide_propchange2(IN_OUT,IN_OUT).

hide_propchange1(iz(symmetry_type(_,False))):- False == false.
hide_propchange1(iz(symmetry_type(_,False))):- False == true.
%hide_propchange1(pg(_,_,_,_)).
hide_propchange1(line(sees(_),_)).
hide_propchange1(pg(_,_,rankLS,_)).
hide_propchange1(iz(P)):-!,hide_propchange1(P).
hide_propchange1(P):- \+ ok_notice(P),!.
%hide_propchange1(P):- make_unifiable_u(P,U),!,P=@=U,!.

hide_propchange(PA,PB):- hide_propchange2(PA,PA1),PA\=@=PA1,!,hide_propchange(PA1,PB).
hide_propchange(PA,PA).

remove_o_giz(OID,Out):- atom(OID),!,indv_props_list(OID,In),remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- \+ compound(In),!,Out=In.
remove_o_giz(obj(In),Out):- nonvar(In),!,remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- is_group(In),mapgroup(remove_o_giz,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):- maplist(hide_propchange,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
%remove_o_giz(In,Out):- remove_giz(In,Out),!.
remove_o_giz(Out,Out).

m_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable(E,U),select(U,More,UMore),other_val(E,U),the_min_unifier(U,E,S),!,m_unifiers([S|UMore],Out).
m_unifiers(IO,IO).
o_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable(E,U),select(U,More,UMore),other_val(E,U),the_or_unifier(U,E,S),!,o_unifiers([S|UMore],Out).
o_unifiers(IO,IO). 
the_or_unifier(U,E,(U;E)).
























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
    ((enum_object_ext(O),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      has_prop(P,O))),[I|L]),
  indv_props_list(I,List),
  findall(U,(member(U,List),U\=@=P,ok_notice(U),forall(member(E,L),has_prop(U,E))),Can).


prop_cant(TestID,IO,P,Set):-
  props_change(TestID,IO,P),
  findall(Cant,
    ((enum_object(O),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      not_has_prop(P,O),indv_props_list(O,List),member(Cant,List),ok_notice(Cant))),Flat),
   list_to_set(Flat,Set).

enum_object_ext(O):-
  ensure_test(TestID),
  current_example_nums(TestID,ExampleNum),
  once((obj_group_io(TestID,ExampleNum,out,Objs),Objs\==[])),member(O,Objs).


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

print_scene_change_rules(Why,TestID):-
 ensure_test(TestID),
 must_det_ll((
  banner_lines(cyan,4),
  print_set(Why,is_accompany_changed_db),
  banner_lines(cyan,4))).


print_set(Why,P3):-
  Ele = is_accompany_changed_db(TestID,IN_OUT,P,PSame),
  findall(Ele,call(P3,TestID,IN_OUT,P,PSame),List),
  sort(List,SetR),reverse(SetR,Set), 
  trace,pp_ilp(Why),
  dash_chars,
  maplist(pp_ilp,Set),!.









has_propcounts(TestID):- 
 forall(current_example_nums(TestID,ExampleNum),
  ( \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(in,IO)),
    \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(out,IO)))).

ensure_propcounts(TestID):- ensure_test(TestID),ensure_propcounts1(TestID).
ensure_propcounts1(TestID):- has_propcounts(TestID),!.
ensure_propcounts1(_TestID):-!.
ensure_propcounts1(TestID):- ensure_individuals(TestID),!.
ensure_propcounts1(TestID):- calc_propcounts(TestID),has_propcounts(TestID),!.
ensure_propcounts1(TestID):- 
  once((with_pair_mode(whole_test,
    with_luser(menu_key,'o',once(ndividuator(TestID)))))),has_propcounts(TestID),!.
ensure_propcounts1(TestID):- show_prop_counts(TestID), has_propcounts(TestID),!.
ensure_propcounts1(_).

props_change(TestID,IO,P):- map_pairs_info(TestID,IO,P,_Step).
props_change2(TestID,IO,P):-
%  ensure_propcounts(TestID),
  %ensure_prop_change(E),
  findall(Q-I_or_O,counts_change(TestID,_,I_or_O,Q,_,_),L),list_to_set(L,S),!,member(P-IO,S),ok_deduce(P).
%ensure_prop_change(IO,P):- (var(P)->props_change(_TestID,IO,P);true).

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
 nop(\+ is_accompany_changed_db(TestID,_,_,_) -> compute_scene_change(TestID) ; true).



compute_scene_change(TestID):-
 ensure_test(TestID),
 with_pair_mode(whole_test,  
 must_det_ll((
  clear_scene_rules(TestID),
  banner_lines(orange,4),
  compute_scene_change_pass1(TestID),  
  banner_lines(orange,4),
  compute_scene_change_pass2(TestID),
  banner_lines(yellow,4),  
  compute_scene_change_pass3(TestID),
  banner_lines(blue,4),

  /*
  compute_scene_change_pass4(TestID),*/
  !))).


compute_scene_change_pass1(TestID):- 
  show_object_dependancy(TestID),!.
  %learn_object_dependancy(TestID).

compute_scene_change_pass2(TestID):- 
  forall(ensure_props_change(TestID,Ctx,P),
    forall(prop_can(TestID,Ctx,P,PSame),
      assert_accompany_changed_db(TestID,Ctx,P,PSame))).

compute_scene_change_pass3(TestID):-
 must_det_ll((
  print_scene_change_rules(pass2,TestID),
  set_of_changes(TestID,compute_scene_change_pass3a(TestID)),
  print_scene_change_rules(pass3a,TestID),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes1)),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)),
  print_scene_change_rules(pass3b1,TestID),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes2)),
  print_scene_change_rules(pass3b2,TestID),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes3)),
  print_scene_change_rules(pass3b3,TestID),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes4)),
  print_scene_change_rules(pass3b4,TestID),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)),
  print_scene_change_rules(pass3c,TestID))),!.

compute_scene_change_pass3a(TestID,IN_OUT-P):- 
   findall(PSame,is_accompany_changed_db(TestID,IN_OUT,P,PSame),List),
   List=[_,_|_],
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
   update_accompany_changed_db(TestID,IN_OUT,P,SameS).
compute_scene_change_pass3a(_,_).

compute_scene_change_pass3b(TestID,P4,IN_OUT-P):-
   findall(PSame,is_accompany_changed_db(TestID,IN_OUT,P,PSame),List),
   flatten(List,SameF), variant_list_to_set(SameF,SameS),
   call(P4,TestID,IN_OUT,P,SameS,Kept), Kept\==[],!,
  update_accompany_changed_db(TestID,IN_OUT,P,Kept).
compute_scene_change_pass3b(_,_,_). 

compute_scene_change_pass3c(_,_):-!.
compute_scene_change_pass3c(TestID,IN_OUT-P):-
  is_accompany_changed_db(TestID,IN_OUT,P,PSame),
  findall(DSame,
     (is_accompany_changed_db(TestID,IN_OUT,DP,DSame), 
      once(other_val(DP,P);DP=P),at_least_one_overlap(DSame,PSame)),
   SL),  SL = [_,_|_],
  common_members(SL,Commons),
  forall((is_accompany_changed_db(TestID,IN_OUT,DP,DSame),once(other_val(DP,P);DP=P)),
      (intersection(DSame,Commons,_,Kept,_),
        ignore((Kept\==[],update_accompany_changed_db(TestID,IN_OUT,P,Kept))))).
compute_scene_change_pass3c(_,_).


compute_scene_change_pass4(TestID):-
   compute_scene_change_pass3(TestID).

set_of_changes(TestID,P1):-
  findall_vset(IN_OUT-P,ensure_props_change(TestID,IN_OUT,P),Ps),
  maplist(P1,Ps).


update_accompany_changed_db(TestID,IN_OUT,P,Kept):- Kept\==[],
   forall(retract(is_accompany_changed_db(TestID,IN_OUT,P,_)),true),
   assert_accompany_changed_db(TestID,IN_OUT,P,Kept).
   
assert_accompany_changed_db(_TestID,_IN_OUT,_P,Kept):- Kept==[],!.
assert_accompany_changed_db(TestID,IN_OUT,P,Kept):- 
   assert_become_new(is_accompany_changed_db(TestID,IN_OUT,P,Kept)).

at_least_one_overlap(DSame,PSame):-
  member(DS,DSame),member(S,PSame),
  (DS=@=S;other_val(S,DS)),!.

correct_antes1(TestID,IN_OUT,P,PSame,SL):- 
  %rev_in_out_atoms(OI,IN_OUT),
  findall(S,
   (member(S,PSame),
     \+ \+ ((
       forall((is_accompany_changed_db(TestID,IN_OUT,DP,DSame),at_least_one_overlap(DSame,PSame)),
          ((P==DP)-> true; (member(DS,DSame),  \+ negated_s_lit(S,_), other_val(S,DS))))))),
   SL), SL\==[],!.
correct_antes1(_TestID,_IN_OUT,_P,PSame,PSame).

correct_antes2(TestID,IN_OUT,P,PSame,Kept):- 
  %rev_in_out_atoms(OI,IN_OUT),
  findall( ( \+ S),
   ((is_accompany_changed_db(TestID,IN_OUT,DP,DSame), 
     other_val(P,DP), at_least_one_overlap(DSame,PSame),
     member(S,DSame),  \+ negated_s_lit(S,_), make_unifiable_u(S,SU), \+ member(SU,PSame))), SL),
  SL\==[],
  append(PSame,SL,Kept),Kept\==[],!.
correct_antes2(_TestID,_IN_OUT,_P,PSame,PSame).

correct_antes3(_TestID,_IN_OUT,P,PSame,Kept):- fail,
  %rev_in_out_atoms(OI,IN_OUT),
  findall(S,
     (member(S,PSame),\+ once(other_val(P,S);S=@=P)),
   Kept), Kept\==[],!.
correct_antes3(_TestID,_IN_OUT,_P,PSame,PSame).


correct_antes4(_TestID,_IN_OUT,_P,PSame,Kept):- 
  %rev_in_out_atoms(OI,IN_OUT),
  findall(S,
      (member(E,PSame), ensure_xformed(E,S)),
   Kept), Kept\==[],!.
correct_antes4(_TestID,_IN_OUT,_P,PSame,PSame).

negated_s_lit(N,P):- compound(N), N = ( \+ P ). 

correct_antes5(TestID,IN_OUT,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_computed(TestID,IN_OUT,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   pred_intersection(other_val,PSame,USame,Kept,_,_,_),Kept\==[].
correct_antes5(_TestID,_IN_OUT,_P,PSame,PSame).

ensure_xformed(pg(_,A,B,C),pg(_,A,B,C)):-!.
ensure_xformed(A,A).

/*
solve_obj_group(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):-
 forall(kaggle_arc(TestID,trn+N,_,_),
  ( findall(Out,((arc_cache:map_pairs(TestID,_,trn+N,info(Step,_,in_out,perfect_in_out,_,trn+N),PreObjs,Out),
      indv_props_list(Out,PropsO),
       closest_object(Out,PreObjs,PreObj),
       rewrite_rules_for(PreObj,Out,Sames,Diffs),
       ,OutL),
    findall(PreObjs,arc_cache:map_pairs(TestID,_,trn+N,info(0,_,in_out,_,_,trn+N),PreObjs,Out),PreObjs),
  homogenize(OutL,Sames,Diffs),
*/


