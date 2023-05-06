/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- include(kaggle_arc_header).

:- dynamic(is_for_ilp/4).
:- abolish(ac_unit/4).
:- dynamic(ac_unit/4).
clear_scene_rules(TestID):-   
  abolish(ac_unit/4),dynamic(ac_unit/4),
  abolish(arc_cache:trans_rule_db/4),dynamic(arc_cache:trans_rule_db/4),
  %forall(ac_unit(TestID,Ctx,P,PSame),ignore(retract(ac_unit(TestID,Ctx,P,PSame)))),!,
  clear_object_dependancy(TestID).


% Define predicates that shouldn't be noticed
%dont_notice(global2G(_,_)).
dont_notice(giz(_)).
dont_notice(iz(i_o(_))).
dont_notice(iz(stype(_))).
dont_notice(global2G(_,_)).
dont_notice(iz(symmetry_type(rollD, _))).
dont_notice(link(contains,_)).
dont_notice(links_count(sees, _)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(occurs_in_links(sees,_)).
dont_notice(oid(_)).
dont_notice(pg(_,pen(_), rankLS ,_)).
dont_notice(pg(_,iz(_),rankLS,_)).
%dont_notice(pg(_, iz(_), rankLS, largest)).
%dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(P):- compound(P),arg(_,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(P):- compound(P),arg(_,P,E),E==norm,!,fail.
dont_notice(F):- \+ atom(F),!,fail.
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).
dont_notice(grid_rep).

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
dont_deduce(grid_ops(comp,_)). 
%dont_deduce(iz(stype(_))). 
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
do_deduce(grid_rep(norm,_)). % pen([cc(blue,1)]),pg(_1489874,mass(_1489884),rank1,4).
do_deduce(grid_ops(norm,_)). % pen([cc(blue,1)]),pg(_1489874,mass(_1489884),rank1,4).

% Predicate to check if P should be deduced
ok_deduce(obj(L)):- nonvar(L),!.
ok_deduce(P):- \+ \+ dont_deduce(P), !, fail.
ok_deduce(P):- \+ \+ do_deduce(P),!.
ok_deduce(P):- good_for_rhs(P),!.
%ok_deduce(P):- \+ \+ dont_notice(P),!,fail.









% Check if two values have the same property names but are not equal

other_val(X1,X2):- negated_s_lit(X1,P1), 
  ( negated_s_lit(X2,P2) -> other_val(P1,P2) ; other_val(X2,P1)).
other_val(X1,X2):- X1\=@=X2, same_prop_names(X1,X2),!.
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),!,  U1 =@= U2.

% Helper predicate to create a unifiable version of a term
make_unifiable_u(P,U):- copy_term(P,PP),make_unifiable_u1(PP,U),!.
make_unifiable_u1(Atom,U):- is_ftVar(Atom),!,Atom=U.
make_unifiable_u1(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u1(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]),!.

make_unifiable_u1(P,U):- assume_prop(P),!,P=U.
make_unifiable_u1(X1,U1):- make_unifiable_cc(X1,U1),!.
make_unifiable_u1(X1,X1).

make_unifiable_ov(I,O):- make_unifiable_u(I,O),!.

make_unifiable_f(I,O):- make_unifiable_ov(I,O).
make_unifiable_f(I,O):- same_functor(I,O),!.


same_context(IO,Ctx):- nonvar(Ctx),!,io_to_cntx1(Out,In_Out_Out),once(Ctx==In_Out_Out;Ctx==Out),!, (once(IO=In_Out_Out;IO=Out)).
same_context(IO,Ctx):- nonvar(IO),!, io_to_cntx1(Out,In_Out_Out), once(IO==In_Out_Out;IO ==Out),!, (once(Ctx=In_Out_Out;Ctx=Out)).
same_context(IO,Ctx):- freeze(IO,same_context(IO,Ctx)),freeze(Ctx,same_context(IO,Ctx)).

io_to_cntx(IO,Ctx):- io_to_cntx1(IO,Ctx).
io_to_cntx1(in,in_out).
%io_to_cntx1(in,in_out_out).
io_to_cntx1(out,in_out_out).
io_to_cntx1(out,s(_)).
io_to_cntx1(X,X).



solve_via_scene_change(TestID):-  
 must_det_ll((
  cls,
  ensure_test(TestID), %make,
  %detect_pair_hints(TestID),
  time(learn_grid_size(TestID)),
  clear_scene_rules(TestID),
  ensure_propcounts(TestID),
  save_test_hints_now(TestID),
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
  ensure_scene_change_rules(TestID),
  print_object_dependancy(TestID),
  print_scene_change_rules(solve_via_scene_change_rules,TestID),
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
  count_difs(ExpectedOut,OurSolution,Errors))),
  (Errors == 0 ->  
   (banner_lines(green,4),
    print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
    print_scene_change_rules(rules_at_time_of_success,TestID),
    banner_lines(green,4))
    ;(banner_lines(red,10),!,
      show_time_of_failure(TestID),
      banner_lines(red,10),
      print_scene_change_rules(rules_at_time_of_failure,TestID),
      print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
      banner_lines(red,1),
      %if_t((findall(_,ac_rules(_,_,_,_),L), L == []), (get_scene_change_rules(TestID,pass2_rule_new,Rules),pp_ilp(Rules))),banner_lines(red,5),
      %print_object_dependancy(TestID),
      !,fail)).

resize_our_solution(PX,PY,OurSolution1,OurSolution):-
  once(ground(PX+PY)
     ->resize_grid(PX,PY,OurSolution1,OurSolution)
      ;notrace(=(OurSolution1,OurSolution));notrace(trim_outside2(OurSolution1,OurSolution))).

maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution):-
  predict_grid_size_now(TestID,In,PX,PY),resize_our_solution(PX,PY,OurSolution1,OurSolution),!.


enter_solve_obj(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):- solve_obj_group(VM,TestID,ExampleNum,ROptions,in_out,Objs,ObjsO),!.

enter_solve_obj(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):- 
  solve_obj_group(VM,TestID,ExampleNum,ROptions,in_out,Objs,ObjsM1),
  solve_obj_group(VM,TestID,ExampleNum,ROptions,in_out_out,ObjsM1,ObjsM2),
  solve_obj_group(VM,TestID,ExampleNum,ROptions,s(_),ObjsM2,ObjsO), ObjsO \==[],!.

score_rule(Ways,Obj,Rule,Score):- is_object(Rule), \+ is_object(Obj),!,score_rule(Ways,Rule,Obj,Score).

score_rule(Ways,Obj,Rule,Score):- 
  into_lhs(Rule,PCond), into_rhs(Rule,P), 
  % indv_props_list(Obj,Props), \+ member(P,Props), %\+ \+ ((member(E,Props),member(E,PCond))),
   once( ( \+ is_bg_object(Obj) ); sub_var(black,PCond)),
    score_rule(Ways,Obj,PCond,P,Score).

score_rule(exact,Obj,PCond,_P,Score):-  score_all_props(PCond,Obj,S0),S0>0.3,!,Score=1000.
score_rule(_Ways,Obj,PCond,_P,Score):-
   obj_atoms(Obj,A),
   obj_atoms(PCond,B),
     intersection(A,B,Good,_Extra,_Bad),
     length(Good,Score).

has_all_props(CanL,Obj):- maplist(inv_has_prop(Obj),CanL).
score_all_props(CanL,Obj,Score):- maplist(inv_has_prop_score(Obj),CanL,ScoreL),sumlist(ScoreL,Score),!.

assume_prop(P):- \+ \+ assume_prop1(P),!.
assume_prop(P):- \+ \+ assume_prop2(P).
assume_prop1(iz(info(_))).
assume_prop1(P):- dont_notice(P).
assume_prop2(giz(_)).
assume_prop2(grid_sz(_)).
assume_prop2(global2G(_,_)).
assume_prop2(was_oid(_)).
assume_prop2(oid(_)).


max_prop_score(P,0.1):- assume_prop1(P),!.
max_prop_score(P,0.2):- assume_prop2(P),!.
max_prop_score(P,1.0):- ground(P),!.
max_prop_score(P,0.0):- is_unbound_prop(P),!.
max_prop_score(_,0.7).

inv_has_prop(Obj,Prop):- has_prop(Prop,Obj),!.
inv_has_prop(Obj,Prop):- inv_has_prop_score(Obj,Prop,Score),Score>0.
inv_has_prop_score(Obj,Prop, Score):- max_prop_score(Prop,Score), inv_has_prop2(Obj,Prop).
inv_has_prop2(_O,P):- \+ \+ assume_prop(P),!.
inv_has_prop2(Obj,pg(A,B,C,D)):- !, has_prop(Obj,pg(A,B,C,D)).
inv_has_prop2(Obj, \+ Prop):- !, \+ inv_has_prop(Obj,Prop).
inv_has_prop2(Obj,grid_ops(norm,Props)):- !, has_prop(grid_ops(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,grid_rep(norm,Props)):- !, has_prop(grid_rep(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,Prop):- has_prop(Prop,Obj).

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
  (Rule = (Ctx:rhs(P):- obj_atoms(PCond))),
  %io_to_cntx(IO_,Ctx), 
  findall_vset_R(Rule,(ac_rules(TestID,Ctx,P,PCond)), Rules),
  member(Ways-Strategy,[exact-_,two_way-one_to_one,_-_]), 
  apply_rules_to_objects(Ways,Strategy,Rules,Objs,Todo), Todo\==[],
  maplist(into_solid_grid_strings,Todo,PStr),
  unwonk_ansi(PStr,PPStr),
  pp_ilp((see_Strategy(Ways-Strategy)=PPStr)), Todo\==[],
  run_todo_output(VM,Todo,ObjsO),ObjsO\==[],!.

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
    (prop_to_can(TestID,IO_,P,_O,_Can1,Cant,Can),
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

%copy_obj(Rules,Objs,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
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
   

%              findall(grp(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IO_2,Info,Pre,Post),List),
%              variant_list_to_set(List,Set)
 
copy_obj(_VM,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj).
%solve_obj(VM,_TestID,_ExampleNum,IO_,_ROptions,Obj,OObj):-
%  edit_object(VM,pen([cc(black,1)]),Obj,OObj).
%solve_obj(VM,_TestID,_ExampleNum,IO_,_ROptions,_Obj,[]).
*/

run_todo_output(VM,[],NewObjs):- NewObjs = VM.objs,!.
run_todo_output(VM,[apply(Rule,Obj)|TODO],NewObjs):-
  edit_object(VM,Rule,Obj),
  run_todo_output(VM,TODO,NewObjs).

clone_object(I,O):- duplicate_term(I,O).

edit_object(_VM,Ps,_Obj):- Ps==[],!.
%edit_object(VM,Ps,Obj):- Ps==[],!,edit_object(VM,pen([cc(black,1)]),Obj).
edit_object(VM,[H|T],Obj):- !,edit_object(VM,H,Obj),edit_object(VM,T,Obj).
edit_object(VM,copy_step(_,perfect_in_out),Obj):- addRObjects(VM,Obj).
edit_object(VM,creation_step(_,_,Props),Obj):-
  clone_object(Obj,NewObj), edit_object(VM,Props,NewObj).
edit_object(VM,Ps,Obj):-
  must_det_ll((
   wots(SS,print(Ps)),
   override_object_1(VM,Ps,Obj,NewObj),
   remObjects(VM,Obj),
   addOGSObjects(VM,NewObj),
   addObjects(VM,NewObj),
   into_solid_grid([NewObj],SG),SG=_,
   dash_chars,
   print_ss(override_object(SS),[Obj],[NewObj]),
   nop((
   indv_props_list(Obj,PL1),
   indv_props_list(NewObj,PL2),
   intersection(PL1,PL2,_Same,Removed,Added),
    pp_ilp(([[removed=Removed],[added=Added]])))))).

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),  
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),rebuild_from_globalpoints(VM,Obj,GPoints,NewObj).
override_object_1(VM,Term,I,O):- sub_compound(rhs(P),Term), !,  override_object_1(VM,P,I,O).
override_object_1(VM,Term,I,O):- sub_compound(edit(P),Term), !,  override_object_1(VM,P,I,O).
override_object_1(VM,Term,I,O):- sub_compound(edit(_,_,P),Term), !, override_object_1(VM,P,I,O).
%override_object_1(VM,Term,I,O):- sub_term(Sub,Term), compound(Sub),Sub=edit(_,_,P),  !, pp_ilp(Term), I=O,!. %override_object_1(VM,P,I,O).


override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.


mapping_step(    in_out).
mapping_step( in_in_out).
mapping_step(in_out_out).
mapping_step(   out_out).


p_of_post(P,Post):- indv_props_list(Post,Props),member(P,Props).



from_same_pair(Post,Pre):-
  has_prop(giz(example_num(trn+N)),Post),
  has_prop(giz(example_num(trn+N)),Pre).
     
     
obj_in_or_out(Pair,IO_):- is_mapping(Pair),!,
    get_mapping_info(Pair,Info,_In,_Out),arg(3,Info,IO_).
obj_in_or_out(Obj,IO_):- must_det_ll(is_object(Obj)),has_prop(giz(g(I_O)),Obj),!,I_O=IO_.
obj_in_or_out(Obj,IO_):- has_prop(iz(i_o(I_O)),Obj),!,I_O=IO_.
%obj_in_or_out(Obj,I_O):- is_input_object(Obj)-> IO_ =out ; IO_ =in.

is_pre_cond_obj(Obj,in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,in);obj_in_or_out(Obj,out).
is_pre_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,s(X)):- nonvar(X), is_pre_cond_obj(Obj,out).
is_pre_cond_obj(Obj,IO_):- obj_in_or_out(Obj,IO_).
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

obj_group_pair1(TestID,ExampleNum,InC,OutC):-
   current_example_nums(TestID,ExampleNum),
   no_repeats_var(OutC), % set_example_num(ExampleNum),
   obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut.

obj_group_pair(TestID,ExampleNum,InC,OutC):-
   %findall(NV,(arc_test_property(TestID,ExampleNum,N,V),append_term(N,V,NV)),Props),
  
  obj_group_pair1(TestID,ExampleNum,InCC,OutCC),
  InC = InCC, OutC = OutCC.
  %Grid= VM.start_grid,
  %hv_point_value(1,1,Grid,PointNW),
  %hv_point_value(1,V,Grid,PointSW),
  %hv_point_value(H,1,Grid,PointNE),
  %hv_point_value(H,V,Grid,PointSE),
%%  once((kaggle_arc(TestID,ExampleNum,In,Out),grid_props(In,InProps),grid_props(Out,OutProps))),
%%  InC = [obj([giz(g(in))|InProps])|InCC], OutC = [obj([giz(g(out))|OutProps])|OutCC].
  %append(Props,[mass(0),vis2D(H,V),birth(named_grid_props),loc2D(1,1),iz(flag(always_keep)),iz(media(image)),iz(flag(hidden))],AllProps),
  %make_indiv_object(VM,AllProps,[PointNW,PointSW,PointNE,PointSE],_),!.
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
 clear_object_dependancy(TestID),
 learn_object_dependancy(TestID),
 print_object_dependancy(TestID).

% =============================================================
learn_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
  must_det_ll((
  ensure_individuals(TestID),
 ignore((ExampleNum=trn+_)),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
	  learn_object_dependancy(TestID,ExampleNum)),
  merge_object_dependancy(TestID))).

learn_object_dependancy(TestID,ExampleNum):-
 %current_example_nums( TestID,ExampleNum),
 must_det_ll(( obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
   maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs))).

maybe_learn_object_dependancy(TestID,ExampleNum,_RHSObjs,_LHSObjs):-
  arc_cache:prop_dep(TestID,ExampleNum,_,_,_,_,_,_,_),
  arc_cache:trans_rule_db(TestID,ExampleNum,_,_),
  !.

maybe_learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

learn_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):- 
 must_det_ll((
              RHSObjs\==[],LHSObjs\==[],
  Step=0,Ctx=in_out,IsSwapped=false,
  normalize_objects_for_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs,RHSObjsOrdered,LHSObjsOrdered),
    %prinnt_sbs_call(LHSObjsOrdered,RHSObjsOrdered),  
  TM = _{rhs:RHSObjsOrdered, lhs:LHSObjsOrdered},
  calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,[],LHSObjsOrdered,RHSObjsOrdered,Groups),
  pp_ilp(groups=Groups),
  assert_map_pairs(TestID,ExampleNum,Ctx,Groups))).

normalize_objects_for_dependancy(_TestID,_ExampleNum,RHSObjs,LHSObjs,RHSObjsOrdered,LHSObjsOrdered):-
  include(is_fg_object,LHSObjs,LHSObjsO),
  include(is_fg_object,RHSObjs,RHSObjsO),
  sort_by_jaccard(one(RHSObjsO),LHSObjsO,LHSObjsOrdered),
  sort_by_jaccard(one(LHSObjsO),RHSObjsO,RHSObjsOrdered),
  !.


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



/*
pair_obj_one_rule(TestID,Ctx,id(Ex,Step),Rule):- 
  Rule = r(Type,LHS,RHS,S,L,R, Ex, Step),
  pair_obj_props(TestID,Ex,Ctx,_Info,Step,Type,LHS,RHS,S,L,R).

trans_rules_combined(TestID,Ctx,Combined):-
  findall(Rule1,pair_obj_one_rule(TestID,Ctx,_RuleID1,Rule1),Rules),
  combine_trans_rules(Rules,CombinedRules),
  member(Combined,CombinedRules).


combine_more(Excluded,TestID,Ctx,Rule1,Combined):- 
   pair_obj_one_rule(TestID,Ctx,RuleID2,Rule2),
   \+ member(RuleID2,Excluded),
   combine_rule(Rule1,Rule2,NewRule),
   combine_more([RuleID2|Excluded],TestID,Ctx,NewRule,Combined).
combine_more(_Excluded,_TestID,_Ctx,Combined,Combined).
*/
assert_map_pairs(TestID,ExampleNum,Ctx,Group):- is_list(Group),!,maplist(assert_map_pairs(TestID,ExampleNum,Ctx),Group).
assert_map_pairs(TestID,ExampleNum,Ctx,grp(Info,In,Out)):-!,
 must_det_ll((
  into_list(In,InL),into_list(Out,OutL),

  trans_rule(Info,InL,OutL,TransRules), TransRules \==[],
   
   assert_map_pairs(TestID,ExampleNum,Ctx,TransRules),
  %assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,grp(Info,InL,OutL))),
  once((diff_l_r(InL,OutL,Same,InFlatP,OutPFlat),
   unnumbervars(v5('$VAR'(0),'$VAR'('_'),Same,InFlatP,OutPFlat),UNV))),
                    must_det_ll((UNV = v5(_FG1,_BG1,USame,InFlatProps,OutFlatProps))),
  %pp_ilp(grp(Info,InL,OutL)),!,  
  assertz_new(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,InL,OutL,USame,InFlatProps,OutFlatProps)))).
assert_map_pairs(_TestID,_ExampleNum,_Ctx,call(Rule)):-!,must_det_ll(Rule),!.
assert_map_pairs(TestID,ExampleNum,Ctx,TransRule):-
   assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,TransRule)),!.


merge_object_dependancy(TestID):-
  forall(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,TransRule),
   merge_map_pairs(TestID,ExampleNum,Ctx,TransRule)).
merge_map_pairs(_TestID,_ExampleNum,_Ctx,_TransRule).



:- dynamic(arc_cache:trans_rule_db/4).

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
  /*if_t(( \+ arc_cache:map_pairs(TestID,_,_,_,_,_)),
   ( dash_chars,forall(arc_cache:map_group(TestID,_,_IO_,Group),
    once(((dash_chars,dash_chars,pp_ilp(Group),dash_chars,dash_chars)))))),
  dash_chars,*/
% findall_vset_R(grp(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IO_2,Info,Pre,Post),Set1),
% maplist(pp_ilp,Set1),
 dash_chars,dash_chars,
 %pp_ilp_vset(grp(Info,Pre,Post),pair_obj_info(TestID,_,_,Info,Pre,Post)),

 %More = call(show_cp_dff_rem_keep_add(USame,InFlatP,OutPFlat)),
 with_vset(
   arc_cache:prop_dep(TestID,_ExampleNum,_Ctx,Info,LHS,RHS,_USame,_InFlatProps,_OutFlatProps),
       pp_ilp(grp(Info,LHS,RHS))),

 dash_chars,dash_chars.
 %if_t(Set1 =@= Set2,  wdmsg('Set 2 the same')),
 %if_t(Set1 \=@= Set2,

findall_vset_R(T,G,R):- findall_vset(T,G,S), vsr_set(S,R). %,reverse(R,L).
vsr_set(L,P):- flatten([L],F),vs_set(F,R),reverse(R,P).
vs_set(L,P):- variant_list_to_set(L,S),sort(S,P).
%pp_ilp_vset(G,T):- dash_chars,with_vset(G,pp_ilp(C)).
with_vset(G,C):- term_variables(C,Vs),findall_vset_R(Vs,G,L),forall(member(Vs,L),call(C)).
%:- dynamic(arc_cache:map_pairs/6).
:- abolish(prop_dep/9).
:- abolish(arc_cache:prop_dep/9).
:- dynamic(arc_cache:prop_dep/9).
:- abolish(arc_cache:trans_rule_db/4).
:- dynamic(arc_cache:trans_rule_db/4).
%:- dynamic(arc_cache:causes/5).
/*
pair_obj_props(TestID,ExampleNum,Ctx,Info,Step,TypeO,LHS,RHS,USame,InFlatProps,OutPFlatrops):-
 ensure_test(TestID),
  Info = info(Step,_IsSwapped,Ctx,TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,USame,InFlatProps,OutPFlatrops).

pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,LHS,RHS,USame,InFlatProps,OutPFlatrops):-  
 ensure_test(TestID),
  Info = info(Step,_IsSwapped,Ctx,TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,USame,InFlatProps,OutPFlatrops).

pair_obj_info(TestID,ExampleNum,Ctx,Info,LHS,RHS):-
 ensure_test(TestID),
  Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,_USame,_InFlatProps,_OutPFlatrops).
*/

ok_intersect(L1,L2):-
  member(E1,L1),member(E2,L2),
  other_val(E1,E2),!,fail.
ok_intersect(_,_).

/*
pair_obj_props54321(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- 
 ensure_test(TestID),
  trans_rules_combined(TestID,Ctx,Combined),
  r(Type,LHS,RHS,S,L,R, Ex, Step) = Combined,
  Info = info(Step,_IsSwapped,Ctx,Type,TestID,Ex, Ex).
*/
pair_obj_props54321(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
 ensure_test(TestID),
  (pair_obj_props5(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props4(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props3(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props2(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
   pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R))))).


into_solid_objs(RHS,RHSO):- flatten([RHS],RHSM),
  maplist(into_obj,RHSM,RHSO).


points_to_objects(ShapeType,Obj,Points,IndvPoints,NextScanPoints):- 
    %Points = VM.lo_points,
    %shape_min_points(VM,ShapeType,IndvPoints),
    %copy_term(ShapeType,OptionC),ShapeType=ShapeType,    
  select(C-HV,Points,Rest0), allowed_color(ShapeType,C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(ShapeType,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(_VM,Dir,ShapeType,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints),%!,
  %make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType)),birth(i2(ShapeType))],IndvPoints,Obj),
   % meets_indiv_criteria(VM,ShapeType,IndvPoints),
  %set(VM.lo_points) = NextScanPoints,
  %assumeAdded(VM,Obj),
  %cycle_back_in(VM,OptionC).
  true,
  Obj = obj([globalpoints(IndvPoints)]).
/*
pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- fail, % fail here is since we should not allow any single example to make a rule
  pair_obj_props(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R).

pair_obj_props2(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- 
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2),
  Ex1#<Ex2,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step,Type,LHS,RHS,S,L,R).

pair_obj_props3(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2+Ex3),
  Ex1#<Ex2,Ex2#<Ex3,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+Ex3,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step,Type,LHS,RHS,S,L,R).

pair_obj_props4(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2+Ex3+Ex4),
  Ex1#<Ex2,Ex2#<Ex3,Ex3#<Ex4,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+Ex3,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step123,Type123,LHS123,RHS123,S123,L123,R123),
  pair_obj_props(TestID,trn+Ex4,Ctx,_Info4,Step4,Type4,LHS4,RHS4,S4,L4,R4),
  combine_rule( do_requires, Step123,Type123,LHS123,RHS123,S123,L123,R123, Step4,Type4,LHS4,RHS4,S4,L4,R4, Step,Type,LHS,RHS,S,L,R).

pair_obj_props5(TestID,trn+N,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+N,trn+0+1+2+3+N),
  pair_obj_props(TestID,trn+0,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+1,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+2,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step123,Type123,LHS123,RHS123,S123,L123,R123),
  pair_obj_props(TestID,trn+3,Ctx,_Info4,Step4,Type4,LHS4,RHS4,S4,L4,R4),
  combine_rule( do_requires, Step123,Type123,LHS123,RHS123,S123,L123,R123, Step4,Type4,LHS4,RHS4,S4,L4,R4, Step1234,Type1234,LHS1234,RHS1234,S1234,L1234,R1234),
  pair_obj_props(TestID,trn+N,Ctx,_Info5,Step5,Type5,LHS5,RHS5,S5,L5,R5),
  combine_rule( do_requires, Step1234,Type1234,LHS1234,RHS1234,S1234,L1234,R1234, Step5,Type5,LHS5,RHS5,S5,L5,R5, Step,Type,LHS,RHS,S,L,R).

combine_rule(Rule1,Rule2,NewRule):-
  r(Type1,LHS1,RHS1,S1,L1,R1, Ex1, Step1) = Rule1,
  r(Type2,LHS2,RHS2,S2,L2,R2, Ex2, Step2) = Rule2,
  combine_rule(do_requires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,    
              Step, Type, LHS, RHS, S ,L ,R  ),!,
  r(Type,LHS,RHS,S ,L ,R ,Ex1+Ex2,Step) = NewRule.
combine_rule(Rule1,_Rule2,Rule1).

combine_rule(DoRequires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,   
              Step, Type, LHS, RHS, S, L, R ):-
              ignore(Step1=Step2), ignore(Step=Step2), 

              (DoRequires == do_requires -> (ok_intersect(R1,R2), something_common(R1,R2)) ; true),

              once((maplist(merge_vals,[Type1,LHS1,RHS1],[Type2,LHS2,RHS2],[Type,LHS,RHS]))),
              merge_vals(R1,R2,R),merge_vals(S1,S2,S),
                merge_vals(L1,L2,L),
                pp_ilp(merge_vals(L1,L2,L)).
*/
something_common(R1,R2):- \+ \+ ((member(E1,R1), good_for_rhs(E1),  member(E2,R2), E1=@=E2)).


%must_be_identical(step).

must_be_identical(edit).
must_be_identical(ctx).
must_be_identical(testid).

merge_list_vals(A,B,[E3|C]):- select(E1,A,AA),same_functor(E1,E2),select(E2,B,BB),!,merge_vals(E1,E2,E3),
 merge_list_vals(AA,BB,C).
merge_list_vals(A,B,C):- append_sets(A,B,C).

merge_vals(A,B,C):- atom(A),!,A==B,C=A.
merge_vals(A,B,C):- A=@=B,!,C=A.
merge_vals(A, A, A) :- !.
merge_vals(A,B,C):- A==[],!,B=C.
merge_vals(A,B,C):- B==[],!,A=C.

merge_vals(A,B,C):- is_obj_props(A),is_obj_props(B),!,merge_props(A,B,C).
merge_vals([A1,A2],[B],[C1,C2]):-  !, merge_vals(A1,B,C1),merge_vals(A2,B,C2).
merge_vals([A|AA],[B|BB],[C|CC]):- !, merge_vals(A,B,C), merge_vals(AA,BB,CC).

merge_vals(prop(Name,A),prop(Name,B),prop(Name,C)):- !, merge_vals(A, B, C).
merge_vals(prop(Name, A1, A2),prop(Name, B1, B2), prop(Name, C1, C2)):- !,
  merge_vals(A1, B1, C1),merge_vals(A2, B2, C2).

merge_vals(A,B,C):- ( \+ compound(A) ; \+ compound(B)),!, flatten_sets([A,B],C),!. 
merge_vals(T+A,T+B,C):-!,must_det_ll((C=(T+A+B))).

merge_vals(A,B,A):- functor(A,F,_),must_be_identical(F),!,A=@=B.

merge_vals(info(A),info(B),info(C)):- !, merge_list_vals(A,B,C).
  
%info([step(Step),is_swapped_lr(IsSwapped),ctx(Ctx),why(TypeO),testid(TestID),example(ExampleNum)])
merge_vals(A,B,C):-
  A =  ac_unit(TestID, IO, P1, PSame1),
  B =  ac_unit(TestID, IO, P2, PSame2),!,
  P1=@=P2,
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_db(TestID, IO, P1, PSame1),
  B =  ac_db(TestID, IO, P2, PSame2),!,
  P1=@=P2,
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_db(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_rules(TestID, IO, P1, PSame1),
  B =  ac_rules(TestID, IO, P2, PSame2),!,
  P1=@=P2,
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_rules(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_listing(TestID, IO, P1, PSame1),
  B =  ac_listing(TestID, IO, P2, PSame2),!,
  P1=@=P2,
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_listing(TestID, IO, P1, PSame).

merge_vals(Rule1,Rule2,NewRule):-
  r(Type1,LHS1,RHS1,S1,L1,R1, Ex1, Step1) = Rule1,
  r(Type2,LHS2,RHS2,S2,L2,R2, Ex2, Step2) = Rule2,
  combine_rule(do_requires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,    
              Step, Type, LHS, RHS, S ,L ,R  ),!,
  r(Type,LHS,RHS,S ,L ,R ,Ex1+Ex2,Step) = NewRule.

merge_vals(rhs(A),rhs(B),rhs(C)):- !, same_rhs_operation(A,B),!,merge_vals(A,B,C).

merge_vals(A,B,C):- compound(A),compound(B),var(C),
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(merge_vals,AA,BB,CC),!, compound_name_arguments(C,F,CC).
%merge_vals(obj(A),obj(B),obj(C)):- is_list(A),is_list(B),!,merge_props(A,B,C).
merge_vals(A,B,C):-  flatten_sets([A,B],C),!. 

same_rhs_operation(A,B):- is_list(A),is_list(B),!.
same_rhs_operation(A,B):- (\+ compound(A) ; \+ compound(B)),!, A=@=B.
same_rhs_operation(A,B):-
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(same_rhs_operation,AA,BB),!.




%good_for_rhs(iz(sid(_))).
%good_for_rhs(mass(_)).
%good_for_rhs(iz(cenGX(_))).
%good_for_rhs(iz(cenGY(_))).
%good_for_rhs(iz(sizeGX(_))).
%good_for_rhs(iz(sizeGY(_))).
/*good_for_rhs(vis2D(_, _)).
good_for_rhs(center2D(_, _)).
good_for_rhs(center2G(_, _)).
good_for_rhs(rot2D(_)).
good_for_rhs(iz(algo_sid(norm,_))).
good_for_rhs(grid_ops(norm,_)).
good_for_rhs(grid_rep(norm,_)).
*/
good_for_rhs(loc2D(_,_)).
good_for_rhs(pen(_)).

good_for_rhs(delete(_)).
good_for_rhs(edit(_)).
good_for_rhs(edit(_,_,_)).
good_for_rhs(create(_)).
good_for_rhs(rhs(_)).
good_for_rhs(obj(_)).

good_for_lhs(P):- \+ ok_notice(P),!,fail.
%good_for_lhs(P):- make_unifiable(P,U),P=@=U,!,fail.
good_for_lhs(cc(bg, _)).
good_for_lhs(cc(fg, _)).
good_for_lhs(cc(_, 0)).
good_for_lhs(cc(FG, _)):- is_real_color(FG).
good_for_lhs(cc(_, _)):- !, fail.
good_for_lhs(center2D(_, _)).
good_for_lhs(empty_area(_)).
good_for_lhs(global2G(_, _)).
%good_for_lhs(grid_ops(comp, _)).
good_for_lhs(grid_ops(norm, _)).
good_for_lhs(iz(algo_sid(comp, _))).
good_for_lhs(iz(algo_sid(norm, _))).
good_for_lhs(iz(cenGX(_))).
good_for_lhs(iz(cenGY(_))).
%good_for_lhs(iz(fg_or_bg(_))).
good_for_lhs(iz(filltype(_))).
good_for_lhs(iz(info(_))).
good_for_lhs(iz(sid(_))).
good_for_lhs(iz(sizeGX(_))).
good_for_lhs(iz(sizeGY(_))).
good_for_lhs(iz(stype(_))).
good_for_lhs(link(sees(_),_)):-!,fail.
good_for_lhs(link(NS, _)):- !, NS\=sees(_).
good_for_lhs(links_count(_, _)).
good_for_lhs(loc2D(_, _)).
good_for_lhs(mass(_)).
good_for_lhs(pen(_)).
good_for_lhs(rot2D(_)).
good_for_lhs(rotSize2D(grav, _, _)).
good_for_lhs(unique_colors(_)).
%good_for_lhs(unique_colors_count(_)).
good_for_lhs(vis2D(_, _)).
good_for_lhs(pg(_,_,_,_)).
good_for_lhs(\+ P):- !, good_for_lhs(P).


/*
pass2_rule_new(TestID,Ctx,RHSO,[iz(info(spawn(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,LHSO,RHSO,[],[],[]),flat_props(LHSO,PSame).
pass2_rule_new(TestID,Ctx,[delete(LHSO)],[iz(info(delete(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,LHSO,[],[],[],[]),flat_props(LHSO,PSame).
pass2_rule_new(TestID,Ctx,P,[iz(info(copy_edit(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,_LHSO,_RHSO,PSame,_L,R),member(P,R),good_for_rhs(P).
*/

/*
pass2_rule_R(TestID,Rule):-
  Rule = rule(RuleType,P,PSame),
  RuleType = edit_copy(Ctx,ReType,P), 
  pass2_rule_1(TestID,IO,P,PSame), 
  once((good_for_rhs(P),
  prop_type(P,ReType),io_to_cntx(IO,Ctx))).
*/


has_a_value(P):- make_unifiable_u(P,U),P\=@=U.

how_are_differnt(O1,O2,Set):-
  findall(Type=Same,prop_pairs(O1,O2,Type,Same,_P),List),
  vsr_set(List,Set).

prop_pairs(O1,O2,Type,Same,P):- 
  flat_props(O1,F1),flat_props(O2,F2),!,
  member(P2,F2),make_unifiable_u(P2,P1),
 (once((member(P1,F1),(other_val(P2,P1)->Same=different;Same=same)))-> min_unifier(P2,P1,P); (Same=adding,P=P2)),
 prop_type(P2,Type).
   
into_lhs(OID,Out):- atom(OID),!,indv_props_list(OID,In),into_lhs(In,Out),!.
into_lhs(In,Out):- \+ compound(In),!,Out=In.
into_lhs(R,P):- sub_compound(lhs(E),R),!, into_lhs(E,P).
into_lhs(rule(_RuleType,_SortKey,In),Out):- nonvar(In),!,into_lhs(In,Out),!.
into_lhs(obj(In),Out):- nonvar(In),!,into_lhs(In,Out),!.
into_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
into_lhs(ac_db(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
into_lhs(ac_listing(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
into_lhs(ac_rules(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
into_lhs(In,Out):- \+ is_list(In),!,Out=In.
into_lhs(In,Out):- flatten([In],InF),into_lhs1(InF,LHSF),flatten(LHSF,LHSV),variant_list_to_set(LHSV,Out),!.
into_lhs1(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- is_group(In),mapgroup(into_lhs1,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- remove_giz(In,Out),!.
into_lhs1(In,Out):- maplist(into_lhs,In,LHSF),flatten(LHSF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
into_lhs1(In,Out):- include(good_for_lhs,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
into_lhs1(Out,Out).

%m_unifiers(In,Out):- \+ is_list(In),Out=In.
   

m_unifiers(In,Out):- my_partition(assume_prop,In,Skip,DontSkip), Skip\==[],
  m_unifiers(DontSkip,Mid), append_sets([Mid,Skip],Out),!.

%m_unifiers(In,Out):- is_list(In), select(E,In,More),is_prop1(E),is_unbound_prop(E),make_unifiable_u(E,U),select(U,More,UMore), 
%  min_unifier(U,E,S),!,m_unifiers([S|UMore],Out),!.

m_unifiers(In,Out):- is_list(In), select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore), 
  min_unifier(U,E,S),!,m_unifiers([S|UMore],Out),!.
%m_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore),other_val(E,U),merge_props(U,E,S),!,m_unifiers([S|UMore],Out).
m_unifiers(IO,IO).
%o_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable(E,U),select(U,More,UMore),other_val(E,U),the_or_unifier(U,E,S),!,o_unifiers([S|UMore],Out).
o_unifiers(IO,IO). 
the_or_unifier(U,E,(U;E)).


merge_props(S1,S2,S):- my_partition(assume_prop,S1,SP1,SO1),my_partition(assume_prop,S2,SP2,SO2),
  the_min_unifier0(SO1,SO2,SO),append_vsets([SO,SP1,SP2],S).

the_min_unifier0(S1,S2,S):- the_min_unifier1(S1,S2,SA),
  m_unifiers(SA,SB),!,variant_list_to_set(SB,S).

the_min_unifier1(S1,S2,[E|S]):- 
   select(E1,S1,S1R),same_functor(E1,E2),select(E2,S2,S2R),

   %make_unifiable_u(E1,E2),
   other_val(E1,E2),%min_unifier(E1,E2,E),!,

   min_unifier(E1,E2,E),

   the_min_unifier1(S1R,S2R,S).
the_min_unifier1(S1,S2,S):- append(S1,S2,S),!.




/*
show_code_diff(Info,PA,[PB]):- !, show_code_diff(Info,PA,PB).
show_code_diff(Info,[PA],PB):- !, show_code_diff(Info,PA,PB).
show_code_diff(Info,[],_).
show_code_diff(Info,[P|A],PB):- !, show_code_diff(Info,P,PB),show_code_diff(Info,A,PB).
*/
show_code_diff(Info,In,Out):-
 must_det_ll((
  into_list(In,InL),into_list(Out,OutL),
  trans_rule(Info,InL,OutL,TransRule),
  pp_ilp(TransRule),
  show_cp_dff_rem_keep_add(TransRule))).

propset_getter(is_group).
propset_getter(is_object).
propset_getter(is_obj_props).
two_prop_sets(TransRule,E1,E2):-
 sub_term(E1,TransRule),propset_getter(P1),call(P1,E1),subst(TransRule,E1,gone,RuleRest),
 sub_term(E2, RuleRest),propset_getter(Q1),call(Q1,E2).

show_cp_dff_rem_keep_add([]):-!.
show_cp_dff_rem_keep_add(TransRule):-   %flat_props([B],PB), intersection(Same,PB,S,SS,_), append(S,SS,SSame),
  two_prop_sets(TransRule,E1,E2),  
  dash_chars,
  if_t(how_are_differnt(E1,E2,Set),pp_ilp(how_are_differnt=Set)),
  flat_props(E1,FP1),flat_props(E2,FP2),
  intersection(FP1,FP2,Same,InFlatP,OutPFlat),
  length(InFlatP,LenA), pp_ilp(removed(LenA)=InFlatP),
   length(Same,SL),      pp_ilp(sames(SL)=Same),
  length(OutPFlat,LenB),pp_ilp(added(LenB)=OutPFlat),
  dash_chars.


pp_ilp(Grp):-pp_ilp(1,Grp),!.

pp_ilp(D,T):-  T==[],!,prefix_spaces(D,write('[] ')),!.
pp_ilp(_,_):- format('~N'),nl,fail.
pp_ilp(D,T):-  is_ftVar(T),!,prefix_spaces(D,print(T)),!.
pp_ilp(D,X=Y):- is_list(Y),length(Y,L),
  must_det_ll((
   prefix_spaces(D, (print(X),write('('),write(L),write(') = '))),nl,
   prefix_spaces(D+2,pp_ilp(Y)))).
pp_ilp(D,X=Y):- 
  must_det_ll((
   prefix_spaces(D, (print(X),write(' = '))),nl,
   prefix_spaces(D+2,pp_ilp(Y)))).
pp_ilp(D,call(T)):- !, prefix_spaces(D,call(T)).
% pp_ilp(D,Grp):- is_mapping(Grp), prefix_spaces(D,print(Grp)),!.
pp_ilp(D,Grp):- is_mapping(Grp), !,
 must_det_ll((
  get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<grp  ~w >\n',[Info]))),
    print_io_terms(D+7,In,Out),
    %prefix_spaces(D+8,show_code_diff(Info,In,Out)),
  prefix_spaces(D,(write('</grp>\n'),dash_chars)))).

pp_ilp(D,Grp):- compound(Grp), 
  (In-Out = Grp), Info=lr,!,
 must_det_ll((
 % get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<grp-hyphen  ~w >\n',[Info]))),
    print_io_terms(D+7,In,Out),
    %prefix_spaces(D+8,show_code_diff(Info,In,Out)),
  prefix_spaces(D,(write('</grp-hyphen>\n'),dash_chars)))).


pp_ilp(D,apply(Rule,Obj)):- !, pp_ilp(D,grp(Rule,[],Obj)).


pp_ilp(D,A+B):-  !, prefix_spaces(D,(pp_ilp(A),nl,pp_ilp(B))).
pp_ilp(D,Grid):- is_grid(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_object(Grid),!,prefix_spaces(D,print_grid([Grid])),!,nl.

pp_ilp(D,(H:-Conj)):- 
  prefix_spaces(D,(print((H)),
     prefix_spaces(D+15,(portray_clause((:-Conj)))))),!.

pp_ilp(D,ac_rules(_TestID,IO,P,PSame)):- !,
 must_det_ll((
  %once((nonvar(IO),io_to_cntx(IO,CTX));IO=CTX),
  once(list_to_conjuncts(PSame,Conj);PSame=Conj),
  pp_ilp(D,((IO:P):- Conj)))).

  

%pp_ilp(D,(H:-Conj)):- prefix_spaces(D,pp_ilp(H:-Conj)),!.


%pp_ilp(D,T):- true,!, prefix_spaces(D,print(T)),!.

%pp_ilp(D,Grid):- is_group(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_group(Grid),!, 
  must_det_ll((length(Grid,Len),
   prefix_spaces(D,(format('<group ~w>\n',[len=Len]))),
   prefix_spaces(D,mapgroup(pp_ilp(D+7),Grid)),!,nl,
   prefix_spaces(D,(format('</group>\n',[]))))),!.


pp_ilp(D,Grid):- is_obj_props(Grid),!,sort(Grid,R),reverse(R,S), prefix_spaces(D,pp(S)).
%pp_ilp(D,List):- is_list(List), \+ is_grid(List),write('['),maplist(pp_ilp(D+3),List),write(']').
pp_ilp(D,List):- is_list(List), !,
 must_det_ll((
  prefix_spaces(D,write('[')),
  maplist(pp_ilp(D+3),List),
  prefix_spaces(D,write(']')))).


%pp_ilp(D,T):- into_solid_grid_strings(T,G),!, prefix_spaces(D,print(G)),!.
pp_ilp(D,T):- prefix_spaces(D,pp(T)),!.


is_grid_or_group(Grid):- is_grid(Grid),!.
is_grid_or_group(Grid):- is_group(Grid),!.

print_io_terms(D,In,Out):-
  once(into_solid_grid_strings_1(In,ITerm)),
  once(into_solid_grid_strings_1(Out,OTerm)),
  once(ITerm\=@=In;Out\=@=OTerm),!, print_io_terms(D,ITerm,OTerm).
/*
print_io_terms(D,ITerm,OTerm):-  
    is_grid_or_group(ITerm),is_grid_or_group(OTerm),
    prefix_spaces(D,print_ss("",ITerm,OTerm)),!.

print_io_terms(D,loc2D(X,Y,IITerm),loc2D(OX,OY,OOTerm)):- 
    \+ is_mapping(IITerm), \+ is_mapping(OOTerm),
    into_obj(IITerm,ITerm),  into_obj(OOTerm,OTerm),
    prefix_spaces(D,print_ss("",ITerm,loc2D(X,Y),OTerm,loc2D(OX,OY))),!.
*/
print_io_terms(D,ITerm,OTerm):-
    prefix_spaces(D,pp_ilp(ITerm)),
    prefix_spaces(D+10,dash_chars),
    prefix_spaces(D,pp_ilp(OTerm)),!.

print_io_terms(D,ITerm,OTerm):- prefix_spaces(D,print_ss("",call(pp_ilp(ITerm)),call(pp_ilp(OTerm)))),!.

%prefix_spaces(D,G):- fail, DD is D, wots(Tabs,(write('\t'),print_spaces(DD),write('.\t'))), wots(SS,G),!, print_prepended(Tabs,SS).
prefix_spaces(D,G):- DD is D, wots(Tabs,(write('\t'),print_spaces(DD),write('\t'))),prepend_each_line(Tabs,G).

into_solid_grid_strings_1(T,WithGrids):-
  sub_term(Obj,T),is_object(Obj),global_grid(Obj,Grid),into_solid_grid(Grid,Solid),
  subst001(T,Obj,Solid,MidTerm),MidTerm\=@=T,!,into_solid_grid_strings_1(MidTerm,WithGrids).
    
into_solid_grid_strings_1(X,Y):- into_solid_grid_strings(X,Y),!.

/*into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),Obj\=@=T,is_mapping(Obj),
  into_solid_grid_strings(Obj,Grid),!,
  subst001(T,Obj,Grid,MidTerm),



  into_solid_grid_strings(MidTerm,WithGrids).*/
prin_to_string(T,Text):- term_contains_ansi(T),Text=T,!.
prin_to_string(T,Text):- wots(Text,print(T)). 

into_solid_grid_strings(T,Text):- is_ftVar(T),Text=T,!.
into_solid_grid_strings(A,Y):-atom(A),into_obj(A,Y),!. %,into_solid_grid_strings(X,Y).
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
%  \+ arc_cache:map_group(TestID,ExampleNum,IO_,LeftRight),

need_positional_context(H,V):- (H=<3;V=<3),!.
need_positional_context(H,V):- (H=<12,V=<12),!.
need_positional_context(_H,_V).


into_solid_grid_str([Obj,Obj2],SS):- fail, is_object(Obj),is_object(Obj2),
 into_solid_grid_str(Obj,Grid1),
 into_solid_grid_str(Obj2,Grid2),
 wots(SS,print_ss(Grid1,Grid2)),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),global_grid(Obj,GG),!,into_solid_grid_str(GG,SS).
into_solid_grid_str(Obj,SS):- is_object(Obj),global_grid(Obj,SS),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),
 vis2D(Obj,H,V), vis2D(Obj,H,V),has_prop(giz(g(IO_)),Obj),
 (need_positional_context(H,V)->global_grid(Obj,GG);=(Obj,GG)),
  as_grid_string(GG,Grid), =((loc2D(IO_,X-Y,Grid)),SS),!.

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
    retract(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,Right,Left,A,B,C))),
 forall(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Info), 
   retract(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Info))),
 !.






  
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
pairs_of_any(LHS,RHS,PairsR):-
  pairs_of_any(LHS,RHS,[],PairsR).

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
append_LR(Mappings,RestLR):- 
  flatten([Mappings],RestLR),!.

:- discontiguous calc_o_d_recursively/10. 

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  maybe_remove_bg(RHSObjs,RHSObjs1), \=@=(RHSObjs,RHSObjs1),!,
  must_det_ll((calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs1,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,_TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):-
  LHSObjs==[], RHSObjs == [], !, 
  Info = info(Step,IsSwapped,Ctx,leftover,TestID,ExampleNum,_),
  append_LR([call(assert_test_property(TestID,ExampleNum,deps,perfect_balance(Info)))],Prev,RestLR).

calc_o_d_recursively(TestID,ExampleNum,_TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   Info = info(Step,IsSwapped,Ctx,leftover,TestID,ExampleNum,_),
   RHSObjs==[], !, 
    must_det_ll((maplist(into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,Info),
     LHSObjs,Mappings),append_LR(Prev,[call(assert_test_property(TestID,ExampleNum,deps,ignore_rest(Info))),Mappings],RestLR))).


calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,[Right],RestLR):- 
  LHSObjs == [],
  into_list(Prev,PrevObjs), PrevObjs\==[],
  my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
  once((PrevRHS = [A,B|C] ; PrevLHS = [A,B|C])),
  sort_by_jaccard(Right,[A,B|C],Stuff),!,
  reverse(Stuff,[AA,BB|_Rest]),
  make_pairs(TestID,ExampleNum,assumed,IsSwapped,Step,Ctx,[],[BB,AA],Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,NewPrev,[],[],RestLR),!.


is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,0),!.
is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,1),!.
is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,2),!.

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], 
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
    append_LR(PrevRHS,PrevLHS,NewLHS),
    is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest),
    incr_step(Step,IncrStep),
    make_pairs(TestID,ExampleNum,is_adjacent_same_color,IsSwapped,Step,Ctx,Prev,R1,R2,Pairs),
    %once((PrevRHS = [A,B|C] ; PrevLHS = [A,B|C])), %append_LR(PrevRHS,PrevLHS,NewLHS), %NewLHS=PrevLHS,    
    !, must_det_ll((calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,[Pairs|Prev],LHSObjs,RHSRest,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], !, must_det_ll((
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,_PrevRHS),
    %once((PrevRHS = [A,B|C] ; PrevLHS = [A,B|C])), %append_LR(PrevRHS,PrevLHS,NewLHS), %NewLHS=PrevLHS,
    incr_step(Step,IncrStep),
    calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,Prev,PrevLHS,RHSObjs,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjsNil,RHSObjs,RestLR):- 
   LHSObjsNil==[], !, 
    incr_cntx(Ctx,IncrCtx),
    incr_step(Step,IncrStep), %incr_step(Step,IncrStep),
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
    member(Type=LHSObjs,[perfect_combo=PrevLHS,perfect_combo=PrevRHS]),
      select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
      must_det_ll((
      remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
      remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
      make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,IncrCtx,Prev,Left,Right,Pairs),
      append_LR(Prev,Pairs,NewPrev),
      
      calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,IncrStep,IncrCtx,NewPrev,LHSRest,RHSRest,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,_Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], !, must_det_ll((
    incr_cntx(Ctx,IncrCtx),
    %incr_step(Step,IncrStep),
    into_list(Prev,NewLHS),
    calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,10,IncrCtx,Prev,NewLHS,RHSObjs,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,[Right],RestLR):- LHSObjs=[_,_|_],
  sort_by_jaccard(Right,LHSObjs,[A,B|C]),
  make_pairs(TestID,ExampleNum,assumed,IsSwapped,Step,Ctx,[],[B,A],Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,NewPrev,C,[],RestLR),!.


calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,[Pairs|RestLR]):-
 select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
 must_det_ll((
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  incr_step(Step,IncrStep),
  calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,NewPrev,LHSRest,RHSRest,RestLR))).

calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,[Pairs|RestLR]):-
 must_det_ll((
  select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  incr_step(Step,IncrStep),
  calc_o_d_recursively(TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,NewPrev,LHSRest,RHSRest,RestLR))).



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
  Info = info([step(Step),is_swapped_lr(IsSwapped),ctx(Ctx),why(TypeO),testid(TestID),example(ExampleNum)]),
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

%solve_obj(_VM,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,IO_,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IO__Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,IO_,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  solve_obj(VM,TestID,ExampleNum,IO__Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsO).


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
/*

p_to_utbs(TestID,Ctx,P,UTBLists):-
 findall(UPB2,
  gather_set(UPB2,(map_pairs_info_io(TestID,_ExampleNum,Ctx,_Step,_TypeO,_A,_B,_USame,_UPA2,UPB2),member(P,UPB2))),UTBLists).
*/
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

%  is_post_objs(TestID,IO_,PostObjs),include(has_prop(P),PostObjs,PostObjsO).


make_common(NewOut1,LHS1,[E|NewOut],[E|NewLHS]):-
   select(O,NewOut1,NewOut2),
   make_unifiable_u(O,I),
   select(I,LHS1,LHS2),
   I=@=O, make_unifiable_u(I,E),!,
   make_common(NewOut2,LHS2,NewOut,NewLHS).
make_common(I,O,I,O).

% old code
diff_l_r_old(InL,OutL,Same,InFlatP,OutPFlat):-
 must_det_ll((
  (( \+ length(InL,1), OutL=[Out] ) -> sort_by_jaccard(Out,InL,[UseL|_]);UseL=InL),
  flat_props([UseL],PA), flat_props([OutL],PB),
  noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat))),!.



% no operation
diff_l_r([],[],[],[],[]):- !.

diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- \+ is_list(InL),!,diff_l_r([InL],OutL,Same,InFlatP,OutPFlat).
diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- \+ is_list(OutL),!,diff_l_r(InL,[OutL],Same,InFlatP,OutPFlat).

diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- fail,
 must_det_ll((
  (( \+ length(InL,1), OutL=[Out] ) -> sort_by_jaccard(Out,InL,[UseL|_]);UseL=InL),
  flat_props([UseL],PA), flat_props([OutL],PB),
  noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat))),!.

% -copy/transform  1-to-1
diff_l_r([InL],[OutL],PA,[],OutFlat):- OutL\==[],!,
  must_det_ll((flat_props([InL],PA), flat_props([OutL],PB),
  intersection(PA,PB,_Shared,_L,OutFlat))).

% -copy/transform
diff_l_r([InL],OutL,PA1,[],OutFlat):- OutL\==[],!,
  must_det_ll((flat_props([InL],PA), flat_props([OutL],PB),
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,_Same,_InFlatP,OutFlat))).

% create new
diff_l_r([],OutL,[],[],OutL):- OutL\==[],!.

% -delete some
diff_l_r(InL,[],Precond,[],[]):- !,
   flat_props([InL],InFlatP),
   remove_o_giz(InFlatP,Precond).

% -mutiple preconds
diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- OutL\==[],InL\==[],!,
  %pp_ilp(out=OutL), pp_ilp(in=InL),
  must_det_ll((
   sort_by_jaccard(OutL,InL,SharedInL),
   [UseL|Rest] = SharedInL,
   diff_l_r([UseL],OutL,Same1,InFlatP1,OutPFlat1),
   diff_l_r(Rest,OutL,SameR,InFlatPR,OutPFlatR),
   append_vsets([Same1,SameR],Same),
   append_vsets([InFlatP1,InFlatPR],InFlatP),
   append_vsets([OutPFlat1,OutPFlatR],OutPFlat))).

append_vsets(I,O):- flatten([I],M),variant_list_to_set(M,O),!.

noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  %=(PA,PA1),=(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,Same,InFlatP,OutPFlat),!.
noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  intersection(PA1,PB1,Same,InFlatP,OutPFlat),!.

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
%hide_propchange2(occurs_in_links(PA,_),occurs_in_links(PA,_)).
%hide_propchange2(links_count(PA,_),links_count(PA,_)).
hide_propchange2(giz(example_num(ExampleNum)),giz(example_num(ExampleNum))).
hide_propchange2(giz(gid(_)),giz(gid(_))).
hide_propchange2(giz(InL),giz(OutL)):- make_unifiable_u(InL,OutL).
hide_propchange2(oid(_),oid(_)).
hide_propchange2((i_o(_)),(i_o(_))).
hide_propchange2(In,Out):- once((sub_term(E,In),is_grid(E),number_fg_colors(E,G),subst001(In,E,G,Mid))),In\=@=Mid,!,hide_propchange(Mid,Out).
hide_propchange2(grid_rep(InL,G),grid_rep(InL,G)).
hide_propchange2(iz(X),iz(Y)):-!,hide_propchange2((X),(Y)).
hide_propchange2(IO_,IO_).

hide_propchange1(iz(symmetry_type(_,False))):- False == false.
hide_propchange1(iz(symmetry_type(_,False))):- False == true.
%hide_propchange1(pg(_,_,_,_)).
hide_propchange1(link(sees(_),_)).
hide_propchange1(pg(_,_,rankLS,_)).
hide_propchange1(iz(P)):-!,hide_propchange1(P).
%hide_propchange1(P):- \+ ok_notice(P),!.
hide_propchange1(P):- dont_notice(P),!.
%hide_propchange1(P):- make_unifiable_u(P,U),!,P=@=U,!.

hide_propchange(PA,PB):- hide_propchange2(PA,PA1),PA\=@=PA1,!,hide_propchange(PA1,PB).
hide_propchange(PA,PA).

remove_o_giz(OID,Out):- atom(OID),!,indv_props_list(OID,In),remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- \+ compound(In),!,Out=In.
remove_o_giz(In,Out):- is_group(In),mapgroup(remove_o_giz,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(obj(In),Out):- nonvar(In),!,remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
%remove_o_giz(In,Out):- remove_giz(In,Out),!.
remove_o_giz(Out,Out).






%is_accompany_changed_verified(TestID,IO,P,PSame):- is_accompany_changed_computed(TestID,IO,P,PSame), PSame\==[].

%is_accompany_changed_computed(TestID,IO,P,PSame):-
%   ac_rules(TestID,IO,P,PSame) *->true ; prop_can(TestID,IO,P,PSame). 
   
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

/*
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
*/

changing_props(TestID,X1,X2):- 
 ensure_test(TestID),
 findall(X1-InOut,props_change(TestID,InOut,X1),X1L),
 variant_list_to_set(X1L,X1S),
 member(X1-IO,X1S),
 member(X2-IO,X1S),
% X1@>X2,
 other_val(X1,X2). 



print_scene_change_rules(TestID):- ensure_test(TestID),
  print_scene_change_rules(print_scene_change_rules,TestID).

get_scene_change_rules(TestID,P4db,Rules):-
 ensure_test(TestID),
  findall_vset_R(ac_rules(TestID,IO,P,PSame),
    call(P4db,TestID,IO,P,PSame),Rules).

print_scene_change_rules(Why,TestID):- 
   print_scene_change_rules(Why,ac_unit_db,TestID).

print_scene_change_rules(Why,P4db,TestID):-
 ensure_test(TestID),
  must_det_ll((
   get_scene_change_rules(TestID,P4db,Rules),
   nb_setval(last_P3,Rules),
   banner_lines(cyan,4),
   %trans_rules_combined(TestID,_Ctx,CombinedR),reverse(CombinedR,Combined), pp_ilp(merged(Why)=Combined),
   /*
   trans_rules_current(TestID,Ctx,Rules),
   must_det_ll(( \+ (member(R,[1|Rules]), is_list(R)))),
   combine_trans_rules(Rules, Combined),!,
   must_det_ll(( \+ (member(R,[2|Combined]), is_list(R)))).
   */
   banner_lines(cyan,2),
   pp_ilp(rules(Why,P4db)=Rules),
   banner_lines(cyan,4))).

print_scene_change_rules_if_differnt(Why,P4db,TestID):-
  nb_current(last_P3,Prev),
  get_scene_change_rules(TestID,P4db,Rules),  
 ignore((
  Prev \=@= Rules,
   nb_setval(last_P3,Rules),
   banner_lines(cyan,4),
   pp_ilp(updated(Why,P4db)=Rules),
   banner_lines(cyan,4))).



has_propcounts(TestID):- 
 forall(current_example_nums(TestID,ExampleNum),
  ( \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(in,IO)),
    \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(out,IO)))).

ensure_propcounts(TestID):- ensure_test(TestID),ensure_propcounts1(TestID).
ensure_propcounts1(TestID):- has_propcounts(TestID),!.
ensure_propcounts1(TestID):- ensure_individuals(TestID),!.
ensure_propcounts1(TestID):- calc_propcounts(TestID),has_propcounts(TestID),!.

ensure_propcounts1(TestID):- 
  once((with_pair_mode(whole_test,
    with_luser(menu_key,'o',once(ndividuator(TestID)))))),has_propcounts(TestID),!.
ensure_propcounts1(TestID):- show_prop_counts(TestID), has_propcounts(TestID),!.
ensure_propcounts1(_).

props_change(TestID,IO,P):- map_pairs_info(TestID,IO,P,_Step),good_for_rhs(P).
props_change2(TestID,IO,P):-
% -  ensure_propcounts(TestID),
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
 (\+ ac_unit(TestID,_,_,_) -> compute_scene_change(TestID) ; true).



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
  compute_scene_change_pass4(TestID)))),!.


compute_scene_change_pass1(TestID):- 
  show_object_dependancy(TestID),!.
  %learn_object_dependancy(TestID).


compute_scene_change_pass2(TestID):- 
  retractall(ac_unit(TestID,_,_,_)),
    forall(pass2_rule(TestID,Ctx,P,PSame),
      assert_accompany_changed_db(TestID,Ctx,P,PSame)),
  print_scene_change_rules(compute_scene_change_pass2,TestID).

compute_scene_change_pass3(TestID):-
 must_det_ll((
  set_of_changes(TestID,compute_scene_change_pass3a(TestID)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes1)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes2)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes3)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes4)),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes5)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes6)),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)))),!.


compute_scene_change_pass3a(TestID,IO_-P):- 
   findall_vset_R(PSame,ac_unit(TestID,IO_,P,PSame),List),
   m_unifiers(List,ListR),
   update_accompany_changed_db(TestID,IO_,P,ListR).
compute_scene_change_pass3a(_,_).

compute_scene_change_pass3b(TestID,P4,IO_-P):-
  findall_vset_R(PSame,ac_unit(TestID,IO_,P,PSame),SameS1),
  my_partition(assume_prop,SameS1,Skip,SameS),
  call(P4,TestID,IO_,P,SameS,KeptS), KeptS\==[],!,
  if_t(SameS\=@=KeptS,
     (append(KeptS,Skip,Kept),
      update_accompany_changed_db(TestID,IO_,P,Kept))).
compute_scene_change_pass3b(_,_,_). 

compute_scene_change_pass3c(_,_):-!.
compute_scene_change_pass3c(TestID,IO_-P):-
  ac_unit(TestID,IO_,P,PSame1),
  my_partition(assume_prop,PSame1,Skip,PSame),
  findall(DSame,
     (ac_unit_db(TestID,IO_,DP,DSame), 
      same_rhs_property(DP,P),at_least_one_overlap(DSame,PSame)),
   SL),  SL = [_,_|_],
  common_members(SL,Commons),
  forall((ac_unit_db(TestID,IO_,DP,DSame),same_rhs_property(DP,P)),
      (intersection(DSame,Commons,_,Kept,_),
        ignore((Kept\==[],append(Kept,Skip,Save),update_accompany_changed_db(TestID,IO_,P,Save))))),

  print_scene_change_rules_if_differnt(compute_scene_change_pass3c,ac_unit,TestID),
  !.
compute_scene_change_pass3c(_,_).


compute_scene_change_pass4(TestID):-
   nop(compute_scene_change_pass3(TestID)),!.

set_of_ps(TestID,Ps):-
  ((findall_vset_R(Ctx-P1,
    ((ac_unit(TestID,IO_,P,_)
     ;ensure_props_change(TestID,IO_,P)
     ;pass2_rule(TestID,IO_,P,_)),
    io_to_cntx(IO_,Ctx),into_rhs(P,P1)), Ps))).

set_of_changes(TestID,P1):-
  set_of_ps(TestID,Ps),
  %findall_vset_R(IO_-P,(ac_rules(TestID,IO_,P,_)), Ps),
  maplist(P1,Ps),
  print_scene_change_rules(P1,ac_unit_db,TestID).

into_rhs(P,P):- \+ compound(P),!.
into_rhs(ac_unit(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
into_rhs(ac_db(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
into_rhs(ac_rules(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
into_rhs(ac_listing(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
into_rhs(P,E):- sub_compound(rhs(E),P),!. %into_rhs(rhs(R),P):- !, into_rhs(R,P).
%into_rhs(edit(R),P):- !, into_rhs(R,P).
%into_rhs(create(R),P):- !, into_rhs(R,P).
%into_rhs(delete(R),P):- !, into_rhs(R,P).
into_rhs([R],P):- !, into_rhs(R,P).
into_rhs(P,P).

update_accompany_changed_db(TestID,IO_,P,Kept):- Kept\==[],
 forall(io_to_cntx(IO_,Ctx), forall(retract(ac_unit(TestID,Ctx,P,_)),true)),
 assert_accompany_changed_db(TestID,IO_,P,Kept).
   
assert_accompany_changed_db(_TestID,_IO_,_P,Kept):- Kept==[],!.
assert_accompany_changed_db(TestID,IO_,P,Kept):- 
  io_to_cntx(IO_,Ctx),  
   assert_ilp_b(ac_unit(TestID,Ctx,P,Kept)).

%assert_ilp_b(Term):- \+ clause_asserted(Term),!, pp_ilp(assert_ilp_b=Term), asserta_new(Term).
assert_ilp_b(Term):- asserta_new(Term).
%assert_ilp_b(Term):- pp_ilp(assert_ilp_b=Term),!, assert_if_new(Term).

at_least_one_overlap(DSame,PSame):-
  member(DS,DSame),member(S,PSame),
  about_same_property(DS,S),!.

about_same_property(DS,S):- \+ \+ (same_rhs_property(DS,S);( \+ DS\=S )).
same_rhs_property(DS,S):- \+ \+ (DS=@=S;other_val(S,DS)).


% Retain Overlap
correct_antes1(TestID,IO_,P,PSame,SL):- 
  %rev_in_out_atoms(OI,IO_),
  findall(S,
   (member(S,PSame),
     \+ \+ ((
       forall((ac_rules(TestID,IO_,DP,DSame),at_least_one_overlap(DSame,PSame)),
          ((P==DP)-> true; (member(DS,DSame),  
             \+ negated_s_lit(S,_), other_val(S,DS))))))),
   SL), SL\==[],!.
correct_antes1(_TestID,_IO_,_P,PSame,PSame).

is_unbound_prop(S):- make_unifiable(S,DS), S=@=DS,!.

% Make sure each arguement is transformed corretly
correct_antes2(_TestID,_IO_,_P,PSame,Kept):-  maplist(ensure_xformed,PSame,Kept),!.
ensure_xformed(pg(_,A,B,C),pg(_,A,B,C)):-!.
ensure_xformed(A,A).

% Remove Redundant Overlap
correct_antes3(_TestID,_IO_,_P,PSame,SL):- 
  findall(S, ( member(S,PSame), \+ is_unbound_prop(S)), SL), SL\==[],!.
correct_antes3(_TestID,_IO_,_P,PSame,PSame).

% Remove Redundant Overlap
correct_antes4(TestID,IO_,P,PSame,SL):- 
  findall(S,
   ( member(S,PSame), 
     (negated_s_lit(S,_)->true;
      \+ ((  
       forall((ac_rules(TestID,IO_,DP,DSame),
              same_rhs_property(P,DP)),          
            (member(DS,DSame), DS=@=S)))))),
   SL), 

  SL\==[],!.
correct_antes4(_TestID,_IO_,_P,PSame,PSame).

% Add Negations
correct_antes5(TestID,IO_,P,PSame,Kept):- correct_antes_neg(TestID,IO_,P,PSame,Kept),!.
correct_antes5(_TestID,_IO_,_P,PSame,Kept):- vsr_set(PSame,Kept),!.
correct_antes5(_TestID,_IO_,_P,PSame,PSame).
correct_antes_neg(TestID,IO_,P,PSame,Kept):-
  findall( ( \+ DS),
   ((member(S,PSame), \+ negated_s_lit(S,_), is_unbound_prop(S), make_unifiable(S,DS),
     ac_rules(TestID,IO_,DP,DSame),      
     other_val(P,DP), %at_least_one_overlap(DSame,PSame),
     member(DS,DSame), \+ negated_s_lit(DS,_), \+ is_unbound_prop(DS), 
       \+ member(\+ DS,PSame))), SL), SL\==[],
  append(PSame,SL,Kept),Kept\==[], !.
correct_antes_neg(_TestID,_IO_,_P,PSame,PSame).


% DISABLED not really a loops
correct_antes6(_TestID,_IO_,P,PSame,Kept):- fail,
  findall(S, (member(S,PSame), \+ same_rhs_property(P,S)), Kept), Kept\==[],!.
correct_antes6(_TestID,_IO_,_P,PSame,PSame).


negated_s_lit(N,P):- compound(N), N = ( \+ P ). 


/*
correct_antes5(TestID,IO_,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_computed(TestID,IO_,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   pred_intersection(other_val,PSame,USame,Kept,_,_,_),Kept\==[].
correct_antes5(_TestID,_IO_,_P,PSame,PSame).


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
