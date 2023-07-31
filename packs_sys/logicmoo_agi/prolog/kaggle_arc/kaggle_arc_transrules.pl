/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

  
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

/*
correct_antes5(TestID,IO_,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_computed(TestID,IO_,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   pred_intersection(other_val, PSame, USame, Kept, _, _, _), warn_and_fail_if_empty(Kept).
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
  
/*
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
  findall(lho(CI, InPSS, HowIn, InC),
    (get_each_ndividuator(Dir, HowIn),
    obj_group_io_5(TestID, ExampleNum, Dir, HowIn, InC),
    length(InC, CI), CI=<50, objs_to_spoints(InC, InPSS)), LHOIn),
   sort(LHOIn, LHOInS), !.
best_obj_group_pair(TestID, ExampleNum, HowIO, InC, OutC):-
  OGP = best_ogp(TestID, ExampleNum, HowIO, InC, OutC),
  (arc_cache:trans_rule_db(TestID, ExampleNum, best_obj_group_pairs, OGPL)*-> true ;
   (time(findall(OGP, best_obj_group_pair1(TestID, ExampleNum, HowIO, InC, OutC), OGPL)),
     assert_test_property(TestID, ExampleNum, best_obj_group_pairs, OGPL))),

  member(OGP, OGPL).
*/
/*
calc_best_individuals(TestID):-
 current_example_scope(TestID, ExampleNum),
 forall(kaggle_arc(TestID, ExampleNum, _, _),
  (grid_indv_versions(TestID, ExampleNum, in, _LHOInS),
   grid_indv_versions(TestID, ExampleNum, out, _LHOOutS))),
 guess_io_opts(TestID, in, InOpts),
 guess_io_opts(TestID, out, OutOpts),
 print(pre_best=in_out(InOpts, OutOpts)), !.

guess_io_opts(TestID, IO, HowIn):-
 current_example_scope(TestID, ExampleNum),
 findall(hi(HowIn, s(Sizes-S)),
  (kaggle_arc(TestID, ExampleNum, _, _),
   grid_indv_versions(TestID, ExampleNum, IO, LHOInS),
   member(lho(S, InPSS, HowIn, _), LHOInS),

   maplist(length, InPSS, Sizes),
   Sizes \= [_, 1|_], Sizes\= [c_] ),
  SizeList),
 maplist(count_peers(SizeList), SizeList, SLPeers),
 reverse_sort(SLPeers, Sorted),
 pp_ilp(s(IO)=Sorted),
 Sorted=[hi(_, HowIn, s(_))|_].

reverse_sort(InputList, OutputList) :-
    sort(InputList, Sorted),
    reverse(Sorted, OutputList).

count_peers(CP, hi(HowIn, s(Sizes)), hi(N, HowIn, s(Sizes))):- findall_count(_, sub_var(s(Sizes), CP), N).
*/


end_of_file.

rhs_ground(G):- ground(G),!.
rhs_ground(G):- nop(writeln(G)),!.

ac_rules(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
ac_rules(TestID,Ctx,P,PSame):- 
  ac_unit(TestID,Ctx,P,Same), \+ never_use_horn_rhs(P),
  include(not_debug_info,Same,PSame), 
  %Same=PSame,
  PSame\==[].

not_debug_info(P):- \+ is_debug_info(P),!.

remove_debug_info(List,NoDebug):- \+ compound(List),!,NoDebug=List.
remove_debug_info(List,NoDebug):- is_list(List), is_obj_props(List),!,include(not_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- is_list(List), !, maplist(remove_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- compound_name_arguments(List,F,AA),
  maplist(remove_debug_info,AA,CC),!, compound_name_arguments(NoDebug,F,CC).

ac_unit(TestID,Ctx,P,Same):- ac_listing(TestID,Ctx,P,Same).

ac_listing(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
%ac_listing(TestID,Ctx,P->ac_db_unit,PSame):- ac_db_unit(TestID,Ctx,P,PSame).
ac_listing(TestID,Ctx,P,PSame):- (ac_db_unit(TestID,Ctx,P,PSame)*->true;pass2_rule(TestID,Ctx,P,PSame)), \+ never_use_horn_rhs(P).
%ac_listing(TestID,Ctx,P,[iz(info(prop_can))|PSame]):- prop_can(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[pass2|PSame]):- pass2_rule(TestID,Ctx,P,PSame), \+ ac_rules(TestID,Ctx,P,PSame).

/*
ac_listing(TestID,rules,P->Ctx->current,LHS):- 
  member(Ctx,[in_out,in_out_out,s(_)]),
  synth_program_from_one_example(TestID,Ctx,R),
  rule_to_pcp(R,P,LHS).
ac_listing(TestID,rules,P->Ctx->combined,LHS):- fail,
  member(Ctx,[in_out,in_out_out,s(_)]),
  trans_rules_combined_members(TestID,Ctx,R),
  rule_to_pcp(R,P,LHS).
*/
show_time_of_failure(_TestID):- !.
show_time_of_failure(TestID):- 
    print_scene_change_rules3(show_time_of_failure,
       ac_listing,TestID).

rule_to_pcp5(TestID,R,Ctx,P,LHS):- is_list(R),!,
 member(E,R),rule_to_pcp(TestID,E,Ctx,P,LHS).
rule_to_pcp5(_TestID,R,Ctx,P0,LHS):- R =..[ac_unit|Rest],append(_,[Ctx,P0,LHS],Rest),!.
rule_to_pcp5(_TestID,R,Ctx,P0,LHS):- 
  must_det_ll((
  find_rhs(R,P),
  find_lhs(R,Conds0),listify(Conds0,Conds),
  subst001(R,P,p,RR), subst001(RR,Conds,conds,RRR),
  append(Conds,[iz(info(RRR))],LHS),
  ignore((sub_compound(ctx(Ctx),R))))),!,P0=P.

%pcp_to_rule(TestID,Ctx,P,LHS,rule(TestID,Ctx,P,LHS)).


%ac_rules(TestID,P,PSame):- ac_rules(TestID,_,P,PSame).

%pass2_rule(TestID,Ctx,P,PSame):- pass2_rule_old(TestID,Ctx,P,PSame).
%pass2_rule(TestID,Ctx,P,PSame):- pass2_rule_new(TestID,Ctx,P,PSame).

pass2_rule(TestID,Ctx,RHS,LHS):-   
  pass2_rule1(TestID,Ctx,RHS,LHS)*->true;
  pass2_rule2(TestID,Ctx,RHS,LHS)*->true;
  fail.
/*
pass2_rule(TestID,Ctx,RHS,LHS):-
  findall_vset(Ctx-RHS,(pass2_rule1(TestID,Ctx,RHS,LHS);pass2_rule2(TestID,Ctx,RHS,LHS)),List),
  member(Ctx-RHS,List),
  (pass2_rule1(TestID,Ctx,RHS,LHS)*->true;pass2_rule2(TestID,Ctx,RHS,LHS)).
*/

pass2_rule1(TestID,Ctx,RHS,LHS):- fail,
 ensure_test(TestID),
  trans_rules_combined_members(TestID,Ctx,Rule),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Rule,Info),
  must_det_ll((
  rule_to_pcp5(TestID,Rule,Ctx,RHS,LHS))).

pass2_rule2(TestID,Ctx,RHS0,LHS0):- 
 ensure_test(TestID),
  synth_program_from_one_example(TestID,Ctx,Rule),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Rule,Info),
  must_det_ll((
  rule_to_pcp5(TestID,Rule,Ctx,RHS,LHS))),
  RHS0=RHS, LHS0=LHS.


%pass2_rule3(TestID,Ctx,edit(Type,different,P),[iz(info(propcan(true,Ctx)))|PSame]):- fail,ensure_test(TestID), ensure_props_change(TestID,Ctx,P).

/*pass2_rule(TestID,Ctx,RHS,[iz(info(Info))|LHS]):- 
 ensure_test(TestID),
  trans_rules_combined(TestID,_Ctx,Combined),
  member(Rule,Combined),
  arg(_,Rule,Info),
  must_det_ll((
  arg(_,Rule,rhs(RHS)),
  arg(_,Rule,lhs(LHS)))),
  rhs_ground(RHS).
*/




/*
pass2_rule(TestID,IO,P,OutputRule):-
  ensure_test(TestID),
  RuleType = edit_copy(IO,ReType,P), 
  SortKey = P,
  OutputRule = rule(RuleType,SortKey,SuperPreconds),
  KeyedRule = rule(RuleType,SortKey,Precond),
  Rule = rule(RuleType,P,LHS),
  findall(Rule,pass2_rule_R(TestID,Rule),Rules),
  maplist(arg(1),Rules,Keyz),vsr_set(Keyz,Keys),
  member(RuleType,Keys),
  findall(KeyedRule,
    (prop_type(P,ReType),findall(LHS,member(Rule,Rules),LHSList),flatten(LHSList,FFound),
      into_lhs(FFound,Precond)),KeyedRules),
  maplist(arg(3),KeyedRules,Preconds),into_lhs(Preconds,SuperPreconds),
  member(KeyedRule,KeyedRules),
  %include(has_a_value,SuperPreconds,UsedPrconds),
  true.
*/



%map_pairs_info(TestID,Ctx,P,Step):- !, map_pairs_info_io(TestID,_ExampleNum,Ctx,Step,_TypeO,_A,_B,_USame,_InFlatProps,UPB2),member(P,UPB2),nop(ok_deduce(P)).
ensure_props_change(TestID,IO,P):-  props_change(TestID,IO,P).


/*

:- abolish(good_conseq/4).
:- dynamic(good_conseq/4).

map_pairs_info2(_TestID,IO,P,_Step):- nonvar(P),nonvar(IO),!.
map_pairs_info2(TestID,IO,P,_Step):- nonvar(P),nonvar(IO),good_conseq(TestID,IO,P,YN),!,YN=yes.
  
  
map_pairs_info2(TestID,IO,P,Step):- 
 ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
 no_repeats_var(IOP),
 (map_pairs_info3(TestID,IO,P,Step)*->asserta(good_conseq(TestID,IO,P,yes));(asserta(good_conseq(TestID,IO,P,no)),fail)),
 IOP=IO+P.

map_pairs_info3(_TestID,_IO,_P,_Step).
*/
/*:- nonvar(P).
map_pairs_info3(TestID,IO,P,Step):- 
  %ensure_individuals(TestID),
  %ensure_propcounts(TestID),
  %learn_object_dependancy(TestID),
  
  (var(IO)->gather_set(Ctx,pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps));true),
  %IO_ = in, Ctx = in_out,
  %gather_set(P,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,PB2))).
  gather_set(P,(
      %nop(gather_set(Step,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,UPB2)))),
      map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,UPB2),member(P,UPB2),ok_deduce(P))),
  %p_to_utbs(TestID,Ctx,P,UTBLists),  
  %common_members(UTBLists,Members), 
  %member(P,Members),

  ignore(gather_set(Step,(pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps)))),
  io_to_cntx(IO,Ctx).

map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2):-
 pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2).

*/

% adds debugging to info/1
trans_rule(Info,In,Out,Rules):- sub_cmpd(info(InfoL),Info),!,trans_rule(InfoL,In,Out,Rules).
trans_rule(Info,In,Out,Clauses):- \+ is_list(In),!,trans_rule(Info,[In],Out,Clauses).
trans_rule(Info,In,Out,Clauses):- \+ is_list(Out),!,trans_rule(Info,In,[Out],Clauses).
trans_rule(Info,In,Out,Rules):- 
  ( \+ sub_cmpd(oin(_),Info); \+ sub_cmpd(oout(_),Info)),
  into_oids(In,OIDIns),into_oids(Out,OIDOuts),
  append_sets(Info,[oin(OIDIns),oout(OIDOuts)],InfM),!,
  trans_rule(InfM,In,Out,Rules).

trans_rule(Info,[],[],[ac_unit(_,Ctx,happy_ending(Type),Info)]):-
   ignore((sub_compound(ctx(Ctx),Info))),
   ignore((sub_compound(type(Type),Info))).


% delete
trans_rule(Info,[In],[],[Unit]):-
 into_lhs(In,Preconds),into_rhs(Info,InfoR), 
 Unit = ac_unit(_,_,delete_object(InfoR,In),[iz(info(Info))|Preconds]),!.


% create
trans_rule(Info,[],[Out],[Unit]):-
 into_lhs(Out,Preconds),into_rhs(Info,InfoR), 
 Unit = ac_unit(_,_,create_object(InfoR,Out),[iz(info(Info))|Preconds]),!.

% mutiple postconds
trans_rule(Info,In,[Out,Out2|OutL],TransRule):- is_object(Out),is_object(Out2),
  maplist(trans_rule(Info,In),[Out,Out2|OutL],TransRule), TransRule\==[],!.

% 2 preconds
%trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2),
%  TransRule = create_object2(Info,rhs(create_obj(Out)),lhs(into_new(In1,In2))),!.

% 2 preconds
trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2), % fail,
   noteable_propdiffs(In1, Out,_Same1,_DontCare1,New1), 
   noteable_propdiffs(In2,New1,_Same2,_DontCare2,New2),
   %remove_o_giz(Out,RHSO), 
   remove_o_giz(In1,Precond1), remove_o_giz(In2,Precond2),
   %sub_comInfo = info(Step,_IsSwapped,_Ctx,TypeO,_,_,_),
   sub_compound(step(Step),Info), sub_compound(why(Type),Info),
   Type \== assumed_in_in_out,
 % append_sets(Same1,Same2,Same), append_sets(DontCare1,DontCare2,DC), append_sets(New1,New2,New),
 % append_sets(Same,New,NewObj),
  %make_common(RHSO,LHS1,NewOut1,NewLHS1),
  %make_common(NewOut1,LHS2,NewOut,NewLHS2),
  TransRule = [create_object1(Info,rhs(creation_step1(Step,Type,New1)), lhs(Precond1)),
               create_object2(Info,rhs(creation_step2(Step,Type,New2)), lhs(Precond2))],!.

% mutiple preconds
trans_rule(Info,[In,In2|InL],OutL,TransRule):- is_object(In),is_object(In2),
  trans_rule(Info,[In2|InL],OutL,TransRuleM), TransRuleM\==[],
  sub_compound(lhs(Precond),TransRuleM),
  noteable_propdiffs(In,OutL,Same,_L,_R),
  append_vsets([Precond,Same],NewPrecond),
  subst(TransRuleM,lhs(Precond),lhs(NewPrecond),TransRule),!.

% just copy an object
trans_rule(Info,[In],[Out],Rules):- 
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  noteable_propdiffs(In,Out,Same,_L,R),R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ],!.

% just copy an object
trans_rule(Info,[In],[Out],Rules):- 
  how_are_different(In,Out,_TypeChanges,Diff),Diff==[],
  into_lhs(In,LHS),
  %%must_det_ll((sub_compound(step(Step),Info), sub_compound(why(TypeO),Info))),
  Rules = [ copy_if_match(Info,rhs(copy_step),lhs(LHS)) ],!.

% just copy an object
trans_rule(Info,In,Out,Rules):- 
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  noteable_propdiffs(In,Out,Same,L,R),L==[],R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ],!.


% copy/transform 
trans_rule(Info,In,Out,Rules):- 
  noteable_propdiffs(In,Out,_Same,_L,R),
  into_lhs(In,LHS),  
  findall(edit_copy(Info,rhs(edit(Type,Change,P)),lhs(LHS)),
    (member(P,R),prop_pairs(In,Out,Type,Change,P),
      Change\==same,
      P\==pen([cc(black,1)]),
      good_for_rhs(P)),Rules),Rules\==[],!.

trans_rule(Info,E1,E2,Rules):-
  noteable_propdiffs(E1,E2,NSame,NL,NR),
  dash_chars,
  pp_ilp(l2r(Info,E1,E2)),
  dash_chars,
  if_t(how_are_different(E1,E2,Set),pp_ilp(how_are_different=Set)),
  flat_props(E1,FP1),flat_props(E2,FP2),
  intersection(FP1,FP2,Same,InFlatP,OutPFlat),
  pp_ilp(info=Info),
  pp_ilp(nadded=NR),
  pp_ilp(added=OutPFlat),
  pp_ilp(nremoved=NL),
  pp_ilp(removed=InFlatP),
  pp_ilp(nsames=NSame),
  pp_ilp(sames=Same),
  itrace,
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  dash_chars,
  Rules = [ 
    create_object_step(Info,rhs(create3c(Step,TypeO,E2)),lhs(Same)) ],!.
    %copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].














:- consult(kaggle_arc_logicmoo).
