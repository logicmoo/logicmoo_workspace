/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


rhs_ground(G):- ground(G),!.
rhs_ground(G):- nop(writeln(G)),!.

not_assumed(P):- is_unbound_prop(P),!.
not_assumed(P):- \+ assume_prop(P).

ac_rules(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
ac_rules(TestID,Ctx,P,PSame):- 
  ac_db(TestID,Ctx,P,Same),
  %include(not_assumed,Same,PSame), 
  Same=PSame,
  PSame\==[].


ac_db(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
ac_db(TestID,Ctx,P,PSame):- (ac_unit_db(TestID,Ctx,P,PSame)*->true;pass2_rule(TestID,Ctx,P,PSame)).
ac_unit_db(TestID,Ctx,P,PSame):- ac_unit(TestID,Ctx,P,Same),include(not_assumed,Same,PSame), PSame\==[].

ac_listing(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
%ac_listing(TestID,Ctx,P->ac_unit_db,PSame):- ac_unit_db(TestID,Ctx,P,PSame).
ac_listing(TestID,Ctx,P,PSame):- ac_unit(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[iz(info(prop_can))|PSame]):- prop_can(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[pass2|PSame]):- pass2_rule(TestID,Ctx,P,PSame), \+ ac_rules(TestID,Ctx,P,PSame).


ac_info(TestID,rules,P->Ctx->current,LHS):- 
  member(Ctx,[in_out,in_out_out,s(_)]),
  trans_rules_current(TestID,Ctx,Rules),member(R,Rules),
  rule_to_pcp(R,P,LHS).
ac_info(TestID,rules,P->Ctx->combined,LHS):- fail,
  member(Ctx,[in_out,in_out_out,s(_)]),
  trans_rules_combined(TestID,Ctx,Rules),member(R,Rules),
  rule_to_pcp(R,P,LHS).

show_time_of_failure(TestID):- !.
show_time_of_failure(TestID):- 
    print_scene_change_rules(show_time_of_failure,
       ac_info,TestID).

rule_to_pcp(R,P,LHS):- 
  must_det_ll((
  into_rhs(R,P),into_lhs(R,Conds),
  subst(R,P,p,RR), subst(RR,Conds,conds,RRR),append(Conds,[iz(info(RRR))],LHS))).

%ac_rules(TestID,P,PSame):- ac_rules(TestID,_,P,PSame).

%pass2_rule(TestID,Ctx,P,PSame):- pass2_rule_old(TestID,Ctx,P,PSame).
%pass2_rule(TestID,Ctx,P,PSame):- pass2_rule_new(TestID,Ctx,P,PSame).

pass2_rule(TestID,Ctx,RHS,LHS):-  !, pass2_rule2(TestID,Ctx,RHS,LHS).
/*
pass2_rule(TestID,Ctx,RHS,LHS):-
  findall_vset(Ctx-RHS,(pass2_rule1(TestID,Ctx,RHS,LHS);pass2_rule2(TestID,Ctx,RHS,LHS)),List),
  member(Ctx-RHS,List),
  (pass2_rule1(TestID,Ctx,RHS,LHS)*->true;pass2_rule2(TestID,Ctx,RHS,LHS)).
*/

pass2_rule2(TestID,Ctx,RHS,LHS):- 
 ensure_test(TestID),
  trans_rules_current(TestID,Ctx,Combined),
  member(Rule,Combined),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Rule,Info),
  must_det_ll((
  rule_to_pcp(Rule,RHS,LHS))).

pass2_rule2(TestID,Ctx,RHS,LHS):- fail,
 ensure_test(TestID),
  trans_rules_combined(TestID,Ctx,Combined),
  member(Rule,Combined),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Rule,Info),
  must_det_ll((
  rule_to_pcp(Rule,RHS,LHS))).

pass2_rule2(TestID,Ctx,edit(Type,different,P),[iz(info(propcan(true,Ctx)))|PSame]):- fail,
  ensure_test(TestID), ensure_props_change(TestID,Ctx,P),
  prop_can(TestID,IO,P,PSame), once((io_to_cntx(IO,Ctx),prop_type(P,Type))).

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



trans_rules_current(TestID,Ctx,Rules):-
  findall_vset_R(TransRule,arc_cache:trans_rule_db(TestID,_ExampleNum1,Ctx,TransRule),Rules),!.

trans_rules_combined(TestID,Ctx,Combined):-
  trans_rules_current(TestID,Ctx,Rules), must_det_ll(( \+ (member(R,[1|Rules]), is_list(R)))),
  combine_trans_rules(Rules, Combined), must_det_ll(( \+ (member(R,[2|Combined]), is_list(R)))),
  !.

combine_trans_rules([R1|Rules], CombinedRules):- 
  into_rhs(R1,RHS1),
  same_functor(R1,R2),
  select(R2,Rules,RulesN), % my_assertion(r1, \+ is_list(R1)), my_assertion(r2, \+ is_list(R2)),
  into_rhs(R2,RHS2),
  %into_step(R2,RHS2),
  RHS1=@=RHS2,
  merge_vals(R1,R2,R),  % my_assertion(r, \+ is_list(R)),
  combine_trans_rules([R|RulesN], CombinedRules).
combine_trans_rules([R|Rules], [R|CombinedRules]):- 
  combine_trans_rules(Rules, CombinedRules).
combine_trans_rules([],[]).



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

map_pairs_info(TestID,IO,P,Step):-
  no_repeats_var(IOP),
  map_pairs_info2(TestID,IO,P,Step),
  ground(P),
  IOP=(IO+P).

%  ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
map_pairs_info2(TestID,IO,P,_Step):- props_change2(TestID,IO,P).
map_pairs_info2(TestID,IO,P,_Step):- 
 var(P), \+ \+ ac_unit(TestID,IO,_,_), %!,
  ac_unit(TestID,IO,P,_).
map_pairs_info2(TestID,Ctx,P,Step):- 
  arc_cache:prop_dep(TestID,_ExampleNum,Ctx,Info,_InL,_OutL,_USame,_InFlatProps,OutFlatProps),
  sub_compound(step(Step),Info),
  member(P,OutFlatProps).

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



% delete
trans_rule(Info,In,[],Rules):- listify(In,InL),
 findall(delete_object(Info,rhs(delete(In)),lhs(Preconds)),
   (member(In,InL),into_lhs(In,Preconds)),Rules), Rules\==[].

% mutiple postconds
trans_rule(Info,In,[Out,Out2|OutL],TransRule):- is_object(Out),is_object(Out2),
  maplist(trans_rule(Info,In),[Out,Out2|OutL],TransRule), TransRule\==[].

% create
trans_rule(Info,[],Out,Rules):- listify(Out,OutL),
 findall(create_object(Info,rhs(create(Out)),lhs(Preconds)),
   ((member(Out,OutL),into_lhs(Out,Preconds))),Rules), Rules\==[].

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
               create_object2(Info,rhs(creation_step2(Step,Type,New2)), lhs(Precond2))].

% mutiple preconds
trans_rule(Info,[In,In2|InL],OutL,TransRule):- is_object(In),is_object(In2),
  trans_rule(Info,[In2|InL],OutL,TransRuleM), TransRuleM\==[],
  sub_compound(lhs(Precond),TransRuleM),
  noteable_propdiffs(In,OutL,Same,_L,_R),
  append_vsets([Precond,Same],NewPrecond),
  subst(TransRuleM,lhs(Precond),lhs(NewPrecond),TransRule).

% just copy an object
trans_rule(Info,[In],[Out],Rules):- 
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  noteable_propdiffs(In,Out,Same,L,R),L==[],R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].

% just copy an object
trans_rule(Info,In,Out,Rules):- 
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  noteable_propdiffs(In,Out,Same,L,R),L==[],R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].


% copy/transform 
trans_rule(Info,In,Out,Rules):- 
  noteable_propdiffs(In,Out,_Same,_L,R),
  into_lhs(In,LHS),  
  findall(edit_copy(Info,rhs(edit(Type,Change,P)),lhs(LHS)),
    (member(P,R),prop_pairs(In,Out,Type,Change,P),Change\==same,good_for_rhs(P)),Rules),Rules\==[].

trans_rule(Info,E1,E2,Rules):-
  noteable_propdiffs(E1,E2,NSame,NL,NR),
  pp_ilp(grp(Info,E1,E2)),
  dash_chars,
  if_t(how_are_differnt(E1,E2,Set),pp_ilp(how_are_differnt=Set)),
  flat_props(E1,FP1),flat_props(E2,FP2),
  intersection(FP1,FP2,Same,InFlatP,OutPFlat),
  pp_ilp(removed=InFlatP),
  pp_ilp(sames=Same),
  pp_ilp(added=OutPFlat),
  pp_ilp(info=Info),
  pp_ilp(nremoved=NL),
  pp_ilp(nsames=NSame),
  pp_ilp(nadded=NR),
  sub_compound(step(Step),Info), sub_compound(why(TypeO),Info),
  dash_chars,
  Rules = [ 
    create_object_step(Info,rhs(create3c(Step,TypeO,E2)),lhs(Same)) ].
    %copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].















