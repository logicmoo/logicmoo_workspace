/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/






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
 var(P), \+ \+ ac_db(TestID,IO,_,_), 
  ac_db(TestID,IO,P,_),map_pairs_info2(TestID,IO,P,Step).
  
  
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


% delete
trans_rule(Info,In,[],Rules):- listify(In,InL),
 findall(delete_object(Info,rhs(delete(In)),lhs(Preconds)),
   (member(In,InL),into_lhs(In,Preconds)),Rules).

% mutiple postconds
trans_rule(Info,In,[Out,Out2|OutL],TransRule):- is_object(Out),is_object(Out2),
  maplist(trans_rule(Info,In),[Out,Out2|OutL],TransRule).

% create
trans_rule(Info,[],Out,Rules):- listify(Out,OutL),
 findall(create_object(Info,rhs(create(Out)),lhs(Preconds)),
   ((member(Out,OutL),into_lhs(Out,Preconds))),Rules).

% 2 preconds
%trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2),
%  TransRule = create_object2(Info,rhs(create_obj(Out)),lhs(into_new(In1,In2))),!.

% 2 preconds
trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2), % fail,
   noteable_propdiffs(In1, Out,_Same1,_DontCare1,New1), 
   noteable_propdiffs(In2,New1,_Same2,_DontCare2,New2),
   %remove_o_giz(Out,RHSO), 
   remove_o_giz(In1,Precond1), remove_o_giz(In2,Precond2),
   Info = info(Step,_IsSwapped,_Ctx,TypeO,_,_,_),!,
 % append_sets(Same1,Same2,Same), append_sets(DontCare1,DontCare2,DC), append_sets(New1,New2,New),
 % append_sets(Same,New,NewObj),
  %make_common(RHSO,LHS1,NewOut1,NewLHS1),
  %make_common(NewOut1,LHS2,NewOut,NewLHS2),
  TransRule = [create_object1(Info,rhs(creation_step1(Step,TypeO,New1)), lhs(Precond1)),
               create_object2(Info,rhs(creation_step2(Step,TypeO,New2)), lhs(Precond2))],!.

% mutiple preconds
trans_rule(Info,[In,In2|InL],OutL,TransRule):- is_object(In),is_object(In2),
  trans_rule(Info,[In2|InL],OutL,TransRuleM),
  sub_compound(lhs(Precond),TransRuleM),
  noteable_propdiffs(In,OutL,Same,_L,_R),
  append_vsets([Precond,Same],NewPrecond),
  subst(TransRuleM,lhs(Precond),lhs(NewPrecond),TransRule),!.

% just copy an object
trans_rule(Info,[In],[Out],Rules):- 
  Info = info(Step,_IsSwapped,_Ctx,TypeO,_,_,_),
  noteable_propdiffs(In,Out,Same,L,R),L==[],R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].


% copy/transform  1-to-1
trans_rule(Info,[In],[Out],Rules):- 
  noteable_propdiffs(In,Out,Same,_L,[P]),good_for_rhs(P),
  into_lhs(Same,LHS),
  findall(edit_copy(Info,rhs(edit(Type,Change,P)),lhs(LHS)),
    (prop_pairs(In,Out,Type,Change,P),Change\==same,good_for_rhs(P)),Rules).

% copy/transform 
trans_rule(Info,[In],[Out],Rules):- 
  noteable_propdiffs(In,Out,Same,_L,_),
  into_lhs(Same,LHS),
  findall(edit_copy(Type,Change,Info,rhs(edit(P)),lhs(LHS)),
    (prop_pairs(In,Out,Type,Change,P),Change\==same,good_for_rhs(P)),Rules).















