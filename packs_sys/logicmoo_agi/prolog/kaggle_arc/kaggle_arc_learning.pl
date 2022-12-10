/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).



:- dynamic(learnt_rule/5).
:- dynamic(print_rule/5).

learned_test:- notrace((get_current_test(TestID),learned_test(TestID))).
learned_test(TName):- 
  fix_test_name(TName,TestID),
   format('% ?- ~q. ~n',[learned_test(TName)]),
   %forall(clause(learnt_rule(TestID,A,B,C,D),Body),
    %print_rule(learned_test,learnt_rule(TestID,A,B,C,D):-Body)),
    training_info(TestID,Info),
    maplist(print_rule(TestID),Info),
    length(Info,Len),
    ptc(orange,format('~N~n% Rules Learned: ~w~n~n',[Len])),!.


is_clause_ref(Ref):-  atomic(Ref), blob(Ref,_), \+ atom(Ref), clause_name(Ref,_),!.

print_rule(M,Ref):-is_clause_ref(Ref),!,
  clause(H,B,Ref), print_rule(M,(H:-B)).

print_rule(M,(X:-True)):- True == true,!, print_rule(M,X).
print_rule(M,(learnt_rule(TestID,A,B,C,D):-Body)):- fail, !,
   \+ \+ (( ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    orpt(M=[C=[TestID,in=(A),label=(B),out=(D)]]))).
print_rule(M,(X:-Body)):- !,
    \+ \+ ((  ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    orpt(M=[X]))).
print_rule(M,(X:-Body)):- !,
    \+ \+ ((  ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
              ignore((Body=was_once(InSet,InVars),maplist(=,InSet,InVars))),   
    orpt(M=[X]))).
print_rule(M,O):- \+ \+ (( orpt(M=[O]))).

orpt(G):- \+ \+ ((numbervars(G,0,_,[attvar(bind),singletons(true)]), format('~N'), pp_safe(orange,(call(print(G)))))).

save_learnt_rule(TestID,In,InKey,RuleDir,Out):-
  if_learn_ok(save_learnt_rule(learnt_rule(TestID,In,InKey,RuleDir,Out))).

save_learnt_rule(RuleIn):- save_learnt_rule(RuleIn,RuleIn,RuleIn).
save_learnt_rule(RuleIn,InGoal,OutGoal):-  
  labels_for(InGoal,OutGoal,InSet),
  length(InSet,InLen),length(InVars,InLen),
  subst_rvars(InSet,InVars,RuleIn,NewRuleIn),!,
  Assert = (NewRuleIn:-was_once(InSet,InVars)), 
  assert_visually(Assert),!.    


learn_group(What,Objs):- assert_visually(group_associatable(What,Objs)).

learn_about_group(In):- 
  forall(group_keys(What),
   (group_group(What,In,Groups),
    maplist(learn_group(What),Groups))).

not_for_matching(_Why,_,Var):- var(Var),!,fail.
not_for_matching(_Why,_,C):- notrace((sub_term(E,C), compound(E))), E= '$VAR'(_),!,fail.
not_for_matching(_Why,_,iz(combined)).
not_for_matching(_Why,_,giz(_)).
not_for_matching(_Why,_,iz(colormass)).
not_for_matching(_Why,_,iz(nsew)).
not_for_matching(_Why,_,iz(image)).
not_for_matching(_Why,_,iz(monochrome)).
not_for_matching(_Why,_,iz(C)):- atom(C),!,fail.
%not_for_matching(_Why,_,iz(C)):- sub_term(E,C), number(E),E\==1.
not_for_matching(_Why,_,iz(_)):- !, fail.
%not_for_matching(_Why,localpoints(_)).
%not_for_matching(_Why,_,link(_,_,_)).
not_for_matching(_Why,_,birth(_)).
not_for_matching(_Why,_,obj_to_oid(_,_)).
%not_for_matching(_Why,L,form(_)):- !, member(localpoints(_),L).
not_for_matching(_Why,L,localpoints(XX)):- !, started_is_list(XX), member(shape(_),L).
not_for_matching(_Why,L,globalpoints(XX)):- !, started_is_list(XX), (member(shape(_),L);member(localpoints(_),L)).

%not_for_matching(_Why,_,center2G(H,V)):- (H\==1,V\==1,H\==2,V\==2,H\==3,V\==3).
%not_for_matching(_Why,_,loc2D(H,V)):- (H\==1;V\==1).
%not_for_matching(_Why,_,M):- too_unique(M),!.
%not_for_matching(_Why,_,M):- too_non_unique(M),!.

started_is_list(X):- nonvar(X), X = [_,_].


my_exclude(P1,I,O):- my_partition(P1,I,_,O).
simplify_for_matching(_Why,I,O):- is_grid(I),O=I.
simplify_for_matching(_Why,I,O):- var(I),O=I.
simplify_for_matching(Why,obj(I),obj(OL)):- is_list(I),!, my_exclude(not_for_matching(Why,I),I,M), append(M,_,OL).
simplify_for_matching(_Why,I,I):- ( \+ compound(I); I='$VAR'(_)), !.
simplify_for_matching(Why,I,O):- is_list(I), my_exclude(not_for_matching(Why,I),I,M),I\=@=M,!,simplify_for_matching(Why,M,O).
simplify_for_matching(Why,I,O):- 
       compound_name_arguments(I, F, Args),
       maplist(simplify_for_matching(Why), Args, ArgsNew),
       compound_name_arguments( O, F, ArgsNew ),!.

member_skip_open_(_, El, El).
member_skip_open_([H|T], El, _) :- nonvar(T),
    member_skip_open_(T, El, H).

member_skip_open(El, [H|T]) :-
    member_skip_open_(T, El, H).


group_group(What,[Obj|In],[G1|Groups]):- indv_props(Obj,Props), member_skip_open(P,Props),matches_key(P,What),!,
  [Obj|Include] = G1,
  my_partition(has_prop(P),In,Include,Exclude),
  group_group(What,Exclude,Groups).
group_group(_,_,[]).

group_keys(iz).
group_keys(amass).
group_keys(birth).
group_keys(color).
%group_keys(shape).
group_keys(rot2L).
group_keys(localpoints).
group_keys(symmetry).

matches_key(P,What):- atom(What), nonvar(P), !,functor(P,What,_).
matches_key(P,What):- nonvar(P), What = P.

simplify_for_matching_nondet(Why,I,O):- is_list(I), is_group(I), \+ is_grid(I), !, 
   group_keys(Key),
   once((group_group(Key,I,ListOfLists),
   list_to_set(ListOfLists,Set))),
   member(M,Set),once(simplify_for_matching(Why,M,O)).
simplify_for_matching_nondet(Why,I,O):- simplify_for_matching(Why,I,O).


train_io_from_hint(TestID,ExampleNum,InVM):-
  ignore((          
    kaggle_arc_io(TestID,ExampleNum,out,ExpectedOut),
    kaggle_arc_io(TestID,ExampleNum,in,InGrid),
   (var(InVM) -> into_fti(TestID>ExampleNum*in,in_out,InGrid,InVM) ; true),
    gset(InVM.grid) = InGrid,
    gset(InVM.grid_target) = ExpectedOut,
    do_sols_for(_All,"Do Training",InVM,TestID,ExampleNum))),
  confirm_train_io_from_hint(TestID,ExampleNum).

confirm_train_io_from_hint(TestID,ExampleNum):-
  InVM = _,
  ignore((          
   %kaggle_arc_io(TestID,ExampleNum,out,ExpectedOut),
    kaggle_arc_io(TestID,ExampleNum,in,InGrid),
   (var(InVM) -> into_fti(TestID>ExampleNum*in,in_out,InGrid,InVM) ; true),
    gset(InVM.grid) = InGrid,
   % gset(InVM.grid_target) = ExpectedOut,
    do_sols_for(_All,"Confirm Trained",InVM,TestID,ExampleNum))).


learn_rule_o(out_out,_InVM,_OutVM):- !.
learn_rule_o(out_in,_InVM,_OutVM):- !.
learn_rule_o(in_in,_InVM,_OutVM):- !.
learn_rule_o(Mode,InVM,OutVM):- % is_map(InVM),is_map(OutVM),!,
 in_out = Mode,
 maplist(must_det_ll,[
  InGrid = InVM.grid_o, InObjs0 = InVM.objs,  
  OutGrid = OutVM.grid_o, OutObjs0 = OutVM.objs,
  ignore(InVM.grid_target = OutGrid),
  maplist(simplify_for_matching(lhs),InObjs0,InObjs),
  %maplist(simplify_for_matching,OutObjs0,OutObjs),
  OutObjs0=OutObjs,
 % extract_vm_props(InVM,InProps),     
 % extract_vm_props(OutVM,OutProps), 
 % prolog_pretty_print:print_term(Mode=learn_rule_o(Mode,InProps,OutProps),[fullstop(true),nl(true)]),!,
  %learn_grid_rules(Mode,InProps),
  %learn_grid_rules(Mode,OutProps),!,
  learn_rule_i_o(Mode,InObjs,OutObjs),
  %learn_rule_i_o(Mode,InGrid,OutObjs),
  %learn_rule_i_o(Mode,InObjs,OutGrid),
  %learn_rule_i_o(Mode,InGrid,OutGrid),
  get_current_test(TestID),
  training_info(TestID,Info),
  length(Info,Len),
  ptc(orange,format('~N~n% Rules so far for ~w: ~w~n~n',[TestID,Len])),!,
  if_learn_ok(confirm_reproduction(InObjs,InObjs0,InGrid)),!,
  if_learn_ok(confirm_reproduction(OutObjs,OutObjs0,OutGrid)),!,
  confirm_learned(InGrid,OutGrid),!,
  nop(show_proof(InGrid,OutGrid))]),!.


learn_rule_i_o(Mode,In,Out):- 
  forall(learn_rule_in_out(1,Mode,In,Out),true).

is_reproduction_obj(O):- \+ is_object(O),!.
is_reproduction_obj(O):-  \+ iz(O,hidden).

reproduction_objs(O,Os):- include(is_reproduction_obj,O,Os).

confirm_reproduction(Objs0,DebugObjs0,ExpectedOut):-    
 must_det_ll((
  grid_size(ExpectedOut,H,V),
  grid_size(DebugObjs0,DH,DV),
  reproduction_objs(Objs0,Objs), 
  reproduction_objs(DebugObjs0,DebugObjs),
  length(Objs0,Len0),
  length(Objs,Len),
  globalpoints(Objs,OGPoints),
  call((points_to_grid(H,V,OGPoints,Solution)->true;points_to_grid(DH,DV,OGPoints,Solution))),
  count_difs(ExpectedOut,Solution,Errors),
  show_result("Our Reproduction"=Len0/Len, Solution,ExpectedOut,Errors),
  (Errors==0 -> true; maplist(debug_reproduction(H,V),Objs,DebugObjs)))).

debug_reproduction(H,V,Obj,DObj):- 
 must_det_ll((
  globalpoints(Obj,Points),
  tersify(Obj,Info),
  print_grid(H,V,Info,Points),
  obj_to_oid(Obj,ID1),
  obj_to_oid(DObj,ID2),
  pp_safe(dobj(ID1,ID2)=DObj))),!.

pp_safe(W):- nl_if_needed,nl,writeq(W),nl.
pp_safe(_,W):- nl_if_needed,nl,writeq(W),nl.

show_result(What,Solution,ExpectedOut,Errors):-
  show_sameness_or_lameness(green,red,What,Solution,ExpectedOut,Errors).

show_sameness_or_lameness(Green,Red,What,OurOut,ExpectOut,Errors):- 
   ignore(( get_current_test(TestID), test_info(TestID,InfoF))), !,
   ignore(( (var(Errors)->count_difs(OurOut,ExpectOut,Errors);true),
    (Errors==0 -> 
      ColorMessage=wqs(Green,pass(What,TestID));
      ColorMessage=wqs(Red,fail(errors(Errors),What,TestID))),
    ColorMessage = wqs(Color,Message),    
    banner_grids(Color,OurOut,Message,ExpectOut,fav(TestID,InfoF)))).

banner_grids(Color,I,Message1,O,Message2):- 
 ignore((
    banner_lines(Color),
    print_side_by_side(Color,I,Message1,_,O,Message2),
    format('~N'),pp_safe(Color,Message1),
    format('~N'),pp_safe(Color,Message2),
    format('~N'),
    banner_lines(Color))).

arcdbg_info(Color, Info):- banner_lines(Color), arcdbg(Info), banner_lines(Color).

confirm_learned(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (     use_test_associatable(Objs,Solution) -> 
   show_result("Our Learned Solution", Solution,ExpectedOut,_)
   ; arcdbg_info(red,warn("No Learned Solution"))).


show_proof(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (test_associatable_proof(Objs,Solution) -> 
   show_result("Our Proved Solution", Solution,ExpectedOut,_Errors)
   ; arcdbg_info(red,warn("No Proved Solution"))).

%learn_rule_o(_Mode,G,_):- is_list(G), is_group(G), learn_about_group(G), fail.
%learn_rule_o(_Mode,_,G):- is_list(G), is_group(G), learn_about_group(G), fail.

/*
learn_rule_in_out(Depth,Mode,In,Out):- is_list(In), is_list(Out), 
  \+ is_grid(In), \+ is_grid(Out),
  \+ is_group(In), \+ is_group(Out),
  forall(select_some(I,In,MoreIn),
   forall(select_some(O,Out,MoreOut),
    (learn_rule_in_out(Depth,Mode,MoreIn,MoreOut),
     learn_rule_in_out(Depth,Mode,I,O)))).
*/

%learn_rule_in_out(Depth,out_out,I,O):- !, ignore(( is_grid(O), save_learnt_rule(oout_associatable(I,O)))).

compare_objs_how([perfect]).
compare_objs_how([turned,+loc2D]).
compare_objs_how([turned,-loc2D]).
compare_objs_how([moved]).
compare_objs_how([sameO]).
compare_objs_how(_).

/*
vis2D(5,5), amass(25),
center2G(9,14),loc2D(7,12),
colors([cc(PURPLE,21),cc(BLACK,4)]),
localpoints
*/

comparable_2props(iz(I),iz(O)):-!,nonvar(I),nonvar(O),comparable_2props(I,O).
comparable_2props(I,O):- compound(I),compound(O),functor(I,F,A),functor(O,F,A).
same_2props(I,O):- comparable_2props(I,O), I  =@= O.
diff_2props(I,O):- comparable_2props(I,O), I \=@= O.


% form, 
% vis2D, mass
% center2G


% symmetrical object
% non-symmetrical
% object with exact mass
% object with most mass
% object with least mass
% distinctive color
% square object
% rectangular object
% hollow object
% solid object
% object count
/*
 test_tag(count_different_colors).
 test_tag(count_hor_lines).
 test_tag(count_patterns).
 test_tag(count_shapes).
 test_tag(count_tiles).
 test_tag(count_ver_lines).
  test_tag(associate_color_to_bools).
 test_tag(associate_colors_to_bools).
 test_tag(associate_colors_to_colors).
 test_tag(associate_colors_to_images).
 test_tag(associate_colors_to_numbers).
 test_tag(associate_colors_to_patterns).
 test_tag(associate_colors_to_ranks).
 test_tag(associate_colors_to_shapes).
 test_tag(associate_images_to_bools).
 test_tag(associate_images_to_colors).
 test_tag(associate_images_to_images).
 test_tag(associate_images_to_numbers).
 test_tag(associate_images_to_patterns).

 test_tag(associate_patterns_to_colors).
 test_tag(associate_patterns_to_patterns).
*/


:- discontiguous(learn_rule_in_out/4).
learn_rule_in_out(_,in_out,In,Out):- is_list(In),is_list(Out),
  (learn_rule_in_out_sames(In,Out), deterministic(TF), true), (TF==true -> !; true).

learn_rule_in_out_sames(In,Out):- fail,
  is_list(In),is_list(Out),
  average_or_mid(mass,Out,MinMass),
  member(I,In),member(O,Out),
  mass(O,Mass), Mass>MinMass,
  mass(I,Mass),
  once((compare_objs_how(How), nonvar(How), compare_objs1(How,I,O))),
  pp_safe(How),
  learn_rule_in_out_objects(How,I,O).

learn_rule_in_out_objects(How,I,O):-   
  simplify_for_matching(lhs,I,II),
  simplify_for_matching(rhs,O,OO),
  save_learnt_rule(test_solved(How,II,OO),I+O,I+O),!.

average_or_mid(_P2,_Out,2):-!.
average_or_mid(P2,Out,MinMass):- is_list(Out),!,
  findall(Mass,(member(O,Out),call(P2,O,Mass)),Masses),
  average_or_mid_n(Masses,MinMass).
average_or_mid(P2,O,Mass):- call(P2,O,Mass).

average_or_mid_n(Masses,MinMass):- 
  min_list(Masses,Max),max_list(Masses,Min), MinMass1 is (Max+Min)/2,
  sumlist(Masses,Sum),length(Masses,Len), MinMass2 is Sum/Len,
  max_min(MinMass1,MinMass2,MinMass,_).
 
learn_rule_in_out(_,in_out,In,Out):- is_list(In),is_list(Out),
  (learn_rule_in_out_level1(In,Out), deterministic(TF), true), (TF==true -> !; true).

learn_rule_in_out_level1(In,Out):- fail,
  is_list(Out), average_or_mid(mass,Out,MinMass),
  member(O,Out), mass(O,Mass), Mass>MinMass,
  indv_props(O,OL),
  once(learn_rule_iin_oout(1,In,O,OL)).

learn_rule_iin_oout(_,In,O,OL):- mass(O,Mass),
  findall(SL-SAME-I-DL,
   (member(I,In),indv_props(I,IL),
    pred_intersection(same_2props,IL,OL,SAME,_,_IF,_OF),
    pred_intersection(diff_2props,IL,OL,DIFF,_,_,_),
    length(SAME,SL), length(DIFF,DL),SL>DL),SLIDL),
  sort(SLIDL,SSLIDL),
  reverse(SSLIDL,RSLIDL),
  member(SL-SAME-I-DL,RSLIDL),
  pp_safe([SL+DL, equal = SAME, in=I,out=OL]),  
  compare_objs1(How,I,O),
  %shape(I,Shape),shape(O,Shape),
  %pen(I,Pen),pen(O,Pen),
  mass(I,Mass),
  simplify_for_matching(lhs,I,II),
  simplify_for_matching(rhs,O,OO),
  save_learnt_rule(test_solved(unk(How),II,OO),I,O),!.

learn_rule_in_out(Depth,Mode,In,Out):- 
  is_list(In), is_list(Out), 
  maplist(compound,In), maplist(compound,Out),
  length(In,L), length(Out,L),
  Depth2 is Depth+1, 
  maplist(learn_rule_in_out(Depth2,Mode),In,Out).

learn_rule_in_out(Depth,Mode,In,Out):-
  forall(simplify_for_matching_nondet(lhs,In,InS),
    forall(simplify_for_matching_nondet(lhs,Out,OutS),
      learn_rule_in_out_now(Depth,Mode,InS,OutS))).

learn_rule_in_out(Depth,Mode,In,Out):- 
  is_group(In),is_group(Out),
  length(In,IL),length(Out,OL),
  Depth2 is Depth+1, 
  if_t((IL=<7,OL=<7),
   forall(member(I,In),
     forall(member(O,Out),
       learn_rule_in_out_now(Depth2,Mode,I,O)))).

%learn_rule_in_out_now(Depth,Mode,_-In,Out):-!,learn_rule_in_out_now(Depth,Mode,In,Out).
%learn_rule_in_out_now(Depth,Mode,In,_-Out):-!,learn_rule_in_out_now(Depth,Mode,In,Out).
learn_rule_in_out_now(_Depth,_Mode,In,Out):- is_list(In),is_list(Out), \+ is_grid(In), \+ is_grid(Out), length(In,L1), length(Out,L2), 
   \+ (L1 is L2 ; (L1 is L2 * 2, L2>1); (L2 is L1 * 2, L1>1)),!.

learn_rule_in_out_now(Depth,Mode,[In],[Out]):- \+ is_grid([In]), \+ is_grid([Out]), !, learn_rule_in_out_now(Depth,Mode,In,Out).
learn_rule_in_out_now(Depth,Mode,In,Out):-  is_list(In),is_list(Out), 
  maplist(compound,In), maplist(compound,Out),
  length(In,L), length(Out,L), !,
  Depth2 is Depth+1, 
  maplist(learn_rule_in_out_now(Depth2,Mode),In,Out).
  %learn_rule_in_out(Depth,Mode,InS,OutS).
learn_rule_in_out_now(_Depth,_Mode,In,Out):- nop(save_learnt_rule(test_associatable(In,Out),In,Out)).

%learn_rule_in_out(Depth,Mode,I,O):- save_learnt_rule(test_associatable(Mode,I,O)).

extract_vm_props(VM,[VM.grid,VM.objs]).

select_some(I,List,Rest):- append(_,[I|Rest],List).

learn_grid_rules(Mode,Props):-  
  forall(select_some(P,Props,Others),
    learn_rule_ee(Mode,P,Others)).
learn_rule_ee(Mode,P,Others):- forall(member(O,Others),learn_grid_local(Mode,P,O)).

learn_grid_local(Mode,P,O):- P @< O, !, learn_grid_local(Mode,O,P).
learn_grid_local(_Mode,P,O):- ignore((\+ is_grid(P),is_grid(O),assert_visually(grid_associatable(P,O)))).

test_local_dyn(F,A):- setof(F/A,(test_local_dyn(F),current_predicate(F/A)),L),member(F/A,L),A\==0.
:- dynamic(test_local_dyn/1).
test_local_dyn(learnt_rule).
test_local_dyn(grid_associatable).
test_local_dyn(test_associatable).
test_local_dyn(test_solved).
%test_local_dyn(why_grouped).
test_local_dyn(cached_dictation).
test_local_dyn(oout_associatable).

test_local_save(F,A):- setof(F/A,(test_local_save(F),current_predicate(F/A),A\==0),L),member(F/A,L).
test_local_save(note).
test_local_save(individuated_cache).
test_local_save(g_2_o).
test_local_save(cmem).
test_local_save(cindv).
test_local_save(is_why_grouped_g).
test_local_save(arc_test_property).
test_local_save(gid_glyph_oid).
test_local_save(oid_glyph_object).
test_local_save(P):- test_local_dyn(P).

training_info(TestID,InfoSet):-
 sub_atom_value(TestID,TestIDA),
  findall(Ref,
  (test_local_dyn(F,A), functor(X,F,A),
      clause(X,_,Ref),once((arg(_,X,E),sub_atom_value(E,AV),atom_contains(AV,TestIDA)))),Info),
  list_to_set(Info,InfoSet).

saveable_test_info(TestID,InfoSet):-
 sub_atom_value(TestID,TestIDA),
 findall(Ref,
  (test_local_save(F,A), functor(X,F,A),
      clause(X,_,Ref),once((arg(_,X,E),sub_atom_value(E,AV),atom_contains(AV,TestIDA)))),Info),
 list_to_set(Info,InfoSet).



assert_visually(H:-B):- !,unnumbervars((H:-B),(HH:-BB)), assert_visually1(HH,BB).
assert_visually( H  ):- unnumbervars(H,HH),assert_visually1(HH,true).

assert_visually1(H,B):- get_current_test(TestID), arg(1,H,W),W\==TestID,!, H=..[F|Args],GG=..[F,TestID|Args],assert_visually2(GG,B).
assert_visually1(H,B):- assert_visually2(H,B).

assert_visually2(H,B):- copy_term((H:-B),(HH:-BB)),clause(HH,BB,Ref), clause(RH,RB,Ref),(H:-B)=@=(RH:-RB) ,!,(pp_safe(cyan,known_exact(H:-B))).
assert_visually2(H,B):- copy_term((H),(HH)),clause(HH,_,Ref), clause(RH,_,Ref),(H)=@=(RH) ,!,pp_safe(cyan,known(H:-B)).
assert_visually2(H,B):- functor(H,F,_), my_asserta_if_new(test_local_dyn(F)), print_rule(F,(H:-B)), my_asserta_if_new((H:-B)).

if_learn_ok(G):- call(G).


 

learn_rule(In,Out):-
  get_vm(VM),
  VM.rule_dir = RuleDir,
  learn_rule(In,RuleDir,Out).



learn_rule(In,RuleDir,Out):- 
 get_vm(VM), 
 Target=VM.grid_target, 
 is_grid(Target),!,
 Out = Target,
 get_current_test(TestID), 
 get_vm(last_key,Key),
 save_learnt_rule(TestID,In,Key,RuleDir,Out),!.

learn_rule(In,RuleDir,ROut):- nop(use_learnt_rule(In,RuleDir,ROut)).

/*
instead of a DSL about transformations i think the DSL would be one that creates the images.. 
then the transformation becomes about what properties of the generative DSL change (both the input and the output 
have their own starting points that are gleaned by looking at what DSL would generate all the outputs vs what 
DSL would generate all the inputs) the thing that is learned by training is how the edits are supposed to happen in each
 of the generative DSLs (edited)
the progression of inputs teaches the system abotu what the input's generative DSL is used for inputs (edited)
though the progression of outputs give more information about the total test (but still give the hints about the the output's generative DSL) 
round tripping between a grid and the generative DSL seems important.   Has anyone started a Grid-> "generative DSL" convertor ?
oh yes even the operations such editing/transformation of the generative DSL is in the domain of yet another DSL that specifies those operations.. and in my code i even have a 3rd DSL that is specific to transformations that both the two previous DSLs are indexed against.. in order to make the things fast i assume that the three forms each transition between will be part of the final index built during training (edited)
s the training pairs are fed in it eliminates the candidate associations
(the more training the faster it gets)
this way the only associations it uses and retains are correct ones 
we'll see if there are extra possible correct associations (which may end up actually being incorrect answers that are logically correct).. 
Luckily the system produces a formal proof for it's answers (explainablity) 
I have also been looking into compression and hashing alternatives in a similar vein such as neural hash and graph compression
 (thanks for the links, BTW!).
*/
properties_that_changed(Grid1,Grid2,Props):-
  individuate(complete,Grid1,Props1),
  individuate(complete,Grid2,Props2),
  diff_terms(Props1,Props2,Props).

has_learnt_rule(TestID,In,Key,RuleDir,Out):- clause(learnt_rule(TestID,In,Key,RuleDir,Out),was_once(InSet,InVars)),
  ignore_equal_e(InSet,InVars).

ignore_equal_e(InSet,InVars):- maplist(ignore_equal,InSet,InVars).
ignore_equal(X,Y):- ignore(X=Y).  

rev_key0(C-P,P-C).

%pp_safe(O):- format('~N'),print(O),nl.
use_test_associatable(In,Solution):- 
  simplify_for_matching(lhs,In,IIn),
  %pp_safe(in=IIn),  
  findall(Ref-OutS,use_test_associatable_io(IIn,OutS,Ref),OutL),
  keysort(OutL,OutLS),
  maplist(arg(2),OutLS,OutLS2),
  clumped(OutLS2,OutLS3),
  maplist(rev_key0,OutLS3,OutLS4),
  sort(OutLS4,OutLS2SS),
  reverse(OutLS2SS,OutLS2SSR),
  maybe_four_terse(OutLS2SSR,OutLS2SSRT),
  print(outs=OutLS2SSRT),nl,
  maplist(arg(2),OutLS2SS,OutLS2SSBest),
  last(OutLS2SSBest,Best),
  globalpoints(Best,OGPoints),  
  points_to_grid(OGPoints,Solution).

use_test_associatable(In,OutR):- 
  findall(InS,simplify_for_matching_nondet(lhs,In,InS),InL),
  findall(OutS-Ref,(member(InS,InL),use_test_associatable_io(InS,OutS,Ref)),OutL),    
   OutSet=[for_output2],     
   nb_set_add1(OutSet,OutL),
   ignore(OutR=OutSet),!,
   pp_safe(outSet2=OutSet).

test_associatable_proof(In,OutR):-
  findall(InS,simplify_for_matching_nondet(lhs,In,InS),InL),
   findall(OutS,
     (member(InS,InL),use_test_associatable_io(InS,OutS,_Ref),
      arcdbg_info(cyan,proof([in=InS,out=OutS]))),OutL),
    Out=[test_associatable_proof],
  nb_set_add1(Out,OutL),
  ignore(OutR=Out),!.


ignore_some_equals(OutS,Out):- must_det_ll( nb_set_add1(OutS,Out)).

:- dynamic(test_associatable/3).
:- dynamic(test_solved/4).
use_test_associatable_io(I,O,Ref):- is_list(I),!,member(E,I),use_test_associatable_io(E,O,Ref).
use_test_associatable_io(I,O,Ref):- get_current_test(TestID), clause(test_solved(TestID,_How,Pre,O),_,Ref), \+ \+ same_props(I,Pre).
use_test_associatable_io(I,O,Ref):- get_current_test(TestID),
  clause(test_associatable(TestID,Pre,O),_,Ref),
  \+ \+ same_props(I,Pre),
  nop(pp_safe(same_props(I,Pre))).

same_props(I,Pre):- (var(I);var(Pre)),!.
same_props(I,Pre):- ([]==(I);[]==(Pre)),!.
same_props(obj(I),Pre):- !, same_props(I,Pre).
same_props(I,obj(Pre)):- !, same_props(I,Pre).
same_props(E1,E2):- E1=E2,!.
same_props(I,Pre):- select(E2,Pre,PrePre),select(E2,I,II),!,same_props(II,PrePre).

use_learnt_rule(In,_RuleDir,Out):- use_test_associatable(In,Out).

use_learnt_rule(In,RuleDir,ROut):- %get_vm(VM), %Target=VM.grid_target, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)),
  ((has_learnt_rule(TestID,In,Key,RuleDir,Out);has_learnt_rule(TestID,_,Key,RuleDir,Out);has_learnt_rule(TestID,In,_,RuleDir,Out))),
  pp_safe(orange,using_learnt_rule(In,Key,RuleDir,Out)),
  ignore(Out = ROut).

use_learnt_rule(In,RuleDir,Out):- get_vm(VM), % Target=VM.grid_target, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)), 
   In = VM.grid_o,
   Head = learnt_rule(TestID0,In0,Key0,RuleDir0,Out0),
   Rule = rule(Len,In0,Key0,RuleDir0,TestID0,Out0,Ref),
   pp_safe(searching_for=[in(In),dir(RuleDir),key(Key)]),
  findall(Rule,
   (clause(Head,_Vars,Ref),
    call(Head),
    matchlen([In0,Key0,RuleDir0,TestID0],[In,Key,RuleDir,TestID],Len)),
   Matches),
   sort(Matches,Sort),
   %reverse(Sort,Reverse),
   last(Sort,Last),
   must_det_ll(Last = Rule),
   ignore(In=In0),
   ignore(Out = Out0),
   %\+ \+ maplist(print_rule(sort),Matches),
   \+ \+ print_rule(using_learnt_rule=Len,ref(Ref)),!.


matchlen([],[],0):-!.
matchlen([A|List1],[B|List2],Len):- fitness(A,B,Fit),!,
   matchlen(List1,List2,Len2), Len is Fit+Len2.

fitness(A,B,1.1):- A=B,!.
fitness(A,B,Fit):- is_list(A),is_list(B), length(A,L),matchlen(A,B,F), \+ is_grid(A), Fit is F/L,!.
fitness(_,_,0.01):- !.

%row_to_row
%row_to_column
%dot_to_

was_once(_,_).


upcase_atom_var(Num,VAR):- ignore((upcase_atom_var0(Num,Name),VAR='$VAR'(Name))),!.
upcase_atom_var(Num,'$VAR'(Num)):-!.
upcase_atom_var(_,_).
upcase_atom_var0(Int,Name):- integer(Int),atom_concat('INT_',Int,Name).
upcase_atom_var0(Num,Name):- number(Num),atom_concat('FLOAT_',Num,DotName),replace_in_string(['.'-'_dot_'],DotName,Name).
upcase_atom_var0(Atom,Name):- atom(Atom),upcase_atom(Atom,Name).

labels_for(obj(I),obj(O),Labels):- findall(EI,(member(EI,I),member(EO,O),EI=@=EO),Labels),length(Labels,N),N>5,!.
labels_for(InGoal,OutGoal,Labels):-
  labels_for1(InGoal,OutGoal,Labels1),
  labels_for2(InGoal,OutGoal,Labels2),
  append(Labels1,Labels2,Labels),!.

labels_for1(InGoal,OutGoal,Labels):- 
  findall(Atom,(sub_label(Atom,InGoal,OutGoal),maybe_unbind_label(Atom)),Atoms), 
  list_to_set(Atoms,Set),
  include(two_or_more(Atoms),Set,Labels).

labels_for2(obj(I),obj(O),Labels):- !, findall(EI,(member(EI,I),member(EO,O),EI=@=EO),Labels).
labels_for2(InGoal,OutGoal,Labels):- labels_for1(InGoal,OutGoal,Labels).


two_or_more(Atoms,Label):- select(Label,Atoms,Rest),member(Label,Rest).

sub_label(X, X, OutGoal):- sub_var(X,OutGoal).
sub_label(X, Term, OutGoal) :-
    compound(Term),
    %is_list(Term),
    \+ never_labels_in(Term),
    arg(_, Term, Arg),
    sub_label(X, Arg, OutGoal).

never_labels_in(iz(_)).
never_labels_in(shape(_)).
never_labels_in(amass(1)).
never_labels_in(mass(1)).
never_labels_in(loc2D(_,_)).


never_unbind_label(G):- var(G),!.
never_unbind_label(Int):- integer(Int), Int > 7 ; Int == 1 ; Int == 0.
never_unbind_label(true).
never_unbind_label(false).
never_unbind_label(G):- \+ atom(G),!,fail.
never_unbind_label(G):- display_length(G,N),N<3,!.
never_unbind_label(G):- downcase_atom(G,D), upcase_atom(G,D).
never_unbind_label(G):- atom_chars(G,Cs),member(C,Cs),char_type(C,digit),!.

maybe_unbind_label(G):- var(G),!,fail.
maybe_unbind_label(iz(_)):- !,fail.
%maybe_unbind_label(G):- too_non_unique(G).
maybe_unbind_label(G):- never_unbind_label(G),!,fail.
maybe_unbind_label(G):- integer(G),G<1.
maybe_unbind_label(G):- \+ atom(G),!,fail.
maybe_unbind_label(G):- is_color(G).
%maybe_unbind_label(G):- downcase_atom(G,D),\+ upcase_atom(G,D).

subst_rvars([],[],A,A):-!. 
subst_rvars([F|FF],[R|RR],S,D):- ignore(debug_var(F,R)),subst_rvars_1(F,R,S,M), subst_rvars(FF,RR,M,D).

map_find_onto_replace(Var,Var):-var(Var),!.
map_find_onto_replace('$VAR'(X),'$VAR'(X)):-!.
map_find_onto_replace(iz(Find),iz(Replace)):- compound(Find), Find\='$VAR'(_), !, map_find_onto_replace(Find,Replace).
map_find_onto_replace(iz(Find),iz(Find)):- atom(Find),!.
map_find_onto_replace(iz(Find),iz(Find)):- !.
map_find_onto_replace(Find,Replace):- functor(Find,F,A),functor(Replace,F,A),
  ignore((arg(A,Replace,E),upcase_atom_var(F,E))).

subst_rvars_1(Find, Replace, Term, NewTerm ) :- var(Replace), compound(Find), 
  map_find_onto_replace(Find,Replace),nonvar(Replace),!,subst_rvars_1(Find, Replace, Term, NewTerm ).
subst_rvars_1(Find, Replace, Term, NewTerm ) :-
 (Find==Term -> Replace=NewTerm ;
  (is_list(Term)-> maplist(subst_rvars_1(Find, Replace), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_); never_labels_in(Term))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       maplist(subst_rvars_1(Find, Replace), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.


rot_in_incr_90(X,Y):- freeze(X,rot90(X,Y)).
rot_in_incr_90(X,Y):- freeze(X,rot180(X,Y)).
rot_in_incr_90(X,Y):- freeze(X,rot270(X,Y)).

rot_by_90_v1(List):- between_each(dif,List),between_each(rot_in_incr_90,List).

between_each(_P2,[]):- !.
between_each(_P2,[_]):- !.
between_each(P2,[X, Y]):- !, call(P2, X, Y).
between_each(P2,[X, Y, Z]):- !, call(P2, X, Y), call(P2, X, Z), call(P2, Z, Y).
between_each(P2,[X|Text]):- mapgroup(dif(X), Text), between_each(P2,Text).


rot_by_90([A,B,C,D]):- rot_by_90_0([A,B,C,D,A,B,C]).

rot_by_90_0([A,B]):- rot90(A,B),!.
rot_by_90_0([A,B|List]):- rot90(A,B),rot_by_90_0([B|List]).
  
subtractGrid(Out,In,Alien):- plain_var(Alien), remove_global_points(In,Out,Alien),!.
subtractGrid(Out,In,Alien):- plain_var(Out),!,add_global_points(Alien,In,Out).
subtractGrid(Out,In,Alien):- plain_var(In),!,remove_global_points(Alien,Out,In).

find_by_shape(Grid,_Find,_Founds):- Grid==[],!,fail.
find_by_shape(Grid,Find,Founds):- 
 get_vm(VM),
 vis2D(Find,GH,GV),
 decolorize(Find,F), 
 Prog = 
  (all_rotations(F,F1),
   %print_grid(F1),!,
   find_ogs(H,V,F1,Grid),% trace,

   grid_to_points(F1,GH,GV,Points),
   pp_safe(Points),
   make_indiv_object(VM,[iz(find_by_shape),F1,loc2D(H,V),alt_grid_size(GH,GV)],Points,F2)),
 findall(F2,Prog,Matches),
 align_founds(Matches,Founds).

align_founds(Founds,Founds).

in_out(In,Out):-
  luser_getval(test_pairname,PairName),
  into_gridnameA(In,PairName*in),
  into_gridnameA(Out,PairName*out).

lrn0:-    
   in_out(In,Out),
   subtractGrid(Out,In,Alien),
   rot_by_90([Alien,A,B,C]),
   find_by_shape(In,Alien,[A,B,C]),
   find_by_shape(Out,Alien,[A,B,C,Alien]).

lrn:- forall(lrn1, true).
lrn1:- learn_arc(_).

tst:- forall(tst1, true).
tst1:- test_arc(_).

learn_arc(TestID):- with_arc(learn,TestID).

test_arc(TestID):- with_arc(solve,TestID).

with_arc(Action,TestID):- plain_var(TestID),!, findall(Name,fav(Name),L),
  list_to_set(L,S), member(TestID,S), with_arc(Action,TestID).

with_arc(Action,arc):- !, findall(Name,kaggle_arc_io(Name,(_+_),_,_),L),
  list_to_set(L,S), member(TestID,S), with_arc(Action,TestID).

with_arc(Action,TestName):-
  fix_test_name(TestName,TestID,_Type),TestName\==TestID,!,
  with_arc(Action,TestID).

with_arc(solve,TestID):- !, 
  with_arc(learn,TestID),
  forall(between(0,6,Num),with_pair(preview,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(solve,TestID,tst,Num)).

with_arc(test,TestID):- !, 
  with_arc(learn,TestID),
  forall(between(0,6,Num),with_pair(solve,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(solve,TestID,tst,Num)).

with_arc(preview,TestID):- !, 
  forall(between(0,6,Num),with_pair(preview,TestID,trn,Num)),
  forall(between(0,6,Num),with_pair(preview,TestID,tst,Num)).

with_arc(Action,TestID):-
  forall(between(0,6,Num),with_pair(Action,TestID,trn,Num)).

with_pair(Action,TestID,Type,Num):-
  kaggle_arc_io(TestID,Type+Num,in,In),
  kaggle_arc_io(TestID,Type+Num,out,Out),
  with_pair(Action,TestID,Type,Num,In,Out),!.

with_pair(Action,TestID,Type,Num,In,Out):- !,
  name_the_pair(TestID,Type,Num,In,Out,PairName),  
  with_named_pair(Action,TestID,PairName,In,Out).

with_named_pair(preview,TestID,PairName,In,Out):- !,
  dash_chars(60,"|"),nl,nl,nop((wqnl(arc1(TestID)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  show_pair_grid(red,IH,IV,OH,OV,in,out,PairName,In,Out).

with_named_pair(solve,TestID,PairName,In,Out):- !,
  with_named_pair(cheat,TestID,PairName,In,Out).

with_named_pair(cheat,TestID,PairName,In,Out):- !,
  ignore(catch(solve_test(TestID,PairName,In,Out),E,(wdmsg(E),fail))),!.

with_named_pair(learn,TestID,PairName,In,Out):- !,
  nop((wqnl(learning(TestID=PairName)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  %ccs(In,InCC),
  %ccs(Out,OutCC),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair_diff(IH,IV,OH,OV,in(unshared),out(unshared),PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair_i(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair_i(IH,IV,OH,OV,combined,PairName,BetterC,Out),
  compute_shared_indivs(In,SharedIn),
  compute_shared_indivs(Out,SharedOut),
  show_pair_diff(IH,IV,OH,OV,shared_in,shared_out,PairName,SharedIn,SharedOut),!,
  ((wqnl(learning_diff(TestID=PairName)),nl)),
  showdiff(SharedOut,SharedIn),
  ((wqnl(learned(TestID=PairName)),nl)).

name_the_pair(TestID,Type,Num,In,Out,PairName):- 
  name_the_pair(TestID,Type+Num,In,Out,PairName).

name_the_pair(TestID,ExampleNum,In,Out,PairName):- 
  PairName= (TestID>ExampleNum),
  get_current_test(CName),
  new_test_pair(PairName),
  /*must_det_ll*/((
   ignore((CName\==TestID, 
        set_current_test(TestID),
        dash_chars(60,"A"),nl,dash_chars(60,"|"),dash_chars(6,"\n"),nl,
        dash_chars(60,"|"),nl,dash_chars(60,"V"),nl,
        nl,wqnl(arc1(TestID)),nl,nl,dash_chars(60,"A"),nl)),   
  GridNameIn= PairName*in,
  GridNameOut= PairName*out,
  set_grid_tid(In,GridNameIn),
  set_grid_tid(Out,GridNameOut),  
  test_info(TestID,Info), pp(fav(TestID,Info)=ExampleNum),nl)).
  


compute_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   compute_unshared_indivs(GN,Grid,Unshared),!.

compute_unshared_indivs(_GN,Grid,Unshared):-
   individuate(complete,Grid,Unshared).

compute_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   compute_shared_indivs(GN,Grid,SharedIndvs).
compute_shared_indivs(GN,Grid,SharedIndvs):-
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   compute_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs).

grid_shared_with(TestName>ExampleNum*in,TestName>ExampleNum*out):-!.
grid_shared_with(TestName>ExampleNum*out,TestName>ExampleNum*in):-!.

get_grid_and_name(In,Grid,GN):- is_grid(In),!,grid_to_tid(Grid,GN).
get_grid_and_name(In,Grid,GN):- into_grid(In,Grid),!,grid_to_tid(Grid,GN).

ensure_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   ensure_unshared_indivs(GN,Grid,Unshared).
ensure_unshared_indivs(GN,Grid,Unshared):-
   is_unshared_saved(GN,Unshared)-> true;
   individuate(complete,Grid,Unshared),
   arc_assert(is_unshared_saved(GN,Unshared)).

ensure_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   ensure_shared_indivs(GN,Grid,SharedIndvs).
ensure_shared_indivs(GN,Grid,SharedIndvs):-
   is_shared_saved(GN,SharedIndvs)-> true;
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   ensure_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs),
   arc_assert(is_shared_saved(GN,SharedIndvs)).


/*

! 
_
/
\


 1 1 1 1 2  -> 2
 1 1 1 -> 1 \
 2 2  -> 2  / _
 1 2 3 4 5  -> _
 2 2 2 2 2  -> 2
 _ _ _ _ 4  -> _

*/
  
growthchart_to_grid(GrowthChart,Color,Fill,BGrid):-
  bg_sym(BG), 
  subst_each(GrowthChart,[
   ' '=BG, ','=Fill, '.'=Fill, '/'=Color, '|'=Color, '-'=Color,
   '_'=Color, '='=Color, '\\'=Color, 'o'=Color], BGrid).

learned_color_inner_shape(Name,Color,Fill,Grid,GrowthChart):-
   l_shape(Name,Ascii),
   ascii_to_growthchart(Ascii,GrowthChart),
   growthchart_to_grid(GrowthChart,Color,Fill,GridIn),
   to_real_grid(GridIn,Grid),
   \+ \+ ((nop((
     Color = green, Fill = red,        
     grid_size(Grid,H,V),
     print_grid(H,V,Grid),     
     wqnl(learned(Name)))))).

%learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).

 

:- include(kaggle_arc_footer).

