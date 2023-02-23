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
    saveable_test_info(TestID,TestInfo),  length(TestInfo,TILen),
    if_t(TILen < 100, maplist(print_rule(TestID),TestInfo)),
    training_info(TestID,Learned), maplist(print_rule(TestID),Learned), length(Learned,LLen),
    ptc(orange,format('~N~n% Observed: ~w  Learned: ~w~n~n',[TILen,LLen])),!,
    if_t((TILen==0,LLen==0),xlisting([TestID-(cached_tests)])).




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



p_g_s(G,O):- term_is_ansi(G),G=O.
p_g_s(I,O):- number(I), !, wots(O,write(I)).
p_g_s(I,O):- is_grid(I), I=O,!.
%p_g_s(I,O):- is_grid(I), wots(O,print_grid(I)). %orpt2(P):- write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(s_p_g),portray(true)]),!. %orpt(G):- orpt2(G),!.
s_p_g(I,_):- is_grid(I), print_grid(I),!.
s_p_g(G,_):- term_contains_ansi(G),!,write(G),!.
s_p_g(G,_):- term_is_ansi(G),!,write(G),!. 
%s_p_g(G,_):- print_term(G,[write_options([ numbervars(true),quoted(true),portray(true)])]),!. %s_p_g(I,_):- flag(orpt2,X,X),X<2,setup_call_cleanup(flag(orpt2,X,X+1),orpt2(I),flag(orpt2,_,X)),!.
%orpt1(G):- write(G),!.

orpt(G):- sub_term(E,G),compound(E),E=debug_var(N,V),!,subst(G,E,was_debug_var(N,V),GG),subst(GG,V,'$VAR'(N),GGG),orpt(GGG).
orpt(G):- !, \+ \+ ((numbervars(G,0,_,[attvar(bind),singletons(true)]), nl_if_needed, ignore(wcg(orange,G)))).

%wcg(_,G):- pp_wcg(G),!.
wcg(O,G):- pp(O,(call(pp_wcg(G)))),!.



%orpt(P):- colorize_oterms(p_g_s,P,G), format('~N',[]), print_term(G,[write_options([ numbervars(true),quoted(false),portray(true),portray_goal(user:s_p_g)])]),!.

save_learnt_rule(TestID,In,InKey,RuleDir,Out):-
  if_learn_ok(save_learnt_rule(learnt_rule(TestID,In,InKey,RuleDir,Out))).

save_learnt_rule(RuleIn):- save_learnt_rule(RuleIn,RuleIn,RuleIn).
save_learnt_rule(RuleIn,InGoal,OutGoal):-  
  labels_for(InGoal,OutGoal,InSet),
  length(InSet,InLen),length(InVars,InLen),
  subst_rvars(InSet,InVars,RuleIn,NewRuleIn),!,
  Assert = (NewRuleIn:-was_once(InSet,InVars)), 
  writeq(was_once(InSet,InVars)),
  assert_visually(Assert),!.    


learn_group(What,Objs):- assert_visually(group_associatable(What,Objs)).

learn_about_group(In):- 
  forall(group_keys(What),
   (group_group(What,In,Groups),
    maplist(learn_group(What),Groups))).

not_for_matching(_Why,_,Var):- var(Var),!,fail.
not_for_matching(_Why,_,C):- not_used(C),!.
not_for_matching(_Why,_,C):- for_matching(C),!,fail.
not_for_matching(_Why,_,giz(_)).


%not_for_matching(_Why,_,_):-!.

/*
not_for_matching(_Why,_,C):- notrace((sub_term(E,C), compound(E))), E= '$VAR'(_),!,fail.
not_for_matching(_Why,_,iz(info(combined))).
not_for_matching(_Why,_,iz(colormass)).
not_for_matching(_Why,_,iz(nsew)).
not_for_matching(_Why,_,iz(media(image))).
not_for_matching(_Why,_,iz(monochrome)).
not_for_matching(_Why,_,iz(C)):- atom(C),!,fail.
%not_for_matching(_Why,_,iz(C)):- sub_term(E,C), number(E),E\==1.
not_for_matching(_Why,_,iz(_)):- !, fail.
%not_for_matching(_Why,points_rep(local,_)).
not_for_matching(_Why,_,/*b*/iz(_)).
not_for_matching(_Why,_,obj_to_oid(_,_)).
%not_for_matching(_Why,L,form(_)):- !, member(points_rep(local,_),L).
not_for_matching(_Why,L,points_rep(local,XX)):- !, started_is_list(XX), member(shape_rep(grav,_),L).
not_for_matching(_Why,L,globalpoints(XX)):- !, started_is_list(XX), (member(shape_rep(grav,_),L);member(points_rep(local,_),L)).

%not_for_matching(_Why,_,center2G(H,V)):- (H\==1,V\==1,H\==2,V\==2,H\==3,V\==3).
%not_for_matching(_Why,_,loc2D(H,V)):- (H\==1;V\==1).
%not_for_matching(_Why,_,M):- too_unique(M),!.
%not_for_matching(_Why,_,M):- too_non_unique(M),!.
*/

propagate(I):- ( \+ callable(I); I='$VAR'(_)), !.
propagate(rot2D(_Rot90)).
propagate(vis2D(_H1,_V3)).
propagate(center2G(_,_)).
propagate(loc2D(_X2D,_Y1D)).
%propagate(pen([cc(_SILVER,_N3)|_More])).
propagate(pen(_)).
propagate(grid_rep(norm,_NormalGrid)).
propagate(grid_ops(norm,_UnrotateList)).
propagate(rotSize2D(grav,_O3,_O1)).
propagate(iz(sid(_Sid_13))).
propagate(loc2G(_X2G,_Y1G)).

secondary_verification(A):- var(A),!,fail.
secondary_verification(cc(_,0)).
secondary_verification(pg(_OG,_,_,_)).
secondary_verification(iz(A)):- \+ not_secondary_verification(A).

not_secondary_verification(A):- var(A).
not_secondary_verification(media(image)).
not_secondary_verification(symmetry_type(_)).

started_is_list(X):- nonvar(X), X = [_,_].

not_used(Var):- var(Var),!,fail.
not_used(/*b*/iz(indiv(_))).
%not_used(X):- sub_term(E,X),atom(E),is_nc_point(E),!.
not_used(X):- sub_term(E,X),compound(E),ground(E),E=info(_).
not_used(giz(_)).
%not_used(link(_,_,_)).
%not_used(link(_,_)).
%not_used(iz(contained_by(0,[]))).
not_used(shape_rep(grav,_)).
not_used(shape_rep(grav,_,_)).
%not_used(points_rep(local,_)).
not_used(globalpoints(_)).
%not_used(pg(OG,_,_,_)).
not_used(iz(X)):- not_used(X).

must_use(Var):- var(Var),!,fail.
must_use(/*b*/iz(indiv(i_diag))).

% show_safe_assumed_mapped

must_use(link(NotSees,_)):- NotSees\=sees(_).
%must_use(link(_,_,_)).
must_use(grid_rep(norm,_)).
must_use(grid_ops(norm,_)).
must_use(was_oid(_)).
must_use(sid(_)).
%must_use(/*b*/iz(Atom)):- !, atom(Atom).
must_use(shape(Atom)):- !, atom(Atom).
must_use(iz(X)):- must_use(X).

% when you have I you wont need II for creating
not_for_creating(_I,II):- for_creating(II),!,fail.
%not_for_creating(I,II):-not_for_matching(create,I,II),!.
not_for_creating( _,_).

for_creating(I):- ( \+ callable(I); I='$VAR'(_)), !.

%for_creating(NoUse):- not_used(NoUse),!,fail.
for_creating(rot2D(_Rot90)).
for_creating(vis2D(_H1,_V3)).
for_creating(center2G(_,_)).
for_creating(loc2D(_X2D,_Y1D)).
%propagate(pen([cc(_SILVER,_N3)|_More])).
for_creating(pen(_)).
for_creating(iz(Atom)):- atom(Atom).
for_creating(grid_rep(norm,_NormalGrid)).
for_creating(grid_ops(norm,_UnrotateList)).
for_creating(rotSize2D(grav,_O3,_O1)).
for_creating(iz(sid(_Sid_13))).
for_creating(loc2G(_X2G,_Y1G)).

for_creating(P):- for_creating1(P),!.
for_creating(NoUse):- must_use(NoUse),!.
for_creating(iz(X)):- !, for_creating1(X).


%for_creating1(colorlesspoints). for_creating1(mass). 
%for_creating1(info). 
for_creating1(I):- ( \+ callable(I); I='$VAR'(_)), !.
for_creating1(P):- propagate(P),!.
for_creating1(giz(W)):- for_creating1(W).
for_creating1(thr(_)).
for_creating1(oi(W)):- for_creating1(W).
for_creating1(oi(W,_)):- for_creating1(W).


for_creating1(oi(_)).
for_creating1(sid(_)).
for_creating1(norm_grid).
for_creating1(norm_ops).
for_creating1(loc2G).
for_creating1(center2G).
for_creating1(center2D).
for_creating1(loc2D).
for_creating1(grid_props). %for_creating1(cc).
for_creating1(grid_size).
for_creating1(pen).  
for_creating1(sid).
for_creating1(P):- \+ compound(P),!,fail.
%for_creating1(P):- functor(P,2,_),!.
for_creating1(P):- functor(P,What,_),for_creating1(What).
for_creating1(NoUse):- must_use(NoUse),!.
for_creating1(NoUse):- not_used(NoUse),!,fail.


for_matching(I):- ( \+ callable(I); I='$VAR'(_)), !.
%for_matching(pg(OG,_,nthOf(_),rank1(_))):-!,fail.
%for_matching(pg(OG,_,nthOf(_),rank1(_))):-!,fail.
%for_matching(pg(OG,_,nthOf(_),rank2(_))):-!,fail. 
for_matching(pg(_OG,_,_,rankA(_))).
for_matching(pg(_OG,_,_,rank1(_))).
%for_matching(pg(OG,_,_,rank2(_))).
for_matching(pg(_OG,_,_,F)):-for_matching(F),!.
for_matching(P):- for_matching1(P),!.
for_matching(X):- for_creating(X),!.
for_matching(iz(X)):- !, (atom(X);for_matching1(X)),!.

for_matching1(NoUse):- not_used(NoUse),!,fail.
for_matching1(mass).
for_matching1(cc). for_matching1(symmetry_type). 
%for_matching1(/*b*/iz). 

for_matching1(P):- \+ compound(P),!,fail.
for_matching1(P):- functor(P,2,_),!.
for_matching1(P):- functor(P,What,_),for_matching1(What).




simplify_for_creating(I,I):- ( \+ compound(I); I='$VAR'(_)), !.
simplify_for_creating(I,O):- is_grid(I),!,O=I.
simplify_for_creating(I,O):- is_list(I),my_exclude(not_for_creating(I),I,M),!,maplist(simplify_for_creating,M,O).
simplify_for_creating(I,O):- 
       compound_name_arguments(I, F, Args),
       maplist(skip_sub_list(simplify_for_creating), Args, ArgsNew),
       compound_name_arguments( O, F, ArgsNew ),!.
  
skip_sub_list(_, I,O):- is_list(I),!,O=I.
skip_sub_list(P2,I,O):- call(P2,I,O).

my_exclude(P1,I,O):- my_partition(P1,I,_,O).
simplify_for_matching(_Why,I,O):- var(I),O=I.
simplify_for_matching(rhs,I,O):-!,simplify_for_creating(I,O).
simplify_for_matching(_Why,I,I):- ( \+ compound(I); I='$VAR'(_)), !.
simplify_for_matching(_Why,I,O):- is_grid(I),!,O=I.
simplify_for_matching(Why,I,O):- is_list(I), my_exclude(not_for_matching(Why,I),I,M),I\=@=M,!,simplify_for_matching(Why,M,O).
simplify_for_matching(Why,I,O):- 
       compound_name_arguments(I, F, Args),
       maplist(skip_sub_list(simplify_for_matching(Why)), Args, ArgsNew),
       compound_name_arguments( O, F, ArgsNew ),!.

member_skip_open_(_, El, El).
member_skip_open_([H|T], El, _) :- nonvar(T),
    member_skip_open_(T, El, H).

member_skip_open(El, [H|T]) :-
    member_skip_open_(T, El, H).


group_group(What,[Obj|In],[G1|Groups]):- indv_props_list(Obj,Props), member_skip_open(P,Props),matches_key(P,What),!,
  [Obj|Include] = G1,
  my_partition(has_prop(P),In,Include,Exclude),
  group_group(What,Exclude,Groups).
group_group(_,_,[]).

group_keys(iz).
group_keys(mass).
group_keys(/*b*/iz).
group_keys(color).
%group_keys(colorlesspoints).
group_keys(rot2D).
group_keys(localpoints).
group_keys(symmetry_type).

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

learn_rule_o(Mode,InVM,OutVM):- is_vm_map(InVM),is_vm_map(OutVM),!,
 in_out = Mode,
 maplist(must_det_ll,[
  InGrid = InVM.grid_o, InObjsOriginal = InVM.objs,  
  OutGrid = OutVM.grid_o, OutObjsOriginal = OutVM.objs,
  ignore(InVM.grid_target = OutGrid),
  /*
  learn_rule_o(Mode,InObjsOriginal,OutObjsOriginal).

learn_rule_o(Mode,InObjsOriginal,OutObjsOriginal):-
  into_grid(InObjsOriginal,InGrid),
  into_grid(OutObjsOriginal,OutGrid),*/
  maplist(simplify_for_matching(lhs),InObjsOriginal,InObjs),
  maplist(simplify_for_matching(rhs),OutObjsOriginal,OutObjs),
 % OutObjsOriginal=OutObjs,
 % extract_vm_props(InVM,InProps),     
 % extract_vm_props(OutVM,OutProps), 
 % prolog_pretty_print:print_term(Mode=learn_rule_o(Mode,InProps,OutProps),[fullstop(true),nl(true)]),!,
  %learn_grid_rules(Mode,InProps),
  %learn_grid_rules(Mode,OutProps),!,
  learn_rule_i_o(Mode,InObjsOriginal,OutObjsOriginal),
  %learn_rule_i_o(Mode,InGrid,OutObjs),
  %learn_rule_i_o(Mode,InObjs,OutGrid),
  %learn_rule_i_o(Mode,InGrid,OutGrid),
  get_current_test(TestID),
  training_info(TestID,Info),
  length(Info,Len),
  ptc(orange,format('~N~n% Rules so far for ~w: ~w~n~n',[TestID,Len])),!,
  if_learn_ok(confirm_reproduction(InObjs,InObjsOriginal,InGrid)),!,
  if_learn_ok(confirm_reproduction(OutObjs,OutObjsOriginal,OutGrid)),!,
  confirm_learned(InGrid,OutGrid),!,
  nop(show_proof(InGrid,OutGrid))]),!.
  

is_reproduction_obj(O):- \+ is_object(O),!.
is_reproduction_obj(O):-  \+ iz(O,info(hidden)).

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
  call((points_to_grid(H,V,OGPoints,Sols)->true;points_to_grid(DH,DV,OGPoints,Sols))),
  count_difs(ExpectedOut,Sols,Errors),
  show_result("Our Reproduction"=Len0/Len, Sols,ExpectedOut,Errors),
  (Errors==0 -> true; maplist(debug_reproduction(H,V),Objs,DebugObjs)))),!.

debug_reproduction(H,V,Obj,DObj):- 
 must_det_ll((
  globalpoints(Obj,Points),
  tersify(Obj,Info),
  print_grid(H,V,Info,Points),
  obj_to_oid(Obj,ID1),
  obj_to_oid(DObj,ID2),
  pp(dobj(ID1,ID2)=DObj))),!.


show_result(What,Sols,ExpectedOut,Errors):-
  show_sameness_or_lameness(green,red,What,Sols,ExpectedOut,Errors).

show_sameness_or_lameness(Green,Red,What,OurOut,ExpectedOut,Errors):- 
   ignore(( get_current_test(TestID), test_info(TestID,InfoF))), !,
   ignore(( (var(Errors)->count_difs(OurOut,ExpectedOut,Errors);true),
    (Errors==0 -> 
      ColorMessage=wqs(Green,pass(What,TestID));
      ColorMessage=wqs(Red,fail(errors(Errors),What,TestID))),
    ColorMessage = wqs(Color,Message),    
    banner_grids(Color,OurOut,Message,ExpectedOut,fav(TestID,InfoF)))).

banner_grids(Color,I,Message1,O,Message2):- 
 ignore((
    banner_lines(Color),
    print_side_by_side(Color,I,Message1,_,O,Message2),
    nl_if_needed,pp(Color,Message1),
    nl_if_needed,pp(Color,Message2),
    nl_if_needed,
    banner_lines(Color))).

arcdbg_info(Color, Info):- banner_lines(Color), arcdbg(Info), banner_lines(Color).

confirm_learned(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (     use_test_associatable_group(Objs,Sols) -> 
   show_result("Our Learned Sols", Sols,ExpectedOut,_)
   ; arcdbg_info(red,warn("No Learned Sols"))).


show_proof(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (test_associatable_proof(Objs,Sols) -> 
   show_result("Our Proved Sols", Sols,ExpectedOut,_Errors)
   ; arcdbg_info(red,warn("No Proved Sols"))).

%learn_rule_o(_Mode,G,_):- is_list(G), is_group(G), learn_about_group(G), fail.
%learn_rule_o(_Mode,_,G):- is_list(G), is_group(G), learn_about_group(G), fail.



%learn_rule_in_out(Depth,out_out,I,O):- !, ignore(( is_grid(O), save_learnt_rule(oout_associatable(I,O)))).

compare_objs_how([perfect]).
compare_objs_how([turned,+loc2D]).
compare_objs_how([turned,-loc2D]).
compare_objs_how([moved]).
compare_objs_how([sameO]).
compare_objs_how(_).

/*
vis2D(5,5), mass(25),
center2G(9,14),loc2D(7,12),
colors_cc([cc(PURPLE,21),cc(BLACK,4)]),
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
% filltype(solid) object
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

keep_code(call(true)):-!,fail.
keep_code(_).

arrange_shared(Var,Var):-var(Var),!.
arrange_shared(List,O):- is_list(List),!,include(keep_code,List,List1),maplist(arrange_shared,List1,O).
arrange_shared(level(_,O),O):- !, arrange_shared(O,O).
arrange_shared(level(_,F,O),F=O):- !, arrange_shared(O,O).
arrange_shared(O,O).

is_original_value(Var):- number(Var),!.
is_original_value(Var):- nonvar(Var), \+ aliased_valued(Var).

aliased_valued(Var):- string(Var),!. 
aliased_valued(Var):- var(Var),!.  
aliased_valued('$VAR'(_)). aliased_valued('VAR'(_)). aliased_valued('$'(_)).

is_all_original_value(Var):- nonvar(Var), \+ non_original_value(Var).
non_original_value(Var):- sub_term(E,Var), compound(E), E='$VAR'(_). 



:- dynamic(arc_cache:doing_map/4).

learn_rule_i_o(Mode,In,Out):- 
  forall(learn_rule_in_out(Mode,In,Out),true).

:- discontiguous(learn_rule_in_out/4).
/*
learn_rule_in_out(Depth,Mode,In,Out):- is_list(In), is_list(Out), 
  \+ is_grid(In), \+ is_grid(Out),
  \+ is_group(In), \+ is_group(Out),
  forall(select_some(I,In,MoreIn),
   forall(select_some(O,Out,MoreOut),
    (learn_rule_in_out(Depth,Mode,MoreIn,MoreOut),
     learn_rule_in_out(Depth,Mode,I,O)))).
*/
%learn_rule_in_out(_,in_out,In,Out):- is_list(In),is_list(Out),
%  (learn_rule_in_out_objects(in_out,In,Out), deterministic(TF), true), (TF==true -> !; true).
learn_rule_in_out(Mode,In,Out):- is_list(In),is_list(Out),!,
  % average_or_mid(mass,Out,MinMass),  
  length(In,IL),length(Out,OL),
  forall(nth1(II,In,I),forall(nth1(OO,Out,O),  
  %mass(O,Mass), Mass>MinMass, mass(I,Mass),
  ((ignore((once((compare_objs_how(Mode), nonvar(Mode), compare_objs1(Mode,I,O))),pp(Mode))),
  locally(nb_setval(rule_note,rule(II/IL,OO/OL)),
    learn_rule_in_out(Mode,I,O)))))).

learn_rule_in_out(Mode,I,O):- 
  arc_assert(need_save_rule1(Mode,"INPUT <--> OUTPUT",I,O)).

sort_by_closeness(In,Objs,List):- sorted_by_closeness(In,_Sorted,Objs,List).

enum_in_objs(In,Objs):- nonvar(In),nonvar(Objs),!.
enum_in_objs(In,Objs):- var(In),see_object_atomslist(_,In,_,_),enum_in_objs(In,Objs).
%enum_in_objs(In,Objs):- is_list(Objs),var(In),select(In,Objs,Rest),Rest\==[],!, enum_in_objs(In,Objs).
enum_in_objs(In,Objs):- var(Objs),!, enum_groups(Objs),enum_in_objs(In,Objs).
enum_in_objs(In,Objs):- is_list(Objs),var(In),member(Obj,Objs),has_prop(giz(g(IO_DIR)),Obj), in_to_out(IO_DIR,OI), prop_group(giz(g(OI)),In),enum_in_objs(In,Objs).
enum_in_objs(In,Objs):- var(Objs),nonvar(In),in_to_out(IO_DIR,OI),has_prop(giz(g(IO_DIR)),In), 
   prop_group(giz(g(OI)),Objs),Objs\==[],!,enum_in_objs(In,Objs).

:- dynamic(saved_sorted_by_closeness/4).
sorted_by_closeness(In,Sorted,Objs,List):- once(var(In);var(Objs)),!,enum_in_objs(In,Objs), sorted_by_closeness(In,Sorted,Objs,List),List\==[].
sorted_by_closeness(In,Sorted,Objs,List):- var(Sorted), maplist(obj_to_oid,Objs,OIDS), sort_safe(OIDS,Sorted),!,sorted_by_closeness(In,Sorted,Objs,List).
sorted_by_closeness(In,Sorted,Objs,List):- saved_sorted_by_closeness(In,Sorted,Objs,List),!.
sorted_by_closeness(In,Sorted,Objs,List):- 
  find_prox_mappings(In,_,Objs,List),
  asserta(saved_sorted_by_closeness(In,Sorted,Objs,List)),!.


find_prox_mappings(A,GID,Candidates,Objs):-
    obj_grp_atomslist(GID,A,PA,PAP),
    ord(NJ/O+JO+Joins,[PA,A],[PB,B],B) = Why,
    findall(Why,
    (      
     member(B,Candidates),obj_grp_atomslist(GID,B,PB,PBP),
     PA\==PB,
     B\==A,
     \+ is_whole_grid(B),
     must_det_ll((
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
       intersection(PAP,PBP,Joins,OtherA,OtherB),     
       flatten([OtherA,OtherB],Other),
       length(Joins,J),length(Other,O),
       NJ is -J,
       JO is - rationalize(J/(O+1))))),
     Pairs), 
   sort_safe(Pairs,RPairs),!,
   %list_upto(3,RPairs,Some),
   maplist(arg(4),RPairs,Objs).
/*
find_prox_mappings(A,GID,Objs):-
    obj_grp_atomslist(_,A,PA,PAP),
    ord(NJ/O+JO+Joins,[PA,A],[PB,B],B) = Why,
    findall(Why,
    (      
     obj_grp_atomslist(GID,B,PB,PBP),
     PA\==PB,
     B\==A,
     \+ is_whole_grid(B),
     must_det_ll((
     % maybe_allow_pair(PA,PB), allow_pair(PA,PB),  
       intersection(PAP,PBP,Joins,OtherA,OtherB),     
       flatten([OtherA,OtherB],Other),
       length(Joins,J),length(Other,O),
       NJ is -J,
       JO is - rationalize(J/(O+1))))),
     Pairs), 
   sort_safe(Pairs,RPairs),!,
   %list_upto(3,RPairs,Some),
   maplist(arg(4),RPairs,Objs).
*/


p2_call_p2(P2a,P2b,A,B):- call(P2a,A,M),call(P2b,M,B).

maybe_exclude_whole([I],[I]).
maybe_exclude_whole(I,I):- \+ (member(Obj,I), is_fg_object(Obj), \+ is_whole_grid(Obj)),!.
maybe_exclude_whole(I,II):- 
  include(not_p1(is_whole_grid),I,II).

:- dynamic(arc_cache:showed_point_mapping/3).

var_or_globalpoints(IP,[]):- var(IP),!.
var_or_globalpoints(IP,GP):- globalpoints(IP,GP).

showed_mapping(IP,OP):-
   var_or_globalpoints(IP,PsI),
   var_or_globalpoints(OP,PsO),!,
   maplist(showed_point_mapping_ok(in),PsI),
   maplist(showed_point_mapping_ok(out),PsO),!.

assert_showed_mapping(IP,OP):-
  %pp_wcg(assert_showed_mapping(IP,OP)),
   must_det_ll((
   var_or_globalpoints(IP,PsI),
   var_or_globalpoints(OP,PsO),
   maplist(assert_showed_point_mapping(in),PsI),
   maplist(assert_showed_point_mapping(out),PsO))),!.

assert_showed_point_mapping(IO_DIR,Ps):- \+ \+ showed_point_mapping_ok(IO_DIR,Ps),!.
assert_showed_point_mapping(IO_DIR,Ps):- assert_in_testid(arc_cache:showed_point_mapping(IO_DIR,Ps)).

showed_point_mapping_ok(_,Black-_):- nonvar(Black), \+ Black \= black,!.
showed_point_mapping_ok(_,BGC-_):- \+ plain_var(BGC), \+ is_fg_color(BGC),!.
showed_point_mapping_ok(IO_DIR,Ps):-  call_in_testid(arc_cache:showed_point_mapping(IO_DIR,Ps)),!.

special_properties(I,O):- findall(SP,(sub_term(E,I), compound(E), into_special_props(E,SP)),L),flatten([L],F),list_to_set(F,O).
into_special_props(cc(C,N),[color(C),cc(C,N)]):- number(N), N > 0, is_real_fg_color(C),!.

save_rule(GID,TITLE,IP,OP):-  list_to_set(IP,IIP), list_to_set(OP,OOP),!, save_rule0(GID,TITLE,IIP,OOP).

save_rule0(_GID,_TITLE,IP,OP):- sort_safe(IP,I),sort_safe(OP,O), showed_mapping(I,O),!.
save_rule0(GID,TITLE1,IP,OP):-
 must_det_ll((
 length(IP,LI), length(OP,LO), sformat(TITLE,"(~w) ~w (~w)",[LI,TITLE1,LO]),
 pp_wcg(TITLE),
 save_rule00(GID,TITLE,IP,OP))).


reachable_a(IP,_OP,A):- member(FF,IP),doing_map_list(_,FF,List),member(A,List).
reachable_a(_IP,OP,A):- member(FF,OP),doing_map_list(_,FF,List),member(A,List).


input_atoms_list(List):- 
 findall(PAP,obj_grp_atomslist(in,_,_,PAP),ListList),flatten(ListList,List).

save_rule00(GID,TITLE,IP,OP):-
  save_rule1(GID,TITLE,IP,OP). /*
save_rule00(GID,TITLE,IP,OP):-
 must_det_ll((
 special_properties(IIPP,SPI),
 special_properties(OOPP,SPO),
 intersection(SPI,SPO,_Shared,_Extra,Missing),
 input_atoms_list(InputList),
 intersection(InputList,Missing,ShouldFind,_,_Unneeded),
 ((ShouldFind==[];nb_current(rule_cannot_add_more_objects,t)) -> save_rule1(GID,TITLE,IP,OP)
 ;(
   call(call(call),((reachable_a(IP,OP,A), 
   obj_grp_atomslist(in,A,_PA,PAP),
   member(S,ShouldFind),
   member(S,PAP)))),
   save_rule(GID,TITLE,[A|IP],OP))))).
*/

is_obj_props(Props):- is_list(Props), Props\==[], \+ is_grid(Props), \+ is_group(Props), \+ is_points_list(Props).

%extend_grp_proplist(Grp,GrpO):- Grp==[],!,GrpO=[].
extend_grp_proplist(Grp,GrpO):- mapgroup(extend_obj_proplist(Grp),Grp,GrpO).

%extend_obj_proplist(Var,NewObj):- var(Var),!, enum_object(Var),extend_grp_proplist(Var,NewObj).
extend_obj_proplist(Grp,obj(Obj),obj(OUT)):-!, extend_obj_proplist1(Grp,Obj,OUT).
extend_obj_proplist(Grp,L1,L2):- extend_obj_proplist1(Grp,L1,L2).

extend_obj_proplist1(Grp,Props,OUTL):- must_det_ll(is_obj_props(Props)),
  Obj = obj(Props),
  findall(P,extend_obj_prop(Grp,Obj,P),NewProps),
  flatten(NewProps,NewPropsF),
  override_object(NewPropsF,Props,Obj1),
  override_object(Props,Obj1,OUT),
  into_obj_plist(OUT,OUTL).

extend_obj_prop(Grp,Obj,Prop):- is_in_subgroup(Grp,Obj,Prop).
extend_obj_prop(_Grp,obj(List),Prop):- algo_list(Algo),
 once((\+ member(grid_rep(Algo,_),List),
  object_grid(obj(List),Grid),
  algo_ops_grid(Algo,NormOps,Grid,NormGrid),
  points_rep(local,NormGrid,NormLPoints),maplist(arg(2),NormLPoints,ShapeNormLPoints),  
  shape_id(ShapeNormLPoints,NormShapeID))),
  member(Prop,[grid_ops(Algo,NormOps),iz(algo_sid(Algo,NormShapeID)),grid_rep(Algo,NormGrid)]).

extend_obj_prop(_Grp,Obj,Props):- fail,
 once((points_rep(local,Obj,P),vis2D(Obj,H,V),points_to_grid(H,V,P,Grid),
  grid_props(Grid,Props))).



%  print_g1(P2,CG):- arc_html,with_toplevel_pp(ansi,( \+ arc_html,print_g1(P2,CG))),!.
show_changes(P2BeforeAfter):- P2BeforeAfter=..[_,Before,After],
 must_det_ll((
  call(P2BeforeAfter),
  show_changes(Before,After))).
show_changes(P3BeforeAfterRem):- 
 must_det_ll(( 
 P3BeforeAfterRem=..[_,_,Retained,Removed|_],
 call(P3BeforeAfterRem),
 show_elem_changes_rr(Retained,Removed))).


show_changes(Before,After):-
 intersection(Before,After,Retained,Removed,Added), 
 if_t(Added\==[],(dash_chars,print_ss(added=Added),dash_chars)),
 show_elem_changes_rr(Retained,Removed),
 !.

show_elem_changes_rr(Retained,Removed):-
 %if_t(Removed==[], print_ss(keptAll=Retained)),
 if_t(Removed\==[],
   (print_ss(retained=Retained),
    dash_chars,
    print_ss(removed=Removed))),
 dash_chars('^').


fix_groups(AG00,BG00,AG,BG):- 
  maybe_fix_group(AG00,AG),
  maybe_fix_group(BG00,BG).


maybe_fix_group(I,OO):-
  extend_grp_proplist(I,AG0),
  predsort(sort_on(mapping_order),AG0,AG01),
  remove_singles_unneeded(AG01,AG1),
  maybe_exclude_whole(AG1,AG2),
  filter_redundant(AG2,AG),
  intersection(AG0,AG,Retained,_Removed,_Added),
  (Retained==[]-> OO = AG1 ; OO = AG1).  % yeah for now doesnt change anything

fix_group(AG00,AG):- 
  extend_grp_proplist(AG00,AG0),  
  predsort(sort_on(mapping_order),AG0,AG1),
  maybe_exclude_whole(AG1,AG2),
 filter_redundant(AG2,AG).
 
learn_group_mapping1(AG00,BG00):-   
  fix_groups(AG00,BG00,AG,BG),
  %forall(member(A,AG), forall(member(B,BG), save_rule2(in,"each I<-->O",A,B))),
  save_rule2(in,"all I<-->O",AG,BG),!,  
  other_io(IO_DIR,OI),

  maplist(obj_grp_atoms(IO_DIR),AG,_AGG),

  maplist(obj_grp_atoms(OI),BG,_BGG),

  forall(member(B,BG),
    ((find_prox_mappings(B,OI,AG,Objs),
     save_rule1(OI,"ordered In <- Out", Objs,[B])))),

  forall(member(A,AG),
    ((find_prox_mappings(A,IO_DIR,BG,Objs),
     save_rule1(IO_DIR,"In -> ordered Out",[A],Objs)))).

     


learn_group_mapping(AG00,BG00):-
  %ignore(learn_group_mapping1(AG00,BG00)),
  fix_groups(AG00,BG00,AG,BG),
 
 learn_rule_i_o(in_out,AG00,BG00),

 must_det_ll((  
  other_io(IO_DIR,OI),

  maplist(obj_grp_atoms(IO_DIR),AG,_AGG),

  maplist(obj_grp_atoms(OI),BG,_BGG))),

  forall(member(B,BG),
    ((find_prox_mappings(B,OI,AG,Objs),
     assert_doing_map(OI,B,Objs)))),

  forall(member(A,AG),
    ((find_prox_mappings(A,IO_DIR,BG,Objs),
     assert_doing_map(IO_DIR,A,Objs)))),

  must_det_ll((  
  abolish(arc_cache:showed_point_mapping/3),dynamic(arc_cache:showed_point_mapping/3),

  learn_group_mapping_p3(IO_DIR,OI,AG,BG))).

learn_group_mapping_p3(IO_DIR,OI,AG,BG):- !,
 locally(nb_setval(rule_cannot_add_more_objects,t),
  (nb_linkval(in_out_pair,in_out_pair(AG,BG,shared)),
   ((  
    length(BG,BL),length(AG,AL),
    forall(nth1(BB,BG,B),
       (doing_map_list(OI,B,[A|Others]), nth1(AA,AG,A),
         locally(nb_setval(rule_note,rule(AA/AL,BB/BL)),
         
         (has_prop(iz(sid(SA)),A),
          (once((fail, member(C,Others), has_prop(iz(sid(SA)),C)))->CC=[C];CC=[]),
           assertz_in_testid(arc_cache:assumed_mapped([A|CC],[B])),
          % save_learnt_rule(arc_cache:assumed_mapped(lhs([A|CC]),rhs([B])),A^B,A^B),
           save_rule1(OI,"INPUT <-- OUTPUT",[A|CC],[B]))))),
    
    forall(nth1(AA,AG,A),
       (doing_map_list(IO_DIR,A,[B|Others]), nth1(BB,BG,B),
         locally(nb_setval(rule_note,rule(AA/AL,BB/BL)),
          (has_prop(iz(sid(SA)),A),
          (once((fail, member(C,Others), has_prop(iz(sid(SA)),C)))->CC=[C];CC=[]),
           assertz_in_testid(arc_cache:assumed_mapped([A],[B|CC])),
           % save_learnt_rule(arc_cache:assumed_mapped(lhs([A]),rhs([B|CC])),A^B,A^B),
           save_rule1(IO_DIR,"INPUT --> OUTPUT",[A],[B|CC]))))),

   !)))).
/*
learn_group_mapping_p3(IO_DIR,OI,AG,BG,AGS,BGS):- !,
 locally(nb_setval(rule_cannot_add_more_objects,t),
  (nb_linkval(in_out_pair,in_out_pair(AG,BG,shared)),
   must_det_ll((  
    
    forall(member(B,BGS),
       (doing_map_list(OI,B,[A|Others]),
          has_prop(iz(sid(SA)),A),
          (once((fail, member(C,Others), has_prop(iz(sid(SA)),C)))->CC=[C];CC=[]),
           assertz_in_testid(arc_cache:assumed_mapped([A|CC],[B])),
          % save_learnt_rule(arc_cache:assumed_mapped(lhs([A|CC]),rhs([B])),A^B,A^B),
           save_rule2(OI,"INPUT <-- OUTPUT",[A|CC],[B]))),
    forall(member(A,AGS),
       (doing_map_list(IO_DIR,A,[B|Others]),
          has_prop(iz(sid(SA)),A),
          (once((fail, member(C,Others), has_prop(iz(sid(SA)),C)))->CC=[C];CC=[]),
           assertz_in_testid(arc_cache:assumed_mapped([A],[B|CC])),
           % save_learnt_rule(arc_cache:assumed_mapped(lhs([A]),rhs([B|CC])),A^B,A^B),
           save_rule2(IO_DIR,"INPUT --> OUTPUT",[A],[B|CC]))),
   !)))).*/


try_using_training(In,ExpectedOut):- 
   try_each_using_training(In,ExpectedOut,Rules,OurOut),
   count_difs(OurOut,ExpectedOut,Errors),
   (Errors==0->pp_wcg(Rules);true).

try_each_using_training(In,ExpectedOut,Keeper,OurOut):-
   is_grid(In),individuate(complete,In,InC),!,
   must_det_ll(try_each_using_training(InC,ExpectedOut,Keeper,OurOut)),!.

try_each_using_training(In,ExpectedOut,Keeper,OurOut):- 
 must_det_ll((
  enum_ruleset(Rules), length(Rules,L),
  pp_wcg(rule_count=L),
  wno_must(try_each_using_training(In,ExpectedOut,Rules,Keeper,OurOut)))),!.

try_each_using_training(In,_ExpectedOut,Rules,Keeper,OurOut):- Rules==[], pp_wcg(Keeper=[no_rules]),!, o_globalpoints([],In,OurOut),!.
try_each_using_training(In,ExpectedOut,Rules,Keeper,OurOut):-
 must_det_ll((
  classify_rules(In,ExpectedOut,Rules,Keeper,Rejected,Unknown),
   maplist(length,[In,Rules,Keeper,Rejected,Unknown],[InC,RulesC|Nums]),
   Pos is InC*RulesC,
   pp_wcg(in_rules_times_keepers_rejected_unknown=[InC,RulesC,Pos|Nums]),!,
   %nop(pp_wcg(Awesome=Keeper)), nop(pp_wcg(rejected=Rejected)), nop(maplist(writeln,Unknown)), 
   wno_must(o_globalpoints(Keeper,GPs)),
   points_to_grid(GPs,OurOut))),!,
 print_grid(using_training,OurOut).


classify_rules(In,ExpectedOut,Rules,Keeper,Rejected,Unknown):- \+ is_points_list(ExpectedOut),!,
 must_det_ll((globalpoints(ExpectedOut,Out),ExpectedOut\=@=Out,
 classify_rules(In,Out,Rules,Keeper,Rejected,Unknown))).
classify_rules(In,ExpectedOut,Rules,Keeper,Rejected,Unknown):- \+ is_group(In),!,
 must_det_ll((is_grid(In),individuate(complete,In,InC),
   classify_rules_0(InC,ExpectedOut,Rules,Keeper,Rejected,Unknown))).
classify_rules(In,ExpectedOut,Rules,Keeper,Rejected,Unknown):-
  must_det_ll(classify_rules_0(In,ExpectedOut,Rules,Keeper,Rejected,Unknown)).

classify_rules_0(In,ExpectedOut,Rules,[PAIR],[],[]):- fail,
  select(I,In,_IIn), find_prox_mappings(I,ri,Rules,[R|_Sorted]),
  copy_term(R,RR),  
  PAIR = result(I,R,RR,GPs,Matches,untested),
  matches_input(I,RR,Matches),
  o_globalpoints(Matches,RR,GPs),
  gp_class(I,GPs,expected,ExpectedOut),
  nop((pp_wcg(matches(classify_rules_0)=Matches), pp_wcg(rule(classify_rules_0)=R))).

classify_rules_0(In,ExpectedOut,Rules,Keeper,Rejected,Unknown):-
  select(I,In,IIn), find_prox_mappings(I,ri,Rules,Sorted),!,
  classify_rules_1([I|IIn],ExpectedOut,Sorted,[],Keeper,Rejected,Unknown).

classify_rules_1(In,ExpectedOut,Rules,PairsUsed,Keeper,Rejected,Unknown):- 
  member(I,In), member(R,Rules),
  copy_term(R,RR),
  PAIR = result(I,R,RR,GPs,Matches,_TestedOrNot),
  \+ member(PAIR,PairsUsed),!,
  must_det_ll((
  once(((matches_input(I,RR,Matches) -> o_globalpoints(Matches,RR,GPs); Matches=[]))),
  classify_rules_2(PAIR,In,ExpectedOut,Rules,[PAIR|PairsUsed],Keeper,Rejected,Unknown))).
classify_rules_1(_,_,_,_,[],[],[]).

classify_rules_2(PAIR,In,ExpectedOut,Rules,PairsUsed,Keeper,Rejected,[PAIR|Unknown]):- 
  PAIR = result(I,Rule,_,GPs,Matches,_),
  (Matches==[]; \+ (sub_term(N,Matches),once(number(N);is_color(N)))),!,
  NotMatched = notMatched,
  must_det_ll((  
  banner_lines(orange,3),
  nb_setarg(6,PAIR,NotMatched),
  gp_class(I,GPs,NotMatched,ExpectedOut),
  pp_wcg(matches(NotMatched)=Matches),
  pp_wcg(rule(NotMatched)=Rule),
  classify_rules_1(In,ExpectedOut,Rules,[PAIR|PairsUsed],Keeper,Rejected,Unknown))).
classify_rules_2(PAIR,In,ExpectedOut,Rules,PairsUsed,Keeper,[PAIR|Rejected],Unknown):- 
  PAIR = result(I,Rule,_,GPs,Matches,_),
  \+ no_violation(GPs,ExpectedOut),!,
  nb_setarg(6,PAIR,violates),
  wqnl([violates=GPs,expected=ExpectedOut]),
  must_det_ll((
  once((
  banner_lines(red,3),
  gp_class(I,GPs,violates,ExpectedOut),
  wqnl(in(violates)=I),
  wqnl(rule(violates)=Rule),
  list_to_set(Matches,MatchesSet),ppt(matches(violates)=MatchesSet))),
  classify_rules_1(In,ExpectedOut,Rules,[PAIR|PairsUsed],Keeper,Rejected,Unknown))).

classify_rules_2(PAIR,In,ExpectedOut,Rules,PairsUsed,[PAIR|Keeper],Rejected,Unknown):- 
  must_det_ll((
  Awesome=awesome,
  PAIR = result(I,Rule,_,GPs,Matches,_),
  banner_lines(green,3),
  nb_setarg(6,PAIR,Awesome),
  gp_class(I,GPs,Awesome,ExpectedOut),
  pp(rule(Awesome)=Rule),
  ppt(matches(Awesome)=Matches),
  print(Matches),
  classify_rules_1(In,ExpectedOut,Rules,[PAIR|PairsUsed],Keeper,Rejected,Unknown))).

gp_class(I,GPs,Awesome,ExpectedOut):- 
  print_ss(in,I,print_ss(wqs(Awesome),GPs,ExpectedOut)).

%no_violation([],[]):-!.
no_violation(GPs,_):- GPs==[],!.
no_violation(_,Var):-var(Var),!.
no_violation(_,ExpectedOut):- ExpectedOut==[],!.
no_violation([C1-P1|GPs],ExpectedOut):- member(C2-P1,ExpectedOut),C1=@=C2,!,no_violation(GPs,ExpectedOut).
no_violation([C1-P1|GPs],ExpectedOut):- select(C2-P1,ExpectedOut,Rest),C1=@=C2,!,no_violation(GPs,Rest).
no_violation([C1-_|GPs],ExpectedOut):- var(C1),!, no_violation(GPs,ExpectedOut).
no_violation([C1-P1|GPs],ExpectedOut):- \+ member(_-P1,ExpectedOut),!,(C1==black;C1==bg;C1==wbg),!,no_violation(GPs,ExpectedOut).
no_violation([C1-P1|GPs],ExpectedOut):- member(C2-P1,ExpectedOut),var(C2),(C1==black;C1==bg;C1==wbg),!,no_violation(GPs,ExpectedOut).
no_violation([C1-P1|GPs],ExpectedOut):- member(C2-P1,ExpectedOut),(C1=@=C2),!,no_violation(GPs,ExpectedOut).
no_violation([_|GPs],ExpectedOut):- no_violation(GPs,ExpectedOut).
  
enum_ruleset(Rules):- 
  get_current_test(TestID),
  R = object_to_object(TestID,Name,LHS,Create,DebugInfo,DebugInfo2),
  findall(R,
     arc_cache:object_to_object(TestID,Name,LHS,Create,DebugInfo,DebugInfo2),Rules).



reduce_rules(lhs(LHS),Out):- !,reduce_rules(LHS,Out).
reduce_rules(rhs(LHS),Out):- !,reduce_rules(LHS,Out).
reduce_rules([LHS],Out):- !,reduce_rules(LHS,Out).
%reduce_rules(obj(LHS),Out):- !,reduce_rules(LHS,Out).
%reduce_rules(some(LHS),Out):- !, member(M,LHS),reduce_rules(M,Out).
reduce_rules(arc_cache:object_to_object(_TestID,_Name,LHS,_Create,_DebugInfo,_DebugInfo2),Out):- !,reduce_rules(LHS,Out).
reduce_rules(arc_cache:object_to_object(_TestID,_Name,LHS,_Create,_DebugInfo),Out):- !,reduce_rules(LHS,Out).
reduce_rules(O,O).

o_globalpoints(I,O):- o_globalpoints([],I,O),!.

o_globalpoints(Ctx,Var,Gps):- var(Var),!,o_globalpoints(Ctx,[],Gps).
o_globalpoints(Var,Was,Gps):- var(Var),!,o_globalpoints([],Was,Gps).
o_globalpoints(_Ctx,C,GPs):- is_points_list(C),!,GPs=C.
o_globalpoints(_Ctx,C,GPs):- is_grid(C),!,globalpoints(C,GPs).
o_globalpoints(_Ctx,C,GPs):- is_grid(C),!,GPs=C.
o_globalpoints(Ctx,[C],GPs):- nonvar(C), !, o_globalpoints(Ctx,C,GPs).
o_globalpoints(Ctx,C,GPsS):- is_list(C),!,must_det_ll((maplist(must_o_globalpoints(Ctx),C,GPs),append_sets(GPs,GPsS))).

o_globalpoints(Ctx,rhs(C),GPs):- !, must_det_ll(o_globalpoints(Ctx,C,GPs)).
o_globalpoints(Ctx,lhs(C),GPs):- !, must_det_ll(o_globalpoints(Ctx,C,GPs)).
o_globalpoints(Ctx,result(_,_,_,CCC,_),GPs):- o_globalpoints(Ctx,CCC,GPs),!.
o_globalpoints(Ctx,object_to_object(_TestID,_Name,_Match,Create,DebugInfo,_DebugInfo2),GPs):- !, must_det_ll((run_delayed_goals(DebugInfo),!,o_globalpoints(Ctx,Create,GPs))).
o_globalpoints(Ctx,obj(C) - Docalls,Grid):- !, must_det_ll(((run_delayed_goals(Docalls),!,o_globalpoints(Ctx,obj(C),Grid)))). 
o_globalpoints(Ctx,obj(C),Grid):- 
 maplist(reduce_prop,C,CC),!,must_det_ll(olg_globalpoints(Ctx,obj(CC),CC,Grid)). 


must_olg_globalpoints(A,B,C,D):- must_det_ll(olg_globalpoints(A,B,C,D)).
must_o_globalpoints(A,B,C):- must_det_ll(o_globalpoints(A,B,C)).

olg_globalpoints(_Matches,_Obj,L,GP):-
   member(globalpoints(GP),L).
olg_globalpoints(_Matches,_Obj,L,GP):-
   member(points_rep(local,LP),L),
   get_nw_absAt(L,X,Y),
   offset_points(X,Y,LP,GP).
olg_globalpoints(Matches,_Obj,L,GP):-
 must_det_ll((
   combine_props(L,Matches,LL),!,   
   member_prop(grid_rep(norm,NG),LL), 
   member_prop(grid_ops(norm,Ops),LL),
   maplist(writeln,[unreduce_grid=NG,ops=Ops]),
   BAD = once(var(NG);var(Ops); \+ can_do_ops(NG,Ops)))),
   if_t(BAD, wqnl(olg_globalpoints(LL))),!,
   \+ ( BAD ),
   unreduce_grid(NG,Ops,RR),!,
   must_det_ll((
   points_rep(local,RR,LP),   
   get_nw_absAt(LL,X,Y),
   offset_points(X,Y,LP,GP))).
   %olg_globalpoints(Matches,Obj,[points_rep(local,LP)|L],GP).

combine_props(L,Matches,LL):- flatten([L,Matches],LL).
member_prop(Prop,LL):- member(Prop,LL),arg(1,Prop,NV),nonvar(NV).
member_prop(Prop,LL):- sub_term(E,LL),compound(E),E=Prop,arg(1,Prop,NV),nonvar(NV),!.
member_prop(Prop,LL):- member(Prop,LL).

can_do_ops(NG,Ops):- wno_must(unreduce_grid(NG,Ops,G)),is_grid(G).

get_nw_absAt(L,X,Y):- make_loc2D(L,X,Y).
get_nw_absAt(L,X,Y):- make_center2D(L,CX,CY),number(CX),number(CY), make_vis2D(L,SX,SY), X is CX+floor(SX/2),Y is CY+floor(SY/2).

valid_xy(X,Y):- number(X),number(Y).

rescale(TY,SY,GY,Y):- TY==SY,!,GY=Y.
rescale(TY,SY,GY,Y):- Y is (TY/SY)*GY.
g_to_d(L,GX,GY,X,Y):- member(globalG(SX,SY),L),member(grid_size(TX,TY),L),rescale(TX,SX,GX,X),rescale(TY,SY,GY,Y).
g_to_d(_,X,Y,X,Y).

make_loc2D(L,X,Y):- member(loc2D(X,Y),L),valid_xy(X,Y).
make_loc2D(L,X,Y):- member(loc2G(GX,GY),L),valid_xy(GX,GY),g_to_d(L,GX,GY,X,Y).

make_vis2D(L,X,Y):- member(vis2D(X,Y),L),valid_xy(X,Y),!.
make_vis2D(L,X,Y):- member(vis2G(GX,GY),L),valid_xy(GX,GY),!,g_to_d(L,GX,GY,X,Y).

make_center2D(L,X,Y):- member(center2D(X,Y),L),valid_xy(X,Y),!.
make_center2D(L,X,Y):- member(center2G(GX,GY),L),valid_xy(GX,GY),!,g_to_d(L,GX,GY,X,Y).


good_enough_match(Matches):- Matches\==[],member(M,Matches),arg(_,M,Num),number(Num),!.


%i doubt it would be a good idea for either of us to hold out waiting for that
matches_input(InC,LHS,Matches):- 
 must_det((
  reduce_rules(InC,ReducedInC),
  reduce_rules(LHS,Reduced))),
  matches_input_2(ReducedInC,Reduced,Matches).

matches_input_2(obj(InC),object_to_object(_TestID,_Name,LHS,_Create,DebugInfo,_DebugInfo2),Matches):- !,
  matches_input_2(obj(InC),LHS,Matches), run_delayed_goals(DebugInfo).

matches_input_2(InC,LHS,Matches):- is_list(LHS),!,maplist(reduce_rules,LHS,LOut),append(LOut,Out),!,
   matches_input_2(InC,Out,Matches).
matches_input_2(InC,LHS,Matches):- is_list(InC),!,maplist(reduce_rules,InC,LOut),append(LOut,Out),!,
   matches_input_2(Out,LHS,Matches).
matches_input_2(InC,LHS,Matches):- matches_oprops(InC,LHS,Matches).

matches_oprops(obj(I),M,Matches):-!,must_det_ll(matches_oprops(I,M,Matches)).
matches_oprops(I,obj(M),Matches):-!,must_det_ll(matches_oprops(I,M,Matches)).
matches_oprops(I,M,[shared(Shared)|Matches]):- intersection(I,M,Shared,II,MM),matches_1oprop(II,MM,Matches).


is_skipped_match(iz(input)).
is_skipped_match(shape_rep(grav,_)).
is_skipped_match(points_rep(local,_)).
is_skipped_match(globalpoints(_)).
is_skipped_match(pg(_,_,_,_)).
is_skipped_match(link(_,_)).

matches_1oprop(_,[],[]):-!.
matches_1oprop([],R,R):-!.
matches_1oprop(I,M,[Prop|Matches]):- select(Prop,M,MM),select(Prop,I,II),!,matches_1oprop(II,MM,Matches).
matches_1oprop(I,M,[oi(Prop)|Matches]):- select(oi(Prop),M,MM),!,select(Prop,I,II),!,matches_1oprop(II,MM,Matches).
matches_1oprop(I,M,[oi(Prop,Code)|Matches]):- select(oi(Prop,Code),M,MM),!,select(Prop,I,II),run_delayed_goals(Code),!,matches_1oprop(II,MM,Matches).
matches_1oprop(I,M,[prefer_thru(Prop,Use)|Matches]):- select(prefer_thru(Prop,Use),M,MM),!,
   (select(Prop,I,II)->matches_1oprop(II,MM,Matches);(Prop=Use)).
matches_1oprop(I,M,[prefer_thru(Prop,Use)|Matches]):- select(prefer_thru(Prop,Use),M,MM),!,
   (select(Prop,I,II)->matches_1oprop(II,MM,Matches);(Prop=Use)).
matches_1oprop(I,M,[skip(Prop)|Matches]):- select(Prop,M,MM),is_skipped_match(Prop),!,matches_1oprop(I,MM,Matches). 
matches_1oprop(I,M,_):- member(Prop,M),make_unifiable(Prop,UProp),member(UProp,I),!,fail.
matches_1oprop(I,M,[leftover(I,M)]).
% matches_1oprop(I,M,[oi(Prop)|Matches]):- select(oi(Prop),M,MM),!,select(Prop,I,II),!,matches_1oprop(II,MM,Matches).

/*
matches_1oprop(I,M,[I|Matches]):- 
 select(MM,M,MMM),select(II,I,III),match_prop(II,MM),!,% pp_wcg(match_prop(II,MM)),
 matches_1oprop(III,MMM,Matches).*/
matches_1oprop(_,M,[]):- run_delayed_goals(M),!.

run_delayed_goals([]):-!.
run_delayed_goals([call(M)|More]):- !, call(M), run_delayed_goals(More).
run_delayed_goals([ignore(M)|More]):- !, ignore(M), run_delayed_goals(More).
run_delayed_goals([prefer_if(M,U)|More]):- !, ignore(M=U), run_delayed_goals(More).
run_delayed_goals([only_if(M,U)|More]):- !, call(M=U), run_delayed_goals(More).
run_delayed_goals([prefer_thru(M,U)|More]):- !, ignore(M=U), run_delayed_goals(More).
run_delayed_goals([_|More]):- !, run_delayed_goals(More).
run_delayed_goals(G):- ignore(call(G)). 
  
reduce_prop(ii(II),MM):- !, reduce_prop(II,MM).
reduce_prop(oi(II),MM):- !, reduce_prop(II,MM).
reduce_prop(obj(II),MM):- !, reduce_prop(II,MM).
reduce_prop(oi(II),MM):- !, reduce_prop(II,MM).
reduce_prop(oo(II),MM):- !, reduce_prop(II,MM).
reduce_prop(if(II),MM):- !, reduce_prop(II,MM).
reduce_prop(thr(II),MM):- !, reduce_prop(II,MM).
reduce_prop(prefer_if(II,MM),II):-!, ignore(II=MM).
reduce_prop(only_if(_,II),MM):- !, reduce_prop(II,MM).
reduce_prop(prefer_thru(II,MM),II):-!, ignore(II=MM).
reduce_prop(MM,MM).


same_props(I,Pre):- (var(I);var(Pre)),!.
same_props(I,Pre):- reduce_prop(I,II),I\==II,!,same_props(II,Pre).
same_props(I,Pre):- reduce_prop(Pre,II),Pre\==II,!,same_props(I,II).
same_props(I,Pre):- ([]==(I);[]==(Pre)),!.
same_props(E1,E2):- E1=E2,!.
same_props(I,Pre):- select(E2,Pre,PrePre),select(E2,I,II),!,same_props(II,PrePre).

match_prop(II,MM):- reduce_prop(II,I),reduce_prop(MM,M),!,I=M,!.


:- dynamic(arc_cache:assumed_mapped/3).

assert_doing_map(IO_DIR,A,[B|Objs]):- 
  assertz_in_testid(arc_cache:doing_map(IO_DIR,A,[B|Objs])),
  debug_c(mapping,
   (dash_chars, pp_wcg(IO_DIR), dash_chars,
     print_ss(IO_DIR,A,B),print_ss_rest(IO_DIR,2,Objs),dash_chars)).

print_ss_rest(IO_DIR,N,[A,B|Objs]):-  print_ss(IO_DIR+N,A,B),N2 is N + 2,print_ss_rest(IO_DIR,N2,Objs).
print_ss_rest(IO_DIR,N,[R]):- print_grid(IO_DIR+N,R).
print_ss_rest(_,_,[]).

mapping_order(I,O):- most_visible(I,O),!.
mapping_order(Obj,MO):- area(Obj,Area),cmass(fg,Obj,CMass),
  findall(_,doing_map_list(_,_,[Obj|_]),L),length(L,Cnt),NCnt is -Cnt,NCMass is -CMass,
  MO = NCnt+NCMass+Area.

cmass(FG,Obj,CMass):- has_prop(cc(FG,CMass),Obj).



into_title(IO_DIR,OI,TITLE):-
  upcase_atom(IO_DIR,A), upcase_atom(OI,B),
  sformat(TITLE,"~w  -->  ~w",[A,B]),!.

is_fg_bg_objects(AL,BL):- is_bg_object(BL) -> is_fg_object(AL) ; \+ is_fg_object(AL).
same_values(Prop,AL,BL):- one_has_prop(Prop,AL),one_has_prop(Prop,BL).
one_has_prop(P,L):- is_list(L),!,member(E,L),has_prop(P,E).
one_has_prop(P,O):- has_prop(P,O).


obj_oids(Obj,OIDZ):- is_object(Obj),!,obj_to_oid(Obj,OIDZ).
obj_oids(Objs,OIDZ):- into_group(Objs,Grp),maplist(obj_to_oid,Grp,OIDS),delistify_single_element(OIDS,OIDZ).

ip_op_debug_info([IP],OP,LOCK):- nonvar(IP), !, ip_op_debug_info(IP,OP,LOCK).
ip_op_debug_info(IP,[OP],LOCK):- nonvar(IP), !, ip_op_debug_info(IP,OP,LOCK).
ip_op_debug_info(IP,OP,[LOCK]):- 
   global_grid(IP,IPO),global_grid(OP,OPO),
   obj_oids(IP,IOIDS),obj_oids(OP,OOIDS),
   LOCK=print_ss([lhs(IOIDS)=IPO,rhs(OOIDS)=OPO]),!.
ip_op_debug_info(IP,OP,[LOCK]):- 
   obj_oids(IP,IOIDS),obj_oids(OP,OOIDS),
   LOCK=print_ss([lhs=IOIDS,rhs=OOIDS]),!.


save_rule1(GID,TITLE,IP,OP):- 
  WAZ_REV = "Was Reversed ",
  \+ sub_var(WAZ_REV,TITLE ),
  has_prop(giz(g(in)),OP),
  has_prop(giz(g(out)),IP),!,
  save_rule1(GID,WAZ_REV + TITLE,OP,IP).

% All background objects
save_rule1(IO_DIR,TITLE,AL,BL):-
  is_bg_object(AL),is_bg_object(BL),
  assert_showed_mapping(AL,BL),!,
  skip_rule(IO_DIR,"All background objects "+ TITLE,AL,BL).

save_rule1(IO_DIR,TITLE,AL,BL):- 
  same_values(iz(sid(_)),AL,BL),!,
  save_rule2(IO_DIR,TITLE+" Same Shapes",AL,BL).

save_rule1(IO_DIR,TITLE,A,B):- save_rule2(IO_DIR,TITLE,A,B),!.

save_rule2(IO_DIR,TITLE,IP,OP):- fail,
  WAZ_REV = "Was Reversed ",
  \+ sub_var(WAZ_REV,TITLE ),
  has_prop(giz(g(in)),OP),
  has_prop(giz(g(out)),IP),!,
  save_rule2(IO_DIR,WAZ_REV + TITLE,OP,IP).

save_rule2(IO_DIR,TITLE,IP,OP):- 
 ip_op_debug_info(IP,OP,LOCK),
 arc_cache:object_to_object(_,Was,_,_,_,LOCK),!,
 nop(skip_rule(IO_DIR, TITLE + "Already " + Was, IP, OP)).

% All background objects
save_rule2(IO_DIR,TITLE,AL,BL):-
  is_bg_object(AL),is_bg_object(BL),
  assert_showed_mapping(AL,BL),!,
  skip_rule(IO_DIR,"All background objects "+ TITLE,AL,BL).

save_rule2(IO_DIR,TITLE,IP,OP):- 
 ip_op_debug_info(IP,OP,LOCK),
 if_t(once(true;is_fg_object(IP);is_fg_object(OP)),
 (must_det_ll((
 assert_showed_mapping(IP,OP),
 make_rule_l2r_objs(Dir,[],IP,OP,II,OO,Mid), 
 %save_learnt_rule(arc_cache:object_to_object(TITLE,lhs(II),rhs(OO),Mid,LOCK),oneTwo,twoOne),
 make_rule_l2r_0(Dir,Mid,II,OO,III,OOO,NewShared),
 arrange_shared(NewShared,NewSharedS), 
 once((nb_current(rule_note,RN);RN=rule_note)),
 w_section(title(["SAVE", TITLE ,IO_DIR,RN]),
 ((print_rule_grids(IO_DIR,TITLE,IP,OP,LOCK),
   save_learnt_rule(arc_cache:object_to_object(TITLE,lhs(III),rhs(OOO),NewSharedS,LOCK),oneTwo,twoOne)))))))).

skip_rule(IO_DIR,TITLE,IP,OP):- 
 ip_op_debug_info(IP,OP,LOCK),
 once((nb_current(rule_note,RN);RN=rule_note)),
 w_section(title(["SKIP", IO_DIR, TITLE , RN]),
   print_rule_grids(IO_DIR,"SKIP " + TITLE,IP,OP,LOCK)).

print_rule_grids(IO_DIR,TITLE,IP,OP,LOCK):-
 must_det_ll((
 print_ss(LOCK),
 nop((og_display(IP,IIP), og_display(OP,OOP), 
 print_ss_html(orange,IIP,'IN'(TITLE,IO_DIR),_LW,OOP,'OUT'(TITLE,IO_DIR)))))).

og_display(OP,[wbg-P|OOP]):- 
  grid_size(OP,H,V),globalpoints(OP,OOP),
  hv_point(H,V,P),

%omit_in_rules(_,giz(_)).
%omit_in_rules(lhs,P):- not_for_matching(lhs,_,P).
%omit_in_rules(rhs,P):- not_for_creating(rhs,P).
omit_in_rules(_Why,globalpoints(_)).
omit_in_rules(_Why,call(True)):- True==true,!.
omit_in_rules(_,P):- not_used(P).

%skip_in_rules(Why,I):- not_for_matching(Why,_,I),not_for_creating(_,I).
skip_in_rules(_Why,points_rep(local,_)).
%skip_in_rules(_Why,link(_,_)).
%skip_in_rules(_Why,shape_rep(grav,_)).
%skip_in_rules(_Why,giz(gid(_))).

:- discontiguous(make_rule_l2r/7).


make_rule_l2r_objs(Dir,Shared,II_0,OO_0,IIIII,OOOOO,NewShared):- 
  listify(II_0,II),
  listify(OO_0,OO),
  select(obj(ObjI),II,III),
  select(obj(ObjO),OO,OOO),
  make_rule_l2r_until_no_changes(Dir,Shared,ObjI,ObjO,ObjII,ObjOO,MidShared),
  (ObjI\=@=ObjII;ObjO\=@=ObjOO),!,
  append(III,[obj(ObjII)],IIII),append(OOO,[obj(ObjOO)],OOOO),  
  make_rule_l2r_objs(Dir,MidShared,IIII,OOOO,IIIII,OOOOO,NewShared).
make_rule_l2r_objs(_Dir,Shared,II,OO,II,OO,Shared).


must_make_rule_l2r(_Dir,Shared,II,OO,III,OOO,SharedMid):- !, II=III,OO=OOO,Shared=SharedMid,!.
must_make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):-
  must_det_ll((make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid))).

/*

must_make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):-
  pp_wcg(must_make_rule_l2r(II,OO)),
  must_det_ll((make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid))).
*/

make_rule_l2r_until_no_changes(Dir,Shared,II,OO,IIII,OOOO,NewShared):-
   %pp_wcg(must_make_rule_l2r(Shared,II,OO)),
  make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid),
  ((II=@=III,OO=@=OOO,Shared=@=SharedMid) 
    -> (III=IIII,OOO=OOOO,Shared=NewShared);
    make_rule_l2r_until_no_changes(Dir,SharedMid,III,OOO,IIII,OOOO,NewShared)).

is_oid_or_gid(V,_):-var(V),!,fail.
is_oid_or_gid(OID,OID):- atom(OID),is_oid(OID),!.
is_oid_or_gid(oid(OID),OID):- !, atom(OID).
is_oid_or_gid(gid(GID),GID):- !, atom(GID).
is_oid_or_gid(glyph(GID),GID):- !, atom(GID).

overly_oided(M,EE=Var,MM):- sub_term(E,M),is_oid_or_gid(E,EE),subst001(M,EE,Var,MM).
remove_oids(M,O,[E|EL]):- overly_oided(M,E,MM),remove_oids(MM,O,EL).
remove_oids(M,M,[]).

% offset 2D
% make_rule_l2r(Dir,Shared,II,OO,III,OOO,[remove_io(RemoveI,RemoveO)|SharedMid]):- % 3 is random(2),
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- % 3 is random(2),
  transfer_props(Type,I,O), 
  my_partition(transfer_prop(Type),II,RemoveI,I_I_I),
  my_partition(transfer_prop(Type),OO,RemoveO,O_O_O),
  member(I,RemoveI),I=..[F1,X1,Y1],is_original_value(X1),is_original_value(Y1),
  member(O,RemoveO),O=..[F2,X2,Y2],is_original_value(X2),is_original_value(Y2),

  %select(O,OO,O_O_O), select(I,II,I_I_I),
  %X1=X2, F1=F2,
  %I=..[F1,X1,Y1], O=..[F2,X2,Y2], 
  \+ no_process(O), \+ no_process(I),
  %writeq(I), %is_all_original_value(X1),is_all_original_value(Y1), %is_all_original_value(X2),is_all_original_value(Y2), 
  %once((X1=X2,Y1=Y2);(X1=X2;Y1=Y2)),
  gen_offset_expression(Type,X2,X1,'X',VX1,VX2,Code1),
  gen_offset_expression(Type,Y2,Y1,'Y',VY1,VY2,Code2), /* gen_subst([F1,'X1'],VX1V), gen_subst([F1,'Y1'],VY1V), gen_subst([F2,'X2'],VX2V), gen_subst([F2,'Y2'],VY2V), DEBUGVARS = [debug_var(VY1V,VY1),debug_var(VY2V,VY2),debug_var(VX2V,VX2),debug_var(VX1V,VX1)], */
  NewI=..[F1,VX1,VY1], NewO=..[F2,VX2,VY2],
  combine_code(Code2,Code1,Code),
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).


make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):-
  \+ member(was_oid(_),II), 
  \+ member(was_oid(_),OO),
   select(oid(OID1),II,II0),atom(OID1),
   select(oid(OID2),OO,OO0),atom(OID2),
   remove_oids(II0,III,ELI), remove_oids(OO0,OOO,ELO),
   (ELI\==[];ELO\==[]),
   numbervars(ELI,66,_,[attvars(skip),singletons(false)]),
   numbervars(ELO,66,_,[attvars(skip),singletons(false)]),
   must_make_rule_l2r(Dir,
     %[remove_oids(lhs,OID1,ELI),remove_oids(rhs,OID2,ELO)|Shared],
     Shared, 
     [was_oid(OID1)|III],[was_oid(OID2)|OOO],IIII,OOOO,NewShared).



:- use_module(library(clpfd)).
%gen_offset_expression(_Type,X1,X2,_Var,VX1,VX1,[]):- number(X2),number(X1), X1 is X2.
gen_offset_expression(Type,X2,X1,Var,VX1,VX2,OUT):- number(X2),number(X1),OVX is X2 - X1,
  % gen_subst([Type,Var],OVXV),
   OVX \== 0,!,
   OUT = [call(ensure_true(Type,Var, VX2 #= VX1 + OVX))].

gen_offset_expression(_Type,X1,X2,_Var,VX1,VX1,[]):- number(X2),number(X1), X1 is X2.

ensure_true(_,_,G):- call(G).

make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- list_to_set(II,IIS),II\=@=IIS,!, make_rule_l2r(Dir,Shared,IIS,OO,III,OOO,SharedMid).
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- list_to_set(OO,OOS),OO\=@=OOS,!, make_rule_l2r(Dir,Shared,II,OOS,III,OOO,SharedMid).

make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):-
   my_partition(for_creating, OO, OOO, Removed), Removed\==[],OOO\==[],OO\=@=OOO,!,
   must_make_rule_l2r(Dir,Shared,II,OOO,IIII,OOOO,NewShared).

make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- fail,
   my_partition(not_for_matching(lhs), II, Removed, III), Removed\==[],!,
   must_make_rule_l2r(Dir,Shared,III,OO,IIII,OOOO,NewShared).

% offset 2D
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail,
  transfer_prop(Type,I),
  my_partition(transfer_prop(Type),II,RemoveI,I_I_I),
  my_partition(transfer_prop(Type),OO,RemoveO,O_O_O),
  member(I,RemoveI),I=..[F1,X1,Y1],is_original_value(X1),is_original_value(Y1),
  member(O,RemoveO),O=..[F2,X2,Y2],is_original_value(X2),is_original_value(Y2),
  ((X1=X2,Y1=Y2);(X1=X2;Y1=Y2)),
  gen_offset_expression(Type,X2,X1,'X3',VX1,VX2,Code1), 
  gen_offset_expression(Type,Y2,Y1,'Y4',VY1,VY2,Code2), 
  NewI=..[F1,VX1,VY1],%gen_subst([F1,'X3'],VX1), gen_subst([F1,'Y'],VY1),
  NewO=..[F2,VX2,VY2],%gen_subst([F2,'X4'],VX2), gen_subst([F2,'Y'],VY2), 
  combine_code(Code2,Code1,Code),
  must_make_rule_l2r(Dir,[removed(lhs,RemoveI),removed(rhs,RemoveO)|Shared],[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).

% offset 2D
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail, % 3 is random(2),
  transfer_props(Type,I,O), select(O,OO,O_O_O), select(I,II,I_I_I),
  I=..[F1,X1,Y1], O=..[F2,X2,Y2], \+ no_process(O), \+ no_process(I),
  %writeq(I), %is_all_original_value(X1),is_all_original_value(Y1), %is_all_original_value(X2),is_all_original_value(Y2), 
  %once((X1=X2,Y1=Y2);(X1=X2;Y1=Y2)),
  gen_offset_expression(Type,X2,X1,'X',VX1,VX2,Code1), 
  gen_offset_expression(Type,Y2,Y1,'Y',VY1,VY2,Code2), !, /* gen_subst([F1,'X1'],VX1V), gen_subst([F1,'Y1'],VY1V), gen_subst([F2,'X2'],VX2V), gen_subst([F2,'Y2'],VY2V), DEBUGVARS = [debug_var(VY1V,VY1),debug_var(VY2V,VY2),debug_var(VX2V,VX2),debug_var(VX1V,VX1)], */  
  NewI=..[F1,VX1,VY1],NewO=..[F2,VX2,VY2],
  combine_code(Code2,Code1,Code),
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).


% mass(R)
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- % 3 is random(2),
  transfer_props(Type,I,O), select(O,OO,O_O_O), select(I,II,I_I_I),
  I=..[F1,R1], O=..[F2,R2], \+ no_process(O), \+ no_process(I),
  gen_offset_expression(Type,R2,R1,'R',VR1,VR2,Code), 
  NewI=..[F1,VR1],NewO=..[F2,VR2],
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).


% cc(C,R)
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- % 3 is random(2),
  select(I,II,I_I_I),  
  C1=C2,
  I=..[F1,C1,R1], O=..[F1,C2,R2],
  select(O,OO,O_O_O), \+ no_process_props(O), \+ no_process_props(I), make_unifiable(I,O),
  gen_offset_expression(_Type,R2,R1,'R',VR1,VR2,Code), 
  NewI=..[F1,C1,VR1],NewO=..[F1,C2,VR2],
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).



not_for_matching(Lhs,I):- not_for_matching(Lhs,_,I).

% Cleanup LHS
make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- 
   my_partition(omit_in_rules(lhs), II, Removed, III), Removed\==[],!,
   must_make_rule_l2r(Dir,Shared,III,OO,IIII,OOOO,NewShared).

% Cleanup RHS
make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- 
   my_partition(omit_in_rules(rhs), OO, Removed, OOO), Removed\==[],!,
   must_make_rule_l2r(Dir,Shared,II,OOO,IIII,OOOO,NewShared).

/*

% Cleanup RHS
make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- 
   my_partition(skip_in_rules(rhs), OO, Removed, OOO), Removed\==[],!,
   must_make_rule_l2r(Dir,Shared,II,OOO,IIII,OOOO,NewShared).

% Cleanup LHS
make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- 
   my_partition(skip_in_rules(lhs), II, Removed, III), Removed\==[],!,
   must_make_rule_l2r(Dir,Shared,III,OO,IIII,OOOO,NewShared).
*/
make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- 
   select(unique_colors(CL1),II,II0), is_all_original_value(CL1), 
   member(unique_colors(CL2),OO), is_all_original_value(CL2),
   intersection(CL1,CL2,Overlap0,L1,_R1),  
   append(Overlap0,L1,Overlap),
   Overlap\==[],
   must_det_ll((
   maplist(gen_subst,Overlap,Vars),
   %pp_wcg(upcase_atom_var(CL1=Vars)),
   maplist(make_term3(debug_var),Overlap,Vars,DebugVars),
   subst_2L(Overlap,Vars,II0+OO+CL1,III+OOO+REPLACED),
   make_rule_l2r(Dir,Shared,[prefer_if(unique_colors(REPLACED),unique_colors(CL1)),run_delayed_goals(DebugVars)|III],OOO,IIII,OOOO,NewShared))).


gen_change_color_expression(C1,C2,VC1,VC2,(VC1=C2,VC2=C1),C1,C2).

% single color transfer/change
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail,
  member(pen([cc(C1,_)]),II),  member(pen([cc(C2,_)]),OO),
  is_original_value(C1),
  subst001(II,C1,VC1,I_I),subst001(OO,C2,VC2,O_O),
  gen_subst(C1,VC1),gen_subst(C2,VC2),
  NewI=pen([cc(VC1,_)]),NewO=pen([cc(VC2,1)]),
  remove_empty_colors(I_I,I_I_I),remove_empty_colors(O_O,O_O_O),
  gen_change_color_expression(C2,C1,VC1,VC2,Code,OVC1,OVC2),
  must_make_rule_l2r(Dir,Shared,[thr(NewI),call(Code),info(offsetV(color,OVC1)),info(offsetV(color,OVC2))|I_I_I],[NewO|O_O_O],III,OOO,SharedMid).

gen_subst(Name,Name):- compound(Name),Name='$VAR'(_),!.
gen_subst(Names,'$VAR'(VAR)):- p_n_atom1(Names,VAR).


make_term3(P2,A,B,Term):- Term=..[P2,A,B].

make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared):- fail, 3 is random(2),
   sub_term(E,II+OO),compound(E),E='$VAR'(D),%wots(S,color_print(white,D)),
   \+ number(D), \+ member(debug_var(D,_),Shared),
   subst(II+OO+Shared,E,S,III+OOO+SharedM),!,
   must_make_rule_l2r(Dir,[debug_var(D,S)|SharedM],III,OOO,IIII,OOOO,NewShared).

/*
make_rule_l2r(Dir,Shared,I,O,IIII,OOOO,NewShared):- fail,
  select(pen([cc(C1,_)]),I,II),select(pen([cc(C2,_)]),O,OO),C1\=@=C2,
  is_all_original_value(C1),is_all_original_value(C2),!,
  subst001(I_I,C1,VC1,II),gen_subst(C1,VC1),
  subst001(O_O,C2,VC2,OO),gen_subst(C2,VC2),
  must_make_rule_l2r(Dir,[call(subst(in,VC2,VC1,out))|Shared],[pen([cc(VC1,_)])|II],[pen([cc(VC2,_)])|OO],IIII,OOOO,NewShared).
*/

make_rule_l2r(Dir,Shared,I,O,IIII,OOOO,NewShared):- % 3 is random(2),
  simplify_l2r(Dir,I,C1,VC1,I_I,O,C2,VC2,O_O,Info),
  is_all_original_value(C1),is_all_original_value(C2),!,
  subst001(I_I,C1,VC1,II),gen_subst(C1,VC1),
  subst001(O_O,C2,VC2,OO),gen_subst(C2,VC2),
  must_make_rule_l2r(Dir,[Info|Shared],II,OO,IIII,OOOO,NewShared).

remove_empty_colors(O,O_O):- my_exclude(=(cc(_,0)),O,O_O).

:- discontiguous(simplify_l2r/10).

% single color changed
simplify_l2r(_Dir,I,C1,VC1,I_I,O,C2,VC2,O_O,Info):- fail,
  member(pen([cc(C1,_)]),I),member(pen([cc(C2,_)]),O),C1\=@=C2,
  is_all_original_value(C1),is_all_original_value(C2),
  Info = call((prefer_unify(VC1,C1),prefer_unify(VC2,C2))),
  remove_empty_colors(I,I_I),
  remove_empty_colors(O,O_O),!.

% single color transfer/change
simplify_l2r(_Dir,I,C1,VC1,I_I,O,C2,VC2,O_O,Info):-
  member(pen([cc(C1,_)]),I),member(pen([cc(C2,_)]),O),C1=@=C2,
  is_all_original_value(C1),is_all_original_value(C2),
  Info = call((prefer_unify(VC1,C1),prefer_unify(VC2,C2))),
  remove_empty_colors(I,I_I),
  remove_empty_colors(O,O_O),!.

prefer_unify(A,B):- A=B.
transfer_prop(locationD,bottem2D(_,_)).
transfer_prop(locationD,loc2D(_,_)).
transfer_prop(locationD,iz(cenYD(_))).
transfer_prop(locationD,iz(cenXD(_))).
transfer_prop(locationD,center2D(_,_)).

transfer_prop(locationG,bottem2G(_,_)).
transfer_prop(locationG,loc2G(_,_)).
transfer_prop(locationG,iz(cenGY(_))).
transfer_prop(locationG,iz(cenGX(_))).
transfer_prop(locationG,center2G(_,_)).

transfer_prop(vis2D,vis2D(_,_)).
transfer_prop(vis2G,vis2G(_,_)).

transfer_prop(rotOffD,rotSize2D(grav,_,_)).
transfer_prop(rotOffG,rotOffset2G(_,_)).

transfer_prop(rotations,rot2D(_)).

%transfer_prop(Type,How):- prop_type(Type,How).

no_process_props_f(holes).
no_process_props(P):- no_process(P); (compound(P),compound_name_arity(P,F,_),no_process_props_f(F)).
no_process(oid(_)).
no_process(P):- (compound(P),compound_name_arity(P,F,_),no_process_props_f(F)),!.
no_process(was_oid(_)).
%no_process(grid_ops(norm,_)).
%no_process(grid_rep(norm,_)).
no_process(call(_)).
no_process(prefer_unify(_,_)).
no_process(prefer_if(_,_)).
no_process(only_if(_,_)).
no_process(ignore(_)).
%no_process(loc2D(_,_)).
no_process(if(_,_)).
no_process(if(_)).
no_process(oi(_)).
no_process(oi(_,_)).
no_process(ii(_)).
no_process(oo(_)).
no_process(O):- \+ is_all_original_value(O).
no_process(thr(_)).
no_process(T):- compound(T),sub_term(E,T),(var(E);E='$VAR'(_)),!.


transfer_props(Type,I,O):- transfer_prop(Type,I), transfer_prop(Type,O), make_unifiable(I,O).
transfer_props(Type,I,O):- transfer_prop(Type,I), transfer_prop(Type,O), \+ make_unifiable(I,O).

% grid_ops(norm,[...])
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- F1=norm_ops, 
  I=..[F1,R1],
  O=..[F1,R2],
  select(I,II,I_I_I),
  select(O,OO,O_O_O),
  \+ no_process(O), \+ no_process(I),
  R1=@=R2,
  is_original_value(R1), is_original_value(R2),  
  (R1=R2),
  NewI=..[F1,RR1],gen_subst([F1,'NORM1'],RR1), 
  NewO=..[F1,RR2],gen_subst([F1,'NORM2'],RR2), 
  gen_offset_expression(F1,R2,R1,'NORM',RR1,RR2,Code), 
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).

combine_code(Code1,Code2,Code):- listify(Code1,C1),listify(Code2,C2),append(C1,C2,Code).
%make_solid_object(shape,sx,sy,grid)
%make_solid_object(shape,sx,sy,grid)
%c_r(copy_row_ntimes(_,sx))
%rot2D(RotGA)
%rot2D(RotGB)
%[a,b,c,d,e],[f,g,h,i,j],[k,l,m,n,o],[p,q,r,s,t],[u,v,w,x,y]
%copy_row_ntimes(_,sy)
%and(make_solid_object(rect,2,1),make_solid_object(rect,3,1)),grid_progress(g(perfect))

%norm_algo_ops(norm,[double_size,grid_progress(g(perfect))],[Red],Ops,G):- (Ops = make_solid_object(square,2,2), G=[[Red]]),!.

  /*

% mass(R)
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail,
  transfer_prop(Type,I),
  my_partition(transfer_prop(Type),II,RemoveI,I_I_I),
  my_partition(transfer_prop(Type),OO,RemoveO,O_O_O),
  member(I,RemoveI),I=..[F1,R1],is_original_value(R1),
  member(O,RemoveO),O=..[F2,R2],is_original_value(R2),
  ((R1==R2);(R1=@=R2);(R1=R2)),
  NewI=..[F1,VR1],gen_subst([F1,'R3'],VR1), 
  NewO=..[F1,VR2],gen_subst([F2,'R4'],VR2), 
  gen_offset_expression(Type,R2,R1,'R',VR1,VR2,Code1), 
  append([[thr(NewI)|Code1],I_I_I],I_I_I_I),
  make_rule_l2r(Dir,[removed(lhs,RemoveI),removed(rhs,RemoveO)|Shared],I_I_I_I,[NewO|O_O_O],III,OOO,SharedMid).

% cc(C,N)
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail,
  transfer_prop(Type,I),
  my_partition(transfer_prop(Type),II,NemoveI,I_I_I),
  my_partition(transfer_prop(Type),OO,NemoveO,O_O_O),
  member(I,NemoveI),I=..[F1,C1,N1],is_original_value(N1),
  member(O,NemoveO),O=..[F2,C2,N2],is_original_value(N2),
  ((N1==N2);(N1=@=N2);(N1=N2)),
  NewI=..[F1,C1,VN1],gen_subst([F1,C1,'N'],VN1), 
  NewO=..[F1,C1,VN2],gen_subst([F2,C2,'N'],VN2), 
  gen_offset_expression(Type,N2,N1,'N',VN1,VN2,Code1), 
  append([[thr(NewI)|Code1],I_I_I],I_I_I_I),
  make_rule_l2r(Dir,[removed(lhs,NemoveI),removed(rhs,NemoveO)|Shared],I_I_I_I,[NewO|O_O_O],III,OOO,SharedMid).


% o(T,E,R)
make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- fail,
  I=o(T1,E1,R1),O=o(T2,E2,R2),
  select(I,II,I_I_I),is_original_value(R1),
  select(O,OO,O_O_O),is_original_value(R2),
  (E1/T1 =@= E2/T2),
  %((R1==R2);(R1=@=R2);(R1=R2)),
  upcase_atom_var(R1,A1),upcase_atom_var(R2,A2),
  NewI=o(VT1,VE1,VR1),gen_subst(['Total',A1],VT1),gen_subst(['Element',A1],VE1), gen_subst([A1,'R'],VR1), 
  NewO=o(VT2,VE2,VR2),gen_subst(['Total',A2],VT2),gen_subst(['Element',A2],VE2), gen_subst([A2,'R'],VR2),   
  gen_offset_expression(ranks,R2,R1,o,VR1,VR2,Code1),
  append([[thr(NewI)|Code1],I_I_I],I_I_I_I),
  make_rule_l2r(Dir,Shared,I_I_I_I,[lookup(NewO)|O_O_O],III,OOO,SharedMid).

*/
make_unifiable(A1,A2):- make_unifiable0(A1,O),!,A2=O.
make_unifiable0(C1,_):- \+ compound(C1),fail.
make_unifiable0(A1,A2):- var(A1),!,A2=A1.
make_unifiable0(cc(C,_),cc(C,_)):-!.
make_unifiable0(iz(C1),iz(C2)):- !, make_unifiable(C1,C2).
make_unifiable0(giz(C1),giz(C2)):- !, make_unifiable(C1,C2).
make_unifiable0(Cmp,CmpU):-  Cmp=..[F|List1], 
  append(Left1,[C1],List1),append(Left2,[C2],List2), CmpU=..[F|List2],
  maplist(unifiable_cmpd_else_keep,Left1,Left2),
  unifiable_cmpd_else_var(C1,C2),!.
make_unifiable0(C1,C2):- functor(C1,F,A),functor(C2,F,A).

unifiable_cmpd_else_keep(A1,A2):- var(A1),!,A2=A1.
unifiable_cmpd_else_keep(Num,_):- number(Num),!.
unifiable_cmpd_else_keep(A1,A2):- compound(A1), \+ is_list(A1), make_unifiable(A1,A2),!.
unifiable_cmpd_else_keep(A1,A1).

unifiable_cmpd_else_var(A1,A2):- var(A1),!,A2=A1.
unifiable_cmpd_else_var(A1,A2):- compound(A1), \+ is_list(A1), make_unifiable(A1,A2),!.
unifiable_cmpd_else_var(_,_).

make_unifiable_with_ftvars(C1,C2):- functor(C1,F,A),functor(C2,F,A),numbervars(C2).

% simple purportionals
simplify_l2r(_Dir,I,C1,VC1,I_I,O,C2,VC2,O_O,Info):- fail,
  member(C1,I), make_unifiable(C1,VC1),
  copy_term(VC1,VC2),copy_term(VC2,C2),member(C2,O),  
  proportional(C1,C2,Purp),
  Info = [proportional(VC1,VC2,C1,C2,Purp)],
  I=I_I,O=O_O,!.

% unlikely useful Ordinals
make_rule_l2r(Dir,Shared,I,O,IIII,OOOO,[unused(ordinals,pg(OG,Size1,Ord1,Val),pg(OG,Size2,Ord2,Val))|NewShared]):- fail,
  select(pg(OG,Size1,Ord1,Val),I,II),
  select(pg(OG,Size2,Ord2,Val),O,OO),
  ((Size1 \== Size2, Ord1==Ord2);(Size1 == Size2, Ord1\==Ord2);(Size1 \== Size2, Ord1\==Ord2)), !,
  must_make_rule_l2r(Dir,Shared,II,OO,IIII,OOOO,NewShared).


%not_for_matching(lhs,I):- not_for_matching(lhs,_,I).

% Fallback 1
make_rule_l2r(_Dir,Shared,I,O,IIII,OOOO,NewSharedO):- fail,
  intersection(I,O,IO_DIR,IIII,OOOO),!,
  my_partition(for_creating,IO_DIR,Creating,Not),
  mark_as(2,l_r,Creating,Info1),
  mark_as(3,l_r,Not,Info2),
  append([Shared,Info1,Info2],NewSharedO).

make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):-  %1 is random(2),
  select(I,II,I_I),\+ no_process(I), select(O,OO,O_O),O=@=I,\+ no_process(O),
  is_all_original_value(O),is_all_original_value(I), %\+ member(ignore(I=_),I), 
   make_unifiable(I,IO_U), 
   %\+ transfer_prop(_,I), 
   !,%make_unifiable(I,IO_U),% numbervars(IO_U),
   make_rule_l2r(Dir,Shared,[prefer_thru(IO_U,I)|I_I],[thr(IO_U)|O_O],III,OOO,SharedMid).


make_rule_l2r(Dir,Shared,II,OO,III,OOO,SharedMid):- %1 is random(2),
  select(I,II,I_I), is_all_original_value(I), \+ no_process(I),   
   %\+ transfer_prop(_,I), 
   make_unifiable(I,O), select(O,OO,O_O), is_all_original_value(O),
   maybe_now_rule(Dir,Shared,I,O,I_I,O_O,III,OOO,SharedMid).

make_rule_l2r(_Dir,Shared,I,O,I,O,Shared):-!.

maybe_now_rule(Dir,Shared,I,O,I_I_I,O_O_O,III,OOO,SharedMid):- fail,
  I=..[F1,X1,Y1], O=..[F2,X2,Y2], \+ no_process(O), \+ no_process(I),
  gen_offset_expression(Type,X2,X1,'X',VX1,VX2,Code1), 
  gen_offset_expression(Type,Y2,Y1,'Y',VY1,VY2,Code2), !,
  NewI=..[F1,VX1,VY1],NewO=..[F2,VX2,VY2],
  combine_code(Code2,Code1,Code),
must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).

maybe_now_rule(Dir,Shared,I,O,I_I_I,O_O_O,III,OOO,SharedMid):- 
  I=..[F1,R1], \+ no_process(I),
  O=..[F2,R2], \+ no_process(O),
  gen_offset_expression(maybe_now_rule,R2,R1,'R',VR1,VR2,Code), 
  NewI=..[F1,VR1], %gen_subst([F1,'R1'],VR1), 
  NewO=..[F2,VR2], %gen_subst([F2,'R2'],VR2), 
  must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).


maybe_now_rule(Dir,Shared,I,O,I_I_I,O_O_O,III,OOO,SharedMid):- 
  I=..[F1,R1], \+ no_process(I),
  O=..[F2,R2], \+ no_process(O),
  gen_offset_expression(maybe_now_rule,R2,R1,'R',VR1,VR2,Code), 
  NewI=..[F1,VR1], %gen_subst([F1,'R1'],VR1), 
  NewO=..[F2,VR2], %gen_subst([F2,'R2'],VR2), 
must_make_rule_l2r(Dir,Shared,[oi(NewI,Code)|I_I_I],[oi(NewO,Code)|O_O_O],III,OOO,SharedMid).

maybe_now_rule(Dir,Shared,I,O,I_I,O_O,III,OOO,SharedMid):- 
   %make_unifiable(O,O_U), !, % numbervars(U),
  %make_unifiable(I,I_U),  make_rule_l2r(Dir,Shared,[only_if(I_U,I)|I_I],[oi(O)|O_O],III,OOO,SharedMid).
  make_rule_l2r(Dir,Shared,[oi(I)|I_I],[oi(O)|O_O],III,OOO,SharedMid).



mark_as(_N,M,I,O):- maplist(append_term(M),I,O),!.
mark_as(N,M,I,O):- maplist(append_term(level(N,M)),I,O).

% major update
make_rule_l2r_0(Dir,Shared,I,O,IIII,OOOO,NewShared):-fail,
  my_partition(for_matching,I,Matching,NotForMatching),
  my_partition(for_creating,O,Creating,NotForCreating),
  mark_as(4,lhs,NotForMatching,Info1),
  mark_as(5,rhs,NotForCreating,Info2),
  append([Shared,Info1,Info2],Shared1),
  make_rule_l2r_1(Dir,Shared1,Matching,Creating,III,OOO,Mid),
  make_rule_l2r_2(Dir,Mid,III,OOO,IIII,OOOO,NewShared).
make_rule_l2r_0(_Dir,Shared,I,O,I,O,Shared):-!.

make_rule_l2r_1(Dir,Shared,II,OO,III,OOO,[when_missing(EVar,E)|SharedMid]):- fail,
  sub_term(E,OO),compound(E),ground(E),propagate(E), 
  subst001(II,E,Var,MII),
  MII\=@=II,!,
  must_det_ll((
  subst001(OO,E,Var,MOO),
  make_rule_l2r_1(Dir,Shared,MII,MOO,III,OOO,SharedMid),
  make_replacement(E,EVar),
  Var = EVar)),!.
make_rule_l2r_1(_Dir,Shared,II,OO,II,OO,Shared).

make_rule_l2r_2(Dir,Shared,II,OO,III,OOO,[thru(O)|SharedMid]):- 
  select(I,II,I_I),select(O,OO,O_O),O=@=I, \+ transfer_prop(_,I), \+ no_process(O), is_all_original_value(O),!,
  make_rule_l2r_2(Dir,Shared,I_I,O_O,III,OOO,SharedMid).
make_rule_l2r_2(_Dir,Shared,II,OO,II,OO,Shared).

/*
arc_cache:object_to_object=[ arc_cache:object_to_object( t('0d3d703e'),
                "IN -> OUT",
                obj( [ pen([cc(RED,MASS)]),
                       grid_rep(norm,[[RED,RED,RED]]),
                       | SHARED_CONSTRAINED_OPEN ]),
                obj( [ pen([cc(PURPLE,MASS)]),
                       grid_rep(norm,[[PURPLE,PURPLE,PURPLE]])
                       | SHARED ])),

                 SHARED= [rot2D(A), vis2D(B,MASS),loc2D(D,E),grid_ops(norm,F), rotSize2D(grav,G,H),iz(sid(I)),
                          loc2G(J/K,L/M)],
                 CONSTRAINED =
                         [cc(fg,MASS), cc(bg,0),cc(is_colorish_var,0),cc(plain_var,0), cc(RED,MASS),
                         iz(symmetry_type((rot180))),iz(symmetry_type((flipV))),
                         iz(symmetry_type((flipH))), iz(symmetry_type((sym_hv))),
                         iz(column), iz(all_columns), iz(media(image))]
                 ]):- append(SHARED,CONSTRAINED,SHARED_CONSTRAINED),
                      append(SHARED_CONSTRAINED,_,SHARED_CONSTRAINED_OPEN).
*/

compound_not_ftvar(C):- compound(C), C\=='$VAR'(_).

make_replacement(I,I):- ( var(I); I='$VAR'(_)), !.
make_replacement([],[]).
make_replacement(E,A):- \+ compound(E), p_n_atom(E,Name),gensym(Name,Name1), A='$VAR'(Name1).
make_replacement(grid_ops(norm,I),grid_ops(norm,_)):- I==[],!.
make_replacement(obj(I),obj(O)):- !, maplist(make_replacement,I,O).
make_replacement(E,A):- is_list(E), p_n_atom(E,Name),gensym(Name,Name1), A='$VAR'(Name1).
make_replacement([H|T],[HH|TT]):- compound_not_ftvar(H), !, make_replacement(H,HH),make_replacement(T,TT).
make_replacement([H|T],[H|TT]):- !, make_replacement(T,TT).
make_replacement(I,O):- 
 compound_name_arguments(I, F, Args),
 maplist(make_replacement, Args, ArgsNew),
 compound_name_arguments( O, F, ArgsNew ),!.



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
  indv_props_list(O,OL),
  once(learn_rule_iin_oout(1,In,O,OL)).

learn_rule_iin_oout(_,In,O,OL):- mass(O,Mass),
  findall(SL-SAME-I-DL,
   (member(I,In),indv_props_list(I,IL),
    pred_intersection(same_2props,IL,OL,SAME,_,_IF,_OF),
    pred_intersection(diff_2props,IL,OL,DIFF,_,_,_),
    length(SAME,SL), length(DIFF,DL),SL>DL),SLIDL),
  sort_safe(SLIDL,SSLIDL),
  reverse(SSLIDL,RSLIDL),
  member(SL-SAME-I-DL,RSLIDL),
  pp([SL+DL, equal = SAME, in=I,out=OL]),  
  compare_objs1(Mode,I,O), %shape_rep(grav,I,Shape),shape_rep(grav,O,Shape), %pen(I,Pen),pen(O,Pen),
  mass(I,Mass),
  simplify_for_matching(lhs,I,II),
  simplify_for_creating(O,OO),
  save_learnt_rule(arc_cache:object_to_object(unk(Mode),II,OO,[],[learn_rule_iin_oout(I,O)]),I,O),!.

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
test_local_dyn(object_to_object).
%test_local_dyn(why_grouped).
test_local_dyn(cached_dictation).
test_local_dyn(oout_associatable).

test_local_save(F,A):- setof(F/A,(test_local_save(F),current_predicate(F/A),A\==0),L),member(F/A,L).
test_local_save(arc_test_property).
test_local_save(cached_tests).
test_local_save(cached_tests_hard).
test_local_save(cindv).
test_local_save(cmem).
test_local_save(cmem_hv).
test_local_save(g_2_o).
test_local_save(gid_glyph_oid).
test_local_save(did_map).
test_local_save(object_atomslist).
test_local_save(assumed_mapped).
test_local_save(object_to_object).
test_local_save(individuated_cache).
test_local_save(is_grid_obj_count).
test_local_save(is_grid_size).
test_local_save(is_gridmass).
test_local_save(is_why_grouped_g).
test_local_save(note).
test_local_save(oid_glyph_object).
test_local_save(omem).
test_local_save(smem).
test_local_save(test_info_cache).
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


assertz_in_testid(Goal):- dress_in_testid(Goal,TestIDGoal),!,assertz_if_new(TestIDGoal).
asserta_in_testid(Goal):- dress_in_testid(Goal,TestIDGoal),!,asserta_if_new(TestIDGoal).
assert_in_testid(Goal):- dress_in_testid(Goal,TestIDGoal),!,assert_if_new(TestIDGoal).
call_in_testid(Goal):- dress_in_testid(Goal,TestIDGoal),!,call(TestIDGoal).
retractall_in_testid(Goal):- dress_in_testid(Goal,TestIDGoal),retractall(TestIDGoal).

dress_in_testid(M:Goal,M:TestIDGoal):- !, dress_in_testid(Goal,TestIDGoal).
dress_in_testid(Goal,TestIDGoal):- Goal=..[F,A|Args], get_current_test(TestID), (A==TestID-> TestIDGoal=Goal; TestIDGoal=..[F,TestID,A|Args]).

assert_visually(H:-B):- !,unnumbervars((H:-B),(HH:-BB)), assert_visually1(HH,BB).
assert_visually( H  ):- unnumbervars(H,HH),assert_visually1(HH,true).

assert_visually1(H,B):- get_current_test(TestID), arg(1,H,W),W\==TestID,!, H=..[F|Args],GG=..[F,TestID|Args],assert_visually2(GG,B).
assert_visually1(H,B):- assert_visually2(H,B).

assert_visually2(H,B):- fail, copy_term((H:-B),(HH:-BB)),clause(HH,BB,Ref), clause(RH,RB,Ref),(H:-B)=@=(RH:-RB) ,!,(pp(cyan,known_exact(H:-B))).
assert_visually2(H,B):- fail, copy_term((H),(HH)),clause(HH,_,Ref), clause(RH,_,Ref),(H)=@=(RH) ,!,pp(cyan,known(H:-B)).
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

ignore_equal_e(InSet,InVars):- maplist(ignore_equal,InSet,InVars).
ignore_equal(X,Y):- ignore(X=Y).  

rev_key0(C-P,P-C).

cur_obj(Obj):- oid_glyph_object(_,_,Obj).
prop_group(Prop,Objs):-
  ensure_group_prop(Prop),findall(Obj,(cur_obj(Obj),has_prop(Prop,Obj)),Objs).


/*
instead of a DSL about transformations i think the DSL would be one that creates the images.. 
then the transformation becomes about what properties of the generative DSL change (both the input and the output 
have their own starting points that are gleaned by looking at what DSL would generate all the outputs vs what 
DSL would generate all the inputs) the thing that is learned by training is how the edits are supposed to happen in each
 of the generative DSLs 
the progression of inputs teaches the system abotu what the input's generative DSL is used for inputs 
though the progression of outputs give more information about the total test (but still give the hints about the the output's generative DSL) 
round tripping between a grid and the generative DSL seems important.   Has anyone started a Grid-> "generative DSL" convertor ?
oh yes even the operations such editing/transformation of the generative DSL is in the domain of yet another DSL that specifies those operations.. and in my code i even have a 3rd DSL that is specific to transformations that both the two previous DSLs are indexed against.. in order to make the things fast i assume that the three forms each transition between will be part of the final index built during training 
s the training pairs are fed in it eliminates the candidate associations
(the more training the faster it gets)
this way the only associations it uses and retains are correct ones 
we'll see thr there are extra possible correct associations (which may end up actually being incorrect answers that are logically correct).. 
Luckily the system produces a formal proof for it's answers (explainablity) 
*/
properties_that_changed(Grid1,Grid2,Props):-
  individuate(complete,Grid1,Props1),
  individuate(complete,Grid2,Props2),
  diff_terms(Props1,Props2,Props).

has_learnt_rule(TestID,In,Key,RuleDir,Out):- clause(learnt_rule(TestID,In,Key,RuleDir,Out),was_once(InSet,InVars)),
  ignore_equal_e(InSet,InVars).

shared_prop(Objs,Info):- var(Objs),!,enum_groups(Objs),shared_prop(Objs,Info).
%shared_prop([O|Objs],all_same(Prop)):- 
%  has_prop(Prop,O),once(maplist(has_prop(Prop),Objs)).
%shared_prop([O|Objs],all_diff(Unif)):-  has_prop(Prop,O),make_unifiable(Prop,Unif), 
%  once((findall(Unif,(member(E,Objs),O\=@=E,has_prop(Unif,E)),LL),
%    sort_safe([Prop|LL],S),length(S,Len1),length([O|Objs],Len2))),Len1\==Len2.

shared_prop(Objs,OUT):- shared_prop(_Prop,Objs,OUT).
shared_prop(Prop,Objs,OUT):- 
  length(Objs,ON),
  findall(Prop-E,(member(E,Objs),has_prop(Prop,E)),PropsObjs),
  maplist(arg(1),PropsObjs,PropVals),
  sort_safe(PropVals,SProps),
  shared_prop(ON,Objs,PropsObjs,SProps,OUT).

shared_prop(ON,Objs,PropsObjs,[Prop|PropVals],OUT):- 
  make_unifiable(Prop,Unif),  my_partition(can_unify(Unif),PropVals,MatchingPV,Nonmatching),
   length([Prop|MatchingPV],MPV),!,
  (shared_prop(ON,Objs,PropsObjs,MPV,Unif,MatchingPV,Prop,PropVals,OUT);
  shared_prop(ON,Objs,PropsObjs,Nonmatching,OUT)).

shared_prop(_ON,_Objs,PropsObjs,MPV,_Unif,[PV],_Prop,_PropVals,unique_value(PV,E)):- MPV==1, !, member(PV-E,PropsObjs).
shared_prop(_ON,_Objs,PropsObjs,_MPV,Unif,_,   Prop,  _PropVals,OUT):- 
  my_partition(same_prop(Prop),PropsObjs,HasProp,NotHasProp),
  maplist(arg(2),HasProp,OHasProp),length(HasProp,N),
  my_partition(can_unify(Unif-_),NotHasProp,HasPropUnif,_NotHasProp),
  length(HasPropUnif,NHP),
  out_prop(Prop,N,NHP,OHasProp,HasPropUnif,OUT).

%out_prop(Prop,N,0,OHasProp,HasPropUnif,OUT):- !, OUT = all_diff(Prop).
out_prop(Prop,_,0,_OHasProp,_HasPropUnif,OUT):- !, OUT = all_same(Prop).
out_prop(Prop,N,NHP,OHasProp,HasPropUnif,OUT):- OUT = prop(Prop,N,NHP,OHasProp),
  maplist(arg(2),HasPropUnif,ONotHasProp),
  nop(once(shared_prop(_Unif,ONotHasProp,_PP))).

can_unify(I,O):- \+ (I \= O).
same_prop(P,P-_).

enum_groups(G):- enum_groups0(G),length(G,L),between(2,30,L).
enum_groups0(G):- group_prop(Prop), prop_group(Prop,G).
enum_groups0(G):- why_grouped(_Why, G).
enum_groups0(G):- findall(Obj,(see_object_atomslist(_,Obj,_,_)),G), G\==[].
enum_groups0(S):- is_unshared_saved(_,S).
%enum_groups0(G):- current_group(G).

ensure_group_prop(Prop):- var(Prop),!,group_prop(Prop).
ensure_group_prop(_).


%pp(O):- nl_if_needed,print(O),nl.
use_test_associatable_group_real(In,SolutionO):- 
 must_det_ll((
  once((listify(In,In1), visible_order_fg(In1,In2))), 
  findall(Sol,(member(In3,In2),once(use_test_associatable_obj(In3,Sol))),Solution),
  flatten(Solution,SolutionF),visible_order_fg(SolutionF,SolutionO))), 
 w_section(use_test_associatable_group_real,
    print_side_by_side(use_test_associatable_group_real,In2,SolutionO)).

use_test_associatable_group(I,O):- 
  use_test_associatable_group_real(I,O),
  nop(print_side_by_side(real_associatable_group,I,O)).

gather_assumed_mapped(A,B):-
  call_in_testid(arc_cache:assumed_mapped([A],[B])).

io_order(A,B,B,A):- has_prop(giz(g(out)),A),!.
io_order(A,B,A,B).


minfo_gp(BS,gp(BS,PP)):- globalpoints(BS,GP),physical_points(GP,PP).


  /*
filter_redundant(BS,BSSS):- 
  abolish(arc_cache:showed_point_mapping/3),dynamic(arc_cache:showed_point_mapping/3),
  include(need_object,BS,BSSS).
*/


is_one_point(Obj1,HV1,C):- globalpoints(Obj1,Points),!,[C-HV1]=Points,nonvar(C).
contains_point(CHV,Obj2):- globalpoints(Obj2,Points),!,member(CHV,Points).

remove_singles_unneeded([],[]).
remove_singles_unneeded(AG01,AG0):- 
  arc_option(remove_singles_unneeded),
  select(Obj1,AG01,Rest), is_one_point(Obj1,HV,C),
  member(Obj2,Rest),contains_point(C-HV,Obj2),!,
  remove_singles_unneeded(Rest,AG0).
remove_singles_unneeded(AG0,AG0).



filter_redundant(BS,BSSSR):- arc_option(filter_redundant),!,filter_redundant(BS,BSSSR,_Removed).
filter_redundant(BS,BS).

filter_redundant(BS,BSSSR,Removed):- maplist(minfo_gp,BS,BSGP),
  reverse(BSGP,BSR),
  filter_redundant_r(BSR,BSSS,Removed),
  reverse(BSSS,BSSSR).

filter_redundant_r(BSR,RRest,[BS|Removed]):- fail,
  select(gp(BS,[GP]),BSR,Rest), member(gp(_,[A,B|C]),Rest),member(OO,[A,B|C]),
  OO=@=GP, !, filter_redundant_r(Rest,RRest,Removed).
filter_redundant_r([gp(BS,GP)|BSR],[BS|BSSS],Removed):-
  findall(GPE,member(gp(_,GPE),BSR),GPS),flatten(GPS,Flat),
  intersection(GP,Flat,_Shared,Unique,_),Unique\==[],!,
  %pp_wcg(uniq=Unique),
  filter_redundant_r(BSR,BSSS,Removed).
filter_redundant_r([gp(BS,_)|BSR],BSSS,[BS|Removed]):- filter_redundant_r(BSR,BSSS,Removed).
filter_redundant_r([],[],[]).



need_object(B):- 
  must_det_ll((
  is_object(B),
  has_prop(giz(g(IO_DIR)),B),  
  findall(IO_DIR-Ps,showed_point_mapping_ok(IO_DIR,Ps),L),
  assert_showed_mapping(B,[]),
  findall(IO_DIR-Ps,showed_point_mapping_ok(IO_DIR,Ps),L2),!,
  pp_wcg(L2\=@=L))),
  (L2\=@=L). %->writeln(need_object(B));(writeln(not_need_object(B)),fail)).
  

show_safe_assumed_mapped:-
% pp_wcg(show_safe_assumed_mapped),
 findall(sam(Why,AA,BB),
   (safe_assumed_mapped(Why,A,B),io_order(A,B,AA,BB)),SAMS),
 maplist(arg(2),SAMS,AS),list_to_set(AS,ASS),
 maplist(arg(3),SAMS,BS),list_to_set(BS,BSS), 
 filter_redundant(ASS,ASSS),
 filter_redundant(BSS,BSSS),!,
 forall(member(A,ASSS),
   forall(member(B,BSSS),
   (findall(Why,member(sam(Why,A,B),SAMS),WHYS),  
    (ignore((      
       %print_ss(A,B),
           % \+  is_bg_object(A), \+ is_bg_object(B),
      (WHYS\==[], 
        sformat(SWHYS,'~w',[WHYS]),
        g_display(A,AA),g_display(B,BB),
         dash_chars,print_ss(SWHYS,AA,BB)))))))),
 dash_chars,!.



gather_assumed_mapped_o_l(A,BL):- findall(B,gather_assumed_mapped(A,B),BL).
gather_assumed_mapped_l_o(AL,B):- findall(A,gather_assumed_mapped(A,B),AL).

arc_cache:doing_map(Out,B,A):-      doing_map_list(Out,B,[A|_]).

doing_map_sc(Out,A,B):-   doing_map_list(Out,B,List),member(A,List),(is_bg_object(B)->is_bg_object(A);is_fg_object(A)),can_pair(A,B).

:- dynamic(cant_pair/2).

doing_map_list(Out,C,D):- call_in_testid(arc_cache:doing_map(Out,C,D)).

can_pair(A,B):- 
  obj_testid(A,TestID),obj_testid(B,TestID),
  obj_example_num(A,EN),obj_example_num(B,EN),
  A\=@=B,
  once(is_fg_object(A);is_fg_object(B)), \+ cant_pair(A,B).

safe_assumed_mapped(o_i,A,B):-  doing_map_list(out,B,[A|_]),can_pair(A,B).
%safe_assumed_mapped(two_way_sc,A,B):- doing_map_sc(out,B,A),doing_map_sc(in,A,B),can_pair(A,B).
%doing_map_list(in,A,[B|_]),
safe_assumed_mapped(only_ness,A,B):- gather_assumed_mapped(A,B), \+ (gather_assumed_mapped(A,C),B\==C),can_pair(A,B).

safe_assumed_mapped(priorities(LenA,LenB),A,B):- findall(v(APB,A,LenA,B,LenB), (safe_assumed_mapped(A,LenA,B,LenB),APB is LenA+LenB),List),
   sort_safe(List,Set),
   member(v(_,A,LenA,B,LenB),Set),can_pair(A,B).
safe_assumed_mapped(A,LenA,B,LenB):- gather_assumed_mapped(A,B),
  gather_assumed_mapped_o_l(A,BL),length(BL,LenA), gather_assumed_mapped_l_o(AL,B),length(AL,LenB).

%clear_arc_learning:- !.
clear_arc_learning:- 
  abolish(arc_cache:object_to_object/6),dynamic(arc_cache:object_to_object/6),
  dmsg(clear_arc_learning),
  must_det_ll((
  retractall_in_testid(arc_cache:did_map(_,_,_,_)),   
  retractall_in_testid(arc_cache:doing_map(_,_,_)),
  retractall_in_testid(arc_cache:object_atomslist(_,_,_,_)),
  retractall_in_testid(arc_cache:object_to_object(_,_,_,_,_)),
  retractall_in_testid(arc_cache:assumed_mapped(_,_)),
  retractall_in_testid(arc_cache:object_atomslist(_,_,_,_)))),!.
prolog:make_hook(after, Some):- any_arc_files(Some), forall(clear_arc_learning,true), fail.

%:- clear_arc_learning.

use_test_associatable_obj(In,Sol):-
 ((
   matches_close_prop(In,giz(g(in)),List),

   if_t(List\==[],
        (member(F0,List),gather_assumed_mapped(F0,F1),
         print_ss([in=In,mid=F0,out=F1]))),

   matches_close_prop(In,giz(g(out)),OutList),
   if_t(OutList\==[],
        (member(F2,OutList), 
         print_side_by_side((in<-out),In,F2))),
  
   ignore(Sol=F1),
   ignore(Sol=F2))).

use_test_associatable_obj(In,Sol):- In = obj(O1),
  wno_must(try_each_using_training([obj(O1)],_ExpectedOut,Keepers,OurOut)),
  o_globalpoints(OurOut,Sol1),
  o_globalpoints(Keepers,Sol2),
  append_sets([Sol1,Sol2],Sol).


matches_close_prop(In,giz(g(in)),List):- 
  findall(F0,gather_assumed_mapped(F0,_),Objs),
  sort_by_closeness(In,Objs,List), List\==[].
matches_close_prop(In,giz(g(out)),List):- 
  findall(F1,gather_assumed_mapped(_,F1),Objs),
  sort_by_closeness(In,Objs,List), List\==[].
matches_close_prop(In,Prop,List):-
  prop_group(Prop,Objs),
  sort_by_closeness(In,Objs,List).


/*
 must_det_ll((
  simplify_for_matching(lhs,In,IIn),
  dmsg(looking_for(IIn)),
  %pp(in=IIn),  
  findall(Ref-OutS,use_test_associatable_io(IIn,OutS,Ref),OutL),
  keysort(OutL,OutLS),
  maplist(arg(2),OutLS,OutLS2),
  print(outs1=OutLS2),nl,
  clumped(OutLS2,OutLS3),
  maplist(rev_key0,OutLS3,OutLS4),
  sort_safe(OutLS4,OutLS2SS),
  reverse(OutLS2SS,OutLS2SSR),
  maybe_four_terse(OutLS2SSR,OutLS2SSRT),
  print(outs2=OutLS2SSRT),nl,
  maplist(arg(2),OutLS2SS,OutLS2SSBest),
  last(OutLS2SSBest,Best),
  globalpoints(Best,OGPoints),  
  points_to_grid(OGPoints,Sol))),atrace.*/

use_test_associatable(In,OutR):- 
  findall(InS,simplify_for_matching_nondet(lhs,In,InS),InL),
  findall(OutS-Ref,(member(InS,InL),use_test_associatable_io(InS,OutS,Ref)),OutL),    
   OutSet=[for_output2],     
   nb_set_add1(OutSet,OutL),
   ignore(OutR=OutSet),!,
   pp(outSet2=OutSet).

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
:- dynamic(arc_cache:object_to_object/6).
use_test_associatable_io(I,O,Ref):- is_list(I),!,member(E,I),use_test_associatable_io(E,O,Ref).
use_test_associatable_io(I,O,Ref):- get_current_test(TestID), clause(arc_cache:object_to_object(TestID,_How,Pre,O,_,_),_,Ref), \+ \+ same_props(I,Pre).
use_test_associatable_io(I,O,Ref):- get_current_test(TestID),
  clause(test_associatable(TestID,Pre,O),_,Ref),
  \+ \+ same_props(I,Pre),
  nop(pp(same_props(I,Pre))).



use_learnt_rule(In,_RuleDir,Out):- use_test_associatable(In,Out).

use_learnt_rule(In,RuleDir,ROut):- %get_vm(VM), %Target=VM.grid_target, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)),
  ((has_learnt_rule(TestID,In,Key,RuleDir,Out);has_learnt_rule(TestID,_,Key,RuleDir,Out);has_learnt_rule(TestID,In,_,RuleDir,Out))),
  pp(orange,using_learnt_rule(In,Key,RuleDir,Out)),
  ignore(Out = ROut).

use_learnt_rule(In,RuleDir,Out):- get_vm(VM), % Target=VM.grid_target, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)), 
   In = VM.grid_o,
   Head = learnt_rule(TestID0,In0,Key0,RuleDir0,Out0),
   Rule = rule(Len,In0,Key0,RuleDir0,TestID0,Out0,Ref),
   pp(searching_for=[in(In),dir(RuleDir),key(Key)]),
  findall(Rule,
   (clause(Head,_Vars,Ref),
    call(Head),
    matchlen([In0,Key0,RuleDir0,TestID0],[In,Key,RuleDir,TestID],Len)),
   Matches), 
   sort_safe(Matches,Sort),
   %reverse(Sort,Reverse), 
   last(Sort,Last),
   must_det_ll(Last = Rule),
   ignore(In=In0),
   ignore(Out = Out0),
   %\+ \+ maplist(print_rule(sort_safe),Matches),
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


upcase_atom_var(Num,VAR):- uc_av(Num,Name),VAR='$VAR'(Name),!.
upcase_atom_var(Num,'$VAR'(Num)):-!.
upcase_atom_var(_,_).

uc_av(Var,Name):- var(Var),!,p_n_atom1(Var,Name).
uc_av(Int,Name):- integer(Int),atom_concat('INT_',Int,Name).
uc_av(Num,Name):- number(Num),atom_concat('FLOAT_',Num,DotName),replace_in_string(['.'-'_dot_'],DotName,Name).
uc_av(List,Name):- List==[],!,Name='ListNil'.
uc_av(List,Name):- is_list(List),maplist(uc_av,List,Names),atomic_list_concat(Names,'_',Name),!.
%uc_av(Atom,Name):- atom(Atom),upcase_atom(Atom,Name).
uc_av(Atom,Name):- p_n_atom1(Atom,Name).

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
never_labels_in(shape_rep(grav,_)).
never_labels_in(mass(1)).
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
maybe_unbind_label(G):- skip_for_now, integer(G),G<1.
maybe_unbind_label(G):- \+ atom(G),!,fail.
maybe_unbind_label(G):- skip_for_now, is_real_fg_color(G),!.
%maybe_unbind_label(G):- downcase_atom(G,D),\+ upcase_atom(G,D).

skip_for_now:- fail.

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
   find_ogs(H,V,F1,Grid),% atrace,

   grid_to_points(F1,GH,GV,Points),
   pp(Points),
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
  dash_chars(60,"|"),nl,nl,nop((ppnl(arc1(TestID)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  show_pair_grid(red,IH,IV,OH,OV,in,out,PairName,In,Out).

with_named_pair(solve,TestID,PairName,In,Out):- !,
  with_named_pair(cheat,TestID,PairName,In,Out).

with_named_pair(cheat,TestID,PairName,In,Out):- !,
  ignore(catch(solve_test(TestID,PairName,In,Out),E,(u_dmsg(E),fail))),!.

with_named_pair(learn,TestID,PairName,In,Out):- !,
  nop((ppnl(learning(TestID=PairName)),nl)),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  %ccs(In,InCC),
  %ccs(Out,OutCC),
  compute_unshared_indivs(In,UnsharedIn),
  compute_unshared_indivs(Out,UnsharedOut),
  show_pair_diff(IH,IV,OH,OV,in(unshared),out(unshared),PairName,UnsharedIn,UnsharedOut),
  %merge_indivs(UnsharedIn,UnsharedOut,BetterA,BetterB,BetterC), 
  %show_pair_i(IH,IV,OH,OV,better,PairName,BetterA,BetterB),
  %show_pair_i(IH,IV,OH,OV,iz(info(combined)),PairName,BetterC,Out),
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
  get_current_test(CName),!,
  new_test_pair(PairName),
  GridNameIn= PairName*in,
  GridNameOut= PairName*out,
  if_t(nonvar(Out),set_grid_tid(In,GridNameIn)),
  if_t(nonvar(In),set_grid_tid(Out,GridNameOut)),!,
  nop(name_the_pair_reset(CName,TestID,ExampleNum)).

name_the_pair_reset(CName,TestID,ExampleNum):- 
  /*must_det_ll*/((
   ignore((CName\==TestID, 
        set_current_test(TestID),
        dash_chars(60,"A"),nl,dash_chars(60,"|"),dash_chars(6,"\n"),nl,
        dash_chars(60,"|"),nl,dash_chars(60,"V"),nl,
        nl,ppnl(arc1(TestID)),nl,nl,dash_chars(60,"A"),nl)),   
  forall(runtime_test_info(TestID,Info), pp_wcg(fav(TestID,Info)=ExampleNum)),nl)),!.


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
     ppnl(learned(Name)))))).

%learn_shapes:- forall(l_shape(Name,Ascii), learn_shape(Name,Ascii)).

 

:- include(kaggle_arc_footer).


