/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.



:- dynamic(learnt_rule/5).
:- dynamic(print_rule/5).

learned_test:- notrace((get_current_test(TestID),learned_test(TestID))).
learned_test(TName):- 
  fix_id(TName,TestID),
   format('% ?- ~q. ~n',[learned_test(TName)]),
   %forall(clause(learnt_rule(TestID,A,B,C,D),Body),
    %print_rule(learned_test,learnt_rule(TestID,A,B,C,D):-Body)),
    training_info(TestID,Info),
    maplist(print_rule(TestID),Info).
 


print_rule(M,ref(Ref)):- !,
  clause(H,B,Ref), 
  print_rule(M,(H:-B)).

print_rule(M,(X:-True)):- True == true,!, print_rule(M,X).
print_rule(M,(learnt_rule(TestID,A,B,C,D):-Body)):- !,
    ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    pt(orange,M=[C=[TestID,in=(A),label=(B),out=(D)]]).
print_rule(M,(X:-Body)):- !,
    ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    pt(orange,M=[X]).
print_rule(M,O):- pt(orange,M=[O]).

save_learnt_rule(TestID,In,InKey,RuleDir,Out):-
  save_learnt_rule(learnt_rule(TestID,In,InKey,RuleDir,Out)).

save_learnt_rule(RuleIn):- save_learnt_rule(RuleIn,RuleIn).
save_learnt_rule(RuleIn,InGoal):-  
  labels_for(InGoal,InSet),
  length(InSet,InLen),length(InVars,InLen),
  subst_rvars(InSet,InVars,RuleIn,NewRuleIn),!,
  Assert = (NewRuleIn:-was_once(InSet,InVars)), 
  assert_visually(Assert),!.

    
group_by(mass).
group_by(iz).
group_by(color).
group_by(shape).
group_by(rotation).

has_prop(P,Obj):- indv_props(Obj,Props),!,member(Q,Props),Q=@=P.

group_group(What,[Obj|In],[P-G1|Groups]):- indv_props(Obj,Props),member(P,Props),functor(P,What,_),!,
  [Obj|Include] = G1,
  my_partition(has_prop(P),In,Include,Exclude),
  group_group(What,Exclude,Groups).
group_group(_,_,[]).

learn_group(What,Objs):- assert_visually(group_associatable(What,Objs)).

learn_about_group(In):- 
  forall(group_by(What),
   (group_group(What,In,Groups),
    maplist(learn_group(What),Groups))).


learn_rule_o(Mode,InVM,OutVM):- is_map(InVM),is_map(OutVM),!,
  pt(Mode=learn_rule_o(Mode,InVM,OutVM)),
  extract_vm_props(InVM,InProps),   learn_grid_rules(Mode,InProps),
  extract_vm_props(OutVM,OutProps), learn_grid_rules(Mode,OutProps),
  learn_rule_o(Mode,InProps,OutProps).

learn_rule_o(_Mode,G,_):- is_list(G), is_group(G), learn_about_group(G), fail.
learn_rule_o(_Mode,_,G):- is_list(G), is_group(G), learn_about_group(G), fail.

learn_rule_o(Mode,In,Out):- is_list(In), is_list(Out), 
  \+ is_grid(In), \+ is_grid(Out),
  \+ is_group(In), \+ is_group(Out),
  forall(select_some(I,In,MoreIn),
   forall(select_some(O,Out,MoreOut),
    (learn_rule_o(Mode,MoreIn,MoreOut),
    learn_rule_o(Mode,I,O)))).

learn_rule_o(out_out,I,O):- !, save_learnt_rule(oout_associatable(I,O)).

learn_rule_o(_Mode,I,O):- save_learnt_rule(test_associatable(I,O)).

extract_vm_props(VM,[VM.grid,VM.objs]).

select_some(I,List,Rest):- append(_,[I|Rest],List).

learn_grid_rules(Mode,Props):-  
  forall(select_some(P,Props,Others),
    learn_rule_ee(Mode,P,Others)).
learn_rule_ee(Mode,P,Others):- forall(member(O,Others),learn_grid_local(Mode,P,O)).

learn_grid_local(Mode,P,O):- P @< O, !, learn_grid_local(Mode,O,P).
learn_grid_local(_Mode,P,O):- assert_visually(grid_associatable(P,O)).

:- dynamic(test_local_dyn/1).
test_local_dyn(learnt_rule).
test_local_dyn(print_rule).
test_local_dyn(grid_associatable).
test_local_dyn(test_associatable).
test_local_dyn(why_grouped).
test_local_dyn(cached_dictation).
test_local_dyn(oout_associatable).

assert_visually(H:-B):- !,unnumbervars((H:-B),(HH:-BB)), assert_visually1(HH,BB).
assert_visually( H  ):- unnumbervars(H,HH),assert_visually1(HH,true).

assert_visually1(H,B):- get_current_test(TestID), arg(1,H,W),W\==TestID,!, H=..[F|Args],GG=..[F,TestID|Args],assert_visually2(GG,B).
assert_visually1(H,B):- assert_visually2(H,B).

assert_visually2(H,B):- copy_term((H:-B),(HH:-BB)),clause(HH,BB,Ref), clause(RH,RB,Ref),(H:-B)=@=(RH:-RB) ,!,pt(cyan,known_exact(H:-B)).
assert_visually2(H,B):- copy_term((H),(HH)),clause(HH,_,Ref), clause(RH,_,Ref),(H)=@=(RH) ,!,pt(cyan,known(H:-B)).
assert_visually2(H,B):- functor(H,F,_), my_asserta_if_new(test_local_dyn(F)), print_rule(F,(H:-B)), my_asserta_if_new((H:-B)).

training_info(TestID,Info):-
 findall((X:-Y),
  (test_local_dyn(F),
   (current_predicate(F/A),
    ((functor(X,F,A),
      ((clause(X,Y,Ref),arg(1,X,E),E==TestID),
       nop(erase(Ref))))))),Info).

clear_training(TestID):-
  set_bgc(_),
  set_flag(indiv,0),
  forall(test_local_dyn(F),
   forall(current_predicate(F/A),
    ((functor(X,F,A),
      forall((clause(X,_,Ref),arg(1,X,E),E==TestID),
       erase(Ref)))))),
  nb_delete(grid_bgc),
  nb_linkval(test_rules, [rules]),
  wno((clear_shape_lib(test), clear_shape_lib(noise), 
   retractall(grid_nums(_,_)), retractall(grid_nums(_)))),
  nop(retractall(g2o(_,_))),!.
 

learn_rule(In,Out):-
  get_vm(VM),
  VM.rule_dir = RuleDir,
  learn_rule(In,RuleDir,Out).



learn_rule(In,RuleDir,Out):- 
 get_vm(VM), 
 Target=VM.grid_out, 
 is_grid(Target),!,
 Out = Target,
 get_current_test(TestID), 
 get_vm(last_key,Key),    
 save_learnt_rule(TestID,In,Key,RuleDir,Out),!.


learn_rule(In,RuleDir,ROut):- %get_vm(VM), %Target=VM.grid_out, 
 get_current_test(TestID),
  get_vm(last_key,Key),  
  ((learnt_rule(TestID,In,Key,RuleDir,Out);learnt_rule(TestID,_,Key,RuleDir,Out))),
  pt(orange,using_learnt_rule(In,Key,RuleDir,Out)),
  ignore(Out = ROut).

learn_rule(In,RuleDir,ROut):- %get_vm(VM), %Target=VM.grid_out, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)),
  ((learnt_rule(TestID,In,Key,RuleDir,Out);learnt_rule(TestID,_,Key,RuleDir,Out);learnt_rule(TestID,In,_,RuleDir,Out))),
  pt(orange,using_learnt_rule(In,Key,RuleDir,Out)),
  ignore(Out = ROut).


learn_rule(_In00,RuleDir,Out):- get_vm(VM), % Target=VM.grid_out, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)), 
   In = VM.grid_o,
   Head = learnt_rule(TestID0,In0,Key0,RuleDir0,Out0),
   Rule = rule(Len,In0,Key0,RuleDir0,TestID0,Out0,Ref),
   pt(searching_for=[in(In),dir(RuleDir),key(Key)]),
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
   \+ \+ maplist(print_rule(sort),Matches),
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

labels_for(Goal,Labels):- 
  findall(Atom,(sub_label(Atom,Goal),maybe_unbind_label(Atom)),Atoms), 
  list_to_set(Atoms,Set),
  include(two_or_more(Atoms),Set,Labels).

two_or_more(Atoms,Label):- select(Label,Atoms,Rest),member(Label,Rest).

sub_label(X, X).
sub_label(X, Term) :-
    compound(Term),
    \+ never_labels_in(Term),
    arg(_, Term, Arg),
    sub_label(X, Arg).

never_labels_in(iz(_)).
ignored_constraint(globalpoints(_)).

never_unbind_label(G):- var(G),!.
never_unbind_label(Int):- integer(Int), Int > 7 ; Int == 1.
never_unbind_label(true).
never_unbind_label(false).
never_unbind_label(G):- \+ atom(G),!,fail.
never_unbind_label(G):- atom_length(G,1),!.
never_unbind_label(G):- downcase_atom(G,D), upcase_atom(G,D).
never_unbind_label(G):- atom_chars(G,Cs),member(C,Cs),char_type(C,digit),!.

maybe_unbind_label(G):- var(G),!,fail.
maybe_unbind_label(G):- never_unbind_label(G),!,fail.
maybe_unbind_label(G):- integer(G),!.
maybe_unbind_label(G):- \+ atom(G),!,fail.
maybe_unbind_label(G):- downcase_atom(G,D),\+ upcase_atom(G,D).

subst_rvars([],[],A,A):-!. 
subst_rvars([F|FF],[R|RR],S,D):- debug_var(F,R),subst001(S,F,R,M), subst_rvars(FF,RR,M,D).


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

find_by_shape(Grid,Find,Founds):- 
 makeup_gridname(ID),
 vis_hv(Find,GH,GV),
 decolorize(Find,F), 
 Prog = 
  (all_rotations(F,F1),
   %print_grid(F1),!,
   find_ogs(H,V,F1,Grid),% trace,

   grid_to_points(F1,GH,GV,Points),
   pt(Points),
   make_indiv_object(ID,GH,GV,Points,[iz(find_by_shape),F1,loc_xy(H,V)],F2)),
 findall(F2,Prog,Matches),
 align_founds(Matches,Founds).

align_founds(Founds,Founds).

in_out(In,Out):-
  nb_current(test_pairname,PairName),
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

with_arc(Action,arc):- !, findall(Name,kaggle_arc_io(Name,_+_,_,_),L),
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
  PairName= TestID*ExampleNum,
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
  set_grid_id(In,GridNameIn),
  set_grid_id(Out,GridNameOut),  
  task_info(TestID,Info), pt(fav(TestID,Info)),nl)).
  


compute_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   compute_unshared_indivs(GN,Grid,Unshared).

compute_unshared_indivs(_GN,Grid,Unshared):-
   individuate(complete,Grid,Unshared).

compute_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   compute_shared_indivs(GN,Grid,SharedIndvs).
compute_shared_indivs(GN,Grid,SharedIndvs):-
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   compute_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs).


ensure_unshared_indivs(In,Unshared):-
   get_grid_and_name(In,Grid,GN),
   ensure_unshared_indivs(GN,Grid,Unshared).
ensure_unshared_indivs(GN,Grid,Unshared):-
   is_unshared_saved(GN,Unshared)-> true;
   individuate(complete,Grid,Unshared),
   assert(is_unshared_saved(GN,Unshared)).

ensure_shared_indivs(In,SharedIndvs):-
   get_grid_and_name(In,Grid,GN),
   ensure_shared_indivs(GN,Grid,SharedIndvs).
ensure_shared_indivs(GN,Grid,SharedIndvs):-
   is_shared_saved(GN,SharedIndvs)-> true;
   grid_shared_with(GN,With),into_grid(With,OtherGrid),
   ensure_unshared_indivs(With,OtherGrid,Unshared),
   individuate(Unshared,Grid,SharedIndvs),
   assert(is_shared_saved(GN,SharedIndvs)).


/*

! 
_
/
\

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



:- fixup_exports.

