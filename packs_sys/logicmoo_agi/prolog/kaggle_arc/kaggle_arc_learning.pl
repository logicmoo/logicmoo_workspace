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
    maplist(print_rule(TestID),Info),
    length(Info,Len),
    ptc(orange,format('~N~n% Rules Learned: ~w~n~n',[Len])),!.
 


print_rule(M,ref(Ref)):- nonvar(Ref), !,
  clause(H,B,Ref), 
  print_rule(M,(H:-B)).

print_rule(M,(X:-True)):- True == true,!, print_rule(M,X).
print_rule(M,(learnt_rule(TestID,A,B,C,D):-Body)):- !,
   \+ \+ (( ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    pt(orange,M=[C=[TestID,in=(A),label=(B),out=(D)]]))).
print_rule(M,(X:-Body)):- !,
    \+ \+ ((  ignore((Body=was_once(InSet,InVars),maplist(upcase_atom_var,InSet,InVars))),   
    pt(orange,M=[X]))).
print_rule(M,O):- \+ \+ (( pt(orange,M=[O]))).

save_learnt_rule(TestID,In,InKey,RuleDir,Out):-
  save_learnt_rule(learnt_rule(TestID,In,InKey,RuleDir,Out)).

save_learnt_rule(RuleIn):- save_learnt_rule(RuleIn,RuleIn,RuleIn).
save_learnt_rule(RuleIn,InGoal,OutGoal):-  
  labels_for(InGoal,OutGoal,InSet),
  length(InSet,InLen),length(InVars,InLen),
  subst_rvars(InSet,InVars,RuleIn,NewRuleIn),!,
  Assert = (NewRuleIn:-was_once(InSet,InVars)), 
  assert_visually(Assert),!.

    

has_prop(P,Obj):- indv_props(Obj,Props),!,member(Q,Props), (Q=@=P -> true ; ( \+ Q \= P)).


learn_group(What,Objs):- assert_visually(group_associatable(What,Objs)).

learn_about_group(In):- 
  forall(group_keys(What),
   (group_group(What,In,Groups),
    maplist(learn_group(What),Groups))).

not_for_matching(Var):- var(Var),!.
not_for_matching(M):- too_unique(M).
not_for_matching(M):- too_non_unique(M).
not_for_matching(localpoints(_)).
not_for_matching(iz(combined)).
not_for_matching(link(_,_,_)).
not_for_matching(globalpoints(_)).
not_for_matching(iz(image)).



my_exclude(P1,I,O):- my_partition(P1,I,_,O).
simplify_for_matching(I,I):- ( \+ compound(I); I='$VAR'(_)), !.
simplify_for_matching(obj(I),obj([O|_])):- is_list(I),!, simplify_for_matching(I, O).
simplify_for_matching(I,O):- is_grid(I),O=I.
simplify_for_matching(I,O):- is_list(I), my_exclude(not_for_matching,I,M),I\=@=M,!,simplify_for_matching(M,O).
simplify_for_matching(I,O):- 
       compound_name_arguments(I, F, Args),
       maplist(simplify_for_matching, Args, ArgsNew),
       compound_name_arguments( O, F, ArgsNew ),!.



group_group(What,[Obj|In],[G1|Groups]):- indv_props(Obj,Props), member(P,Props),matches_key(P,What),!,
  [Obj|Include] = G1,
  my_partition(has_prop(P),In,Include,Exclude),
  group_group(What,Exclude,Groups).
group_group(_,_,[]).

group_keys(iz).
group_keys(mass).
group_keys(birth).
group_keys(color).
group_keys(shape).
group_keys(rotation).
group_keys(localpoints).
group_keys(symmetry).

matches_key(P,What):- atom(What),!,functor(P,What,_).
matches_key(P,P).

simplify_for_matching_nondet(I,O):- is_list(I), is_group(I), \+ is_grid(I), !, 
   group_keys(Key),
   group_group(Key,I,ListOfLists),
   list_to_set(ListOfLists,Set),
   member(M,Set),once(simplify_for_matching(M,O)).
simplify_for_matching_nondet(I,O):- simplify_for_matching(I,O).


learn_rule_o(out_out,_InVM,_OutVM):- !.
learn_rule_o(out_in,_InVM,_OutVM):- !.
learn_rule_o(in_in,_InVM,_OutVM):- !.
learn_rule_o(in_out,InVM,OutVM):- % is_map(InVM),is_map(OutVM),!,
 % extract_vm_props(InVM,InProps),     
 % extract_vm_props(OutVM,OutProps), 
 % prolog_pretty_print:print_term(Mode=learn_rule_o(Mode,InProps,OutProps),[fullstop(true),nl(true)]),!,
  %learn_grid_rules(Mode,InProps),
  %learn_grid_rules(Mode,OutProps),!,
  learn_rule_i_o(Mode,InVM.objs,OutVM.objs),
  %learn_rule_i_o(Mode,InVM.grid,OutVM.objs),
  %learn_rule_i_o(Mode,InVM.objs,OutVM.grid),
  learn_rule_i_o(Mode,InVM.grid,OutVM.grid),
  get_current_test(TestID),
  training_info(TestID,Info),
  length(Info,Len),
  ptc(orange,format('~N~n% Rules so far for ~w: ~w~n~n',[TestID,Len])),!,
  confirm_reproduction(InVM.objs,InVM.grid),
  confirm_reproduction(OutVM.objs,OutVM.grid),
  confirm_learned(InVM.grid,OutVM.grid).

learn_rule_i_o(Mode,In,Out):- 
  forall(learn_rule_in_out(Mode,In,Out),true).

confirm_reproduction(Objs,ExpectedOut):- 
   globalpoints(Objs,OGPoints),
   points_to_grid(OGPoints,Solution),
   show_result("Our Reproduction", Solution,ExpectedOut).

show_result(What,Solution,ExpectedOut):-
 get_current_test(TestID),
 ignore((count_difs(ExpectedOut,Solution,Errors),
   print_side_by_side(blue,Solution,What,_,ExpectedOut,"Expected"),
      (Errors==0 -> 
           arcdbg_info(green,pass(What,TestID))
         ; arcdbg_info(red,fail(What,Errors,TestID))))),
 task_info(TestID,InfoF),wqnl(fav(TestID,InfoF)),!.


arcdbg_info(Color, Info):- banner_lines(Color), arcdbg(Info), banner_lines(Color).

confirm_learned(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (use_test_associatable(Objs,Solution) -> 
   show_result("Our Solution", Solution,ExpectedOut)
   ; arcdbg_info(red,warn("No Solution"))).


show_proof(In,ExpectedOut):-
  individuate(complete,In,Objs),
  (test_associatable_proof(Objs,Solution) -> 
   show_result("Our Solution", Solution,ExpectedOut)
   ; arcdbg_info(red,warn("No Solution"))).

%learn_rule_o(_Mode,G,_):- is_list(G), is_group(G), learn_about_group(G), fail.
%learn_rule_o(_Mode,_,G):- is_list(G), is_group(G), learn_about_group(G), fail.

/*
learn_rule_in_out(Mode,In,Out):- is_list(In), is_list(Out), 
  \+ is_grid(In), \+ is_grid(Out),
  \+ is_group(In), \+ is_group(Out),
  forall(select_some(I,In,MoreIn),
   forall(select_some(O,Out,MoreOut),
    (learn_rule_in_out(Mode,MoreIn,MoreOut),
     learn_rule_in_out(Mode,I,O)))).
*/

%learn_rule_in_out(out_out,I,O):- !, ignore(( is_grid(O), save_learnt_rule(oout_associatable(I,O)))).

learn_rule_in_out(Mode,In,Out):- 
  is_list(In), is_list(Out), 
  maplist(compound,In), maplist(compound,Out),
  length(In,L), length(Out,L),
  maplist(learn_rule_in_out(Mode),In,Out).

learn_rule_in_out(Mode,In,Out):-
  forall(simplify_for_matching_nondet(In,InS),
    forall(simplify_for_matching_nondet(Out,OutS),
    learn_rule_in_out_now(Mode,InS,OutS))).

learn_rule_in_out(Mode,In,Out):- 
  is_group(In),is_group(Out),
  length(In,IL),length(Out,OL),
  fif((IL=<7,OL=<7),
   forall(member(I,In),
     forall(member(O,Out),
       learn_rule_in_out_now(Mode,I,O)))).

%learn_rule_in_out_now(Mode,_-In,Out):-!,learn_rule_in_out_now(Mode,In,Out).
%learn_rule_in_out_now(Mode,In,_-Out):-!,learn_rule_in_out_now(Mode,In,Out).
learn_rule_in_out_now(Mode,[In],[Out]):- \+ is_grid([In]), \+ is_grid([Out]), !, learn_rule_in_out_now(Mode,In,Out).
learn_rule_in_out_now(_Mode,In,Out):- is_list(In),is_list(Out), \+ is_grid(In), \+ is_grid(Out), length(In,L1), length(Out,L2), \+ (L1 is L2 ; (L1 is L2 * 2, L2>1); (L2 is L1 * 2, L1>1)),!.
learn_rule_in_out_now(Mode,In,Out):-  is_list(In),is_list(Out), 
  maplist(compound,In), maplist(compound,Out),
  length(In,L), length(Out,L),!,
  maplist(learn_rule_in_out_now(Mode),In,Out).
  %learn_rule_in_out(Mode,InS,OutS).
learn_rule_in_out_now(_Mode,In,Out):- save_learnt_rule(test_associatable(In,Out),In,Out).

%learn_rule_in_out(Mode,I,O):- save_learnt_rule(test_associatable(Mode,I,O)).

extract_vm_props(VM,[VM.grid,VM.objs]).

select_some(I,List,Rest):- append(_,[I|Rest],List).

learn_grid_rules(Mode,Props):-  
  forall(select_some(P,Props,Others),
    learn_rule_ee(Mode,P,Others)).
learn_rule_ee(Mode,P,Others):- forall(member(O,Others),learn_grid_local(Mode,P,O)).

learn_grid_local(Mode,P,O):- P @< O, !, learn_grid_local(Mode,O,P).
learn_grid_local(_Mode,P,O):- ignore((\+ is_grid(P),is_grid(O),assert_visually(grid_associatable(P,O)))).

:- dynamic(test_local_dyn/1).
test_local_dyn(learnt_rule).
test_local_dyn(grid_associatable).
test_local_dyn(test_associatable).
test_local_dyn(why_grouped).
test_local_dyn(cached_dictation).
test_local_dyn(oout_associatable).

assert_visually(H:-B):- !,unnumbervars((H:-B),(HH:-BB)), assert_visually1(HH,BB).
assert_visually( H  ):- unnumbervars(H,HH),assert_visually1(HH,true).

assert_visually1(H,B):- get_current_test(TestID), arg(1,H,W),W\==TestID,!, H=..[F|Args],GG=..[F,TestID|Args],assert_visually2(GG,B).
assert_visually1(H,B):- assert_visually2(H,B).

assert_visually2(H,B):- copy_term((H:-B),(HH:-BB)),clause(HH,BB,Ref), clause(RH,RB,Ref),(H:-B)=@=(RH:-RB) ,!,nop(pt(cyan,known_exact(H:-B))).
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
  retractall(individuated_cache(_,_,_)),
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

learn_rule(In,RuleDir,ROut):- nop(use_learnt_rule(In,RuleDir,ROut)).

/*
] dmiles: instead of a DSL about transformations i think the DSL would be one that creates the images.. then the transformation becomes about what properties of the generative DSL change (both the input and the output have their own starting points that are gleaned by looking at what DSL would generate all the outputs vs what DSL would generate all the inputs) the thing that is learned by training is how the edits are supposed to happen in each of the generative DSLs (edited)
[7:02 AM] dmiles: the progression of inputs teaches the system abotu what the input's generative DSL is used for inputs (edited)
[7:02 AM] dmiles: though the progression of outputs give more information about the total task (but still give the hints about the the output's generative DSL) 
round tripping between a grid and the generative DSL is seems important 
*/
properties_that_changed(Grid1,Grid2,Props):-
  individuate(complete,Grid1,Props1),
  individuate(complete,Grid2,Props2),
  diff_terms(Props1,Props2,Props).

has_learnt_rule(TestID,In,Key,RuleDir,Out):- clause(learnt_rule(TestID,In,Key,RuleDir,Out),was_once(InSet,InVars)),
  maplist(ignore_equal,InSet,InVars).
ignore_equal(X,Y):- ignore(X=Y).  


use_test_associatable(In,OutR):-
  findall(InS,simplify_for_matching_nondet(In,InS),InL),
   findall(OutS,(member(InS,InL),use_test_associatable_io(InS,OutS)),OutL),
    Out=[set],
     maplist(ignore_some_equals,OutL,Out),
     ignore(OutR=Out),!.

test_associatable_proof(In,OutR):-
  findall(InS,simplify_for_matching_nondet(In,InS),InL),
   findall(OutS,
     (member(InS,InL),use_test_associatable_io(InS,OutS),
      arcdbg_info(cyan,proof(InS->OutS))),OutL),
    Out=[set],
  maplist(ignore_some_equals,OutL,Out),
  ignore(OutR=Out),!.


ignore_some_equals(OutS,Out):- must_det_ll( nb_set_add1(OutS,Out)).

:- dynamic(test_associatable/3).
use_test_associatable_io(I,O):- get_current_test(TestID),test_associatable(TestID,I,O).

use_learnt_rule(In,_RuleDir,Out):- use_test_associatable(In,Out).

use_learnt_rule(In,RuleDir,ROut):- %get_vm(VM), %Target=VM.grid_out, 
 get_current_test(TestID),
  ignore(get_vm(last_key,Key)),
  ((has_learnt_rule(TestID,In,Key,RuleDir,Out);has_learnt_rule(TestID,_,Key,RuleDir,Out);has_learnt_rule(TestID,In,_,RuleDir,Out))),
  pt(orange,using_learnt_rule(In,Key,RuleDir,Out)),
  ignore(Out = ROut).

use_learnt_rule(In,RuleDir,Out):- get_vm(VM), % Target=VM.grid_out, 
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

labels_for(InGoal,OutGoal,Labels):- 
  findall(Atom,(sub_label(Atom,InGoal,OutGoal),maybe_unbind_label(Atom)),Atoms), 
  list_to_set(Atoms,Set),
  include(two_or_more(Atoms),Set,Labels).

two_or_more(Atoms,Label):- select(Label,Atoms,Rest),member(Label,Rest).

sub_label(X, X, OutGoal):- sub_var(X,OutGoal).
sub_label(X, Term, OutGoal) :-
    compound(Term),
    is_list(Term),
    \+ never_labels_in(Term),
    arg(_, Term, Arg),
    sub_label(X, Arg, OutGoal).

never_labels_in(iz(_)).
never_labels_in(shape(_)).
never_labels_in(loc(_,_)).


never_unbind_label(G):- var(G),!.
never_unbind_label(Int):- integer(Int), Int > 7 ; Int == 1 ; Int == 0.
never_unbind_label(true).
never_unbind_label(false).
never_unbind_label(G):- \+ atom(G),!,fail.
never_unbind_label(G):- atom_length(G,1),!.
never_unbind_label(G):- downcase_atom(G,D), upcase_atom(G,D).
never_unbind_label(G):- atom_chars(G,Cs),member(C,Cs),char_type(C,digit),!.

maybe_unbind_label(G):- var(G),!,fail.
maybe_unbind_label(G):- too_non_unique(G).
maybe_unbind_label(G):- never_unbind_label(G),!,fail.
maybe_unbind_label(G):- integer(G),!.
maybe_unbind_label(G):- \+ atom(G),!,fail.
maybe_unbind_label(G):- is_color(G).
%maybe_unbind_label(G):- downcase_atom(G,D),\+ upcase_atom(G,D).

subst_rvars([],[],A,A):-!. 
subst_rvars([F|FF],[R|RR],S,D):- debug_var(F,R),subst_rvars_1(F,R,S,M), subst_rvars(FF,RR,M,D).

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

find_by_shape(Grid,Find,Founds):- 
 get_vm(VM),
 v_hv(Find,GH,GV),
 decolorize(Find,F), 
 Prog = 
  (all_rotations(F,F1),
   %print_grid(F1),!,
   find_ogs(H,V,F1,Grid),% trace,

   grid_to_points(F1,GH,GV,Points),
   pt(Points),
   make_indiv_object(VM,[iz(find_by_shape),F1,loc(H,V),alt_grid_size(GH,GV)],Points,F2)),
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

