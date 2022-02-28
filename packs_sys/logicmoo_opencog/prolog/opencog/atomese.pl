/*
 * LOGICMOO CogServer Atomese Reader/Writer
 *
 * Copyright (c) 2022 Logicmoo Co <support@logicmoo.org>
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */
 :- module(cogserver_atomese, [read_atomese/2,read_atomese/1,run_opencog_example_tests/0]).


% read_atomese(X):- read(X).
read_atomese(X):- read_atomese(current_input, X).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_common)).
:- use_module(library(wamcl)).
:- use_module(library(wam_cl/sreader)).

outof_name(_,F,O):- O=F,!.
outof_name(P,F,O):- O=F:P,!.

into_name(I,O):- var(I),!,I=O.
into_name(I,O):- atom(I),!,I=O.
into_name(I,O):- is_list(I),!,catch(text_to_string(I,S),_,fail),into_name(S,O),!.
into_name([I],O):- !, into_name(I,O).
into_name('$STRING'(I),O):- into_name(I,O),!.
into_name(I,O):- compound(I),!,O=I.
into_name(I,O):- atom_concat('$',V,I),!,into_name(V,O).
into_name(I,O):- atom_concat('#',V,I),!,into_name(V,O).
into_name(I,O):- atom_concat('',O,I),!.

s_to_atomese(U,A):- once(s_to_atomese01(U,M)),U\=@=M,!,s_to_atomese(M,A).
s_to_atomese(U,U).

s_to_atomese01(I,O):- s_to_atomese0r(I,M),s_to_atomese1r(M,O).

s_to_atomese0r(U,A):- once(s_to_atomese0(U,M)),U\=@=M,!,s_to_atomese0r(M,A).
s_to_atomese0r(U,U).

s_to_atomese1r(U,A):- once(s_to_atomese1(U,M)),U\=@=M,!,s_to_atomese1r(M,A).
s_to_atomese1r(U,U).


s_to_atomese0(I,O):- var(I),!,I=O.
s_to_atomese0('$VAR'(I),'$VAR'(I)):-!.
s_to_atomese0(['Variable',N],'$VAR'(F)):- into_name(N,M),svar_fixvarname(M,F).
s_to_atomese0('$STRING'(I),O):- !, any_to_string(I,O).
s_to_atomese0([F|List],[stv,X,Y,O]):- select([stv,X,Y],List,NewList),maplist(s_to_atomese0,[F|NewList],O).
s_to_atomese0([F|List],[stv,X,Y,O]):- select(['SimpleTruthValue',X,Y],List,NewList),maplist(s_to_atomese0,[F|NewList],O).
s_to_atomese0(I,O):- is_list(I),maplist(s_to_atomese0,I,O),!.
s_to_atomese0(I,O):- compound(I),!,
  compound_name_arguments(I, F, ARGS), 
  s_to_atomese0(F,FF),
  maplist(s_to_atomese0, ARGS, ArgsO), 
  compound_name_arguments(O, FF, ArgsO),!.
s_to_atomese0(I,O):- \+ atom(I), I=O.
%s_to_atomese0(A,O):- atom_concat(C,'Link',A),!,s_to_atomese0(C,O).
s_to_atomese0(A,O):- atom_concat(C,'Node',A),!,s_to_atomese0(C,O).
s_to_atomese0(A,O):- atom_concat(C,'Link',A),atom_length(C,L),L>1,!,s_to_atomese0(C,O).
%s_to_atomese0('EvaluationLink','Evaluation').
s_to_atomese0(A,A).




s_prolog(A,N,O):- atom_concat(C,'Node',A),!,s_prolog(C,N,O).
%s_prolog(A,N,O):- atom_concat(C,'Link',A),!,s_prolog(C,N,O).
s_prolog('True',_,_):-!,fail.
s_prolog('List',_,_):-!,fail.
s_prolog('Schema',_,_):-!,fail.

s_prolog('Number',N,F):- atom_number(N,F),!.
s_prolog('Variable',N,'$VAR'(F)):- svar_fixvarname(N,F),!.
s_prolog('Predicate',N,N).
s_prolog('Concept',N,N).
%s_prolog(T,N,F):- atomic_list_concat([N,T],'_',F).
%s_prolog(T,N,F):- atomic_list_concat([N,T],'_',F).

s_to_atomese1(I,O):- \+ compound(I),!,I=O.
s_to_atomese1('Evaluation'(Pred,List),(O)):- compound(List),compound_name_arguments(List,'List',Args),atom(Pred), O=..[Pred|Args].
s_to_atomese1('Evaluation'(Pred,List),(O)):- atom(Pred), O=..[Pred,List].
s_to_atomese1([T,Name],F):- atom(T),into_name(Name,N),s_prolog(T,N,F),!.
s_to_atomese1(I,O):- \+ is_list(I),
  compound_name_arguments(I, F, ARGS), 
  maplist(s_to_atomese1, ARGS, ArgsO), 
  compound_name_arguments(O, F, ArgsO),!.
s_to_atomese1(I,O):- \+ is_list(I),I=O,!.
s_to_atomese1([F|I],O):- atom(F), pify(F), maplist(s_to_atomese1,I,M),O=..[F|M],!.
s_to_atomese1(I,O):- maplist(s_to_atomese1,I,O),!.
s_to_atomese1(O,O).

pify(stv).
pify(F):- \+ downcase_atom(F,F).

%s_to_forms(_,_):- source_location(F,L),writeln(source_location(F,L)),fail.
s_to_forms(S,A):- atom(S),exists_file(S),!,s_to_forms(file(S),A).
s_to_forms(file(S),A):- !, filematch(S,F), writeln(file(F)),open(F,read,SS),undo(close(SS)),s_to_forms(in(SS),A).
s_to_forms(in(S),A):- !,s_to_forms(S,A),ignore((stream_loc_info(S,L),write(L))).
%s_to_forms(S,A):- string(S),!,parse_sexpr_string(S,A).
s_to_forms(S,A):- nd_parse_sexpr_stream(S,U),must(to_untyped(U,A)).

nd_parse_sexpr_stream(S,U):- repeat, (at_end_of_stream(S)->(!,fail);true),parse_sexpr_stream(S,U).

stream_loc_info(S,L):- 
  ignore(stream_property(S,file_name(F))),  
  ignore(line_count(S,L)),
  ignore(line_position(S,C)), L=F:L:C.

read_atomese(S, A):- s_to_forms(S,A).

show_atomese(XX):- copy_term(XX,X),to_untyped(X,U),s_to_atomese(U,A),
 wdmsg(XX),ansicall(cyan,print_tree_nl(A)),!.
show_atomese(XX):- ansicall(red,print_tree_nl(XX)).

read_atomese_file(F,L):-  %open(F,read,S),
   findall(X,(read_atomese(F,X),show_atomese(X)),L).


% will change later to what we consider "enough" ground
opencog_z_ground(G):-  ground(G).

opencog_string(Name):- atom(Name) ; string(Name).

atomspace_examples:atomspace_example_test(read_atomese_file(File,O),is_list(O)):-
  filematch(library('../pln/*/*/*/*.scm'),File);
  filematch(library('../pln/*/*/*.scm'),File).
  
atomspace_examples:atomspace_example_test(read_atomese_file(File,O),is_list(O)):- fail,
  (filematch(library('../*/*/*/*.scm'),File);
   filematch(library('../*/*/*.scm'),File);
   filematch(library('../*/*/*/*/*.scm'),File);
   filematch(library('../*/*/*/*/*/*.scm'),File);
   filematch(library('../*/*/*/*/*/*/*.scm'),File);
   filematch(library('../*/*/*/*/*/*/*/*.scm'),File);
   filematch(library('../*/*/*/*/*/*/*/*/*.scm'),File)),
  %\+ atom_contains(File,'deduction-engine'),
  %\+ atom_contains(File,'chicken-feet-or-pizza'), %1230 Multibyte
  % C:/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/ure/tests/ure/rules/implication-instantiation-rule.scm:1098
  %\+ atom_contains(File,'/guile'),
  %/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/guile-log/ice-9/set/basic.scm
  %\+ atom_contains(File,'/implication-instantiation-rule'),
  %\+ atom_contains(File,'/opencog/scm/opencog/base/file-utils'),
  %C:/opt/logicmoo_workspace/packs_sys/logicmoo_opencog/pln/opencog/pln/rules/wip/decontextualize.scm:321
  \+ atom_contains(File,'/guile-log/'),
  \+ atom_contains(File,'/ice-9/'),  
  \+ atom_contains(File,'/unused/'),
  \+ atom_contains(File,'/build/'),
  \+ atom_contains(File,'/wam_common_lisp/').
  
  



do_opencog_example_tests:- 
  run_opencog_example_tests.

run_opencog_example_tests:-
 make,
  add_history(run_opencog_example_tests),
  forall(atomspace_examples:atomspace_example_test(Goal, Results),
         take_atomspace_example_test(Goal, Results)).


baseKB:sanity_test:- do_opencog_example_tests.

baseKB:feature_test:- run_opencog_example_tests.
/*
```diff
- text in red
+ text in green
! text in orange
# text in gray
@@ text in purple (and bold)@@
```
*/
%take_atomspace_example_test(_,_):- make, fail.
atomspace_into_prolog:- 
  make,
  forall((atomspace_is_test(Type, Text),
  Type == into_prolog),
  atomspace_do_exec_test(Type,Text)).


atomspace_do_exec_test(_Type,Text):- atomspace_do_exec_test_text(Text).

atomspace_do_exec_test_text(Text):-
  atomspace_read_clauses(Text,Clauses),
  opencog_exec_ex(in,Clauses).

opencog_exec_ex(Clauses):-
  opencog_exec_ex(in,Clauses).

opencog_exec_ex(_,[]):-!.
opencog_exec_ex(IO,[C|Clauses]):- !,
  opencog_exec_ex(IO,C),
  opencog_exec_ex(IO,Clauses).
opencog_exec_ex(IO,C):- atomese_to_prolog(IO,C,P),!,opencog_exec_ex1(IO,P).
opencog_exec_ex(IO,C):- opencog_exec_ex1(IO,C).


opencog_exec_ex1(_,[]):-!.
opencog_exec_ex1(IO,[C|Clauses]):- !,
  opencog_exec_ex1(IO,C),
  opencog_exec_ex1(IO,Clauses).
opencog_exec_ex1(IO,nop(C)):-!,opencog_exec_ex1(IO,C).
opencog_exec_ex1(_,atomspace_in(C,_V3)):-!,opencog_exec_ex1(in,C).
opencog_exec_ex1(_,atomspace_out(C,_V3)):-!,opencog_exec_ex1(out,C).
opencog_exec_ex1(_,outputMustContain(C)):- !, opencog_exec_ex1(out,C).
opencog_exec_ex1(_,expected(C)):- !, opencog_exec_ex1(out,C).
opencog_exec_ex1(_,write(C)):- !, opencog_exec_ex1(cmt,C).
opencog_exec_ex1(IO,C):- dmsg(IO=C),fail.
opencog_exec_ex1(_,do_steps(N)):- !, forall(between(1,N,_),nop(inference_step(_))),!.
%opencog_exec_ex1(IO,task(judgement,C,_,TV,_)):- !, opencog_exec_ex1(IO,C).
opencog_exec_ex1(in,C):- assert(opencog_db(C)), nop(derive_event(C)),!.
opencog_exec_ex1(_,_).


atomspace_exec_tests:- make,
  forall((atomspace_reader:atomspace_is_test(Type, Text), 
         Type \== read),
  atomspace_do_exec_test(Type,Text)).


atomspace_clauses_to_test(Clauses,Goal,[ResultsExpected]):-
  atomspace_clauses_to_test(Clauses,true,Goal,true,ResultsExpected).

atomspace_clauses_to_test([],Goal,Goal,InOut,InOut):- !.
atomspace_clauses_to_test([C|Cs],Goal,PGoal,In,Out):-!,
  atomspace_clauses_to_test(C,Goal,MGoal,In,Mid),!,
  atomspace_clauses_to_test(Cs,MGoal,PGoal,Mid,Out).
atomspace_clauses_to_test(expected(C),Goal,Goal,In,Out):- !,
  atomese_to_prolog(out,C,P),conjoin_op(';',In,P,Out).
atomspace_clauses_to_test(outputMustContain(C),Goal,Goal,In,Out):- !,
  atomese_to_prolog(out,C,P),conjoin_op(';',In,P,Out).
atomspace_clauses_to_test(atomspace_out(C,W),Goal,Goal,In,Out):- !,
  atomese_to_prolog(out,atomspace_out(C,W),P),conjoin(In,P,Out).
atomspace_clauses_to_test(atomspace_in(C,W),Goal,PGoal,InOut,InOut):- !,
  atomese_to_prolog(in,atomspace_in(C,W),P),conjoin(Goal,P,PGoal).
atomspace_clauses_to_test(CMT,Goal,Goal,In,Out):- In\==true, CMT = '$COMMENT'(_,_,_),!,
  atomese_to_prolog(out,CMT,P),conjoin(In,P,Out).
atomspace_clauses_to_test(C,Goal,PGoal,InOut,InOut):- atomese_to_prolog(in,C,P),conjoin(Goal,P,PGoal),!.

% atomese_to_prolog(IO,'$COMMENT'(C,_,_),writeln(C)):- !.
%atomese_to_prolog(IO,'$COMMENT'(_,_,_),true):- !.
atomese_to_prolog(IO,'$COMMENT'(C,_,_),M):- !,atomese_to_prolog(IO,write(C),M).
atomese_to_prolog(IO,atomspace_in(C,_),M):- !,atomese_to_prolog(IO,C,M).
atomese_to_prolog(IO,atomspace_out(C,_),M):- !,atomese_to_prolog(IO,C,M).
atomese_to_prolog(IO,task(X,S,T,O,B),M):- \+ ground(O),
  O = [C,F],
  ignore(C=1.0),
  ignore(F=1.0),
  atomese_to_prolog(IO,task(X,S,T,O,B),M).
atomese_to_prolog(IO,task(X,S,T,O,B),M):- var(T), T = present, atomese_to_prolog(IO,task(X,S,T,O,B),M).
atomese_to_prolog(IO,task(judgement,S,_,FC,_),O):- append_term(S,FC,M),atomese_to_prolog(IO,M,O).
atomese_to_prolog(in,X,nop(X)).
atomese_to_prolog(out,X,nop(expected(X))).


take_atomspace_example_test(TFR, ResultsExpected):- forall(TFR,must(ResultsExpected)).

take_atomspace_example_test(Goal, ResultsExpected):-
  Failed = _,
  wots(S, ignore((take_atomspace_example_test_node(Goal, ResultsExpected, Failed)))),
  (Failed==failed->ansifmt([red],S);ansifmt([green],S)).

take_atomspace_example_test_node(Goal, [Results;Expected],Failed):- !,
   take_atomspace_example_test_node(Goal, [Results],Failed),
   take_atomspace_example_test_node(Goal, [Expected],Failed).

take_atomspace_example_test_node(Goal, ResultsExpected, Failed):-   
 guess_pretty(take_atomspace_example_test(Goal, ResultsExpected)),
 format('~N~n```prolog~nTEST: ?- ~@~n```',[print_tree(Goal)]),
  % term_variables(Goal,Vs),
  maplist([R]>>format('~NEXPECTED: `~@`',[print_tree(R)]), ResultsExpected),
  take_atomspace_example_test_result(Goal, ResultsExpected, Failed).

take_atomspace_example_test_result(Goal, ResultsExpected, Failed):-
  Failed = _,
 (( \+ \+ (opencog_call_ex(Goal),opencog_z_check_results(ResultsExpected)))
    -> (format('~N~n```diff~n+SUCCESS!~n```',[]))
     ; format('~N~n```diff~n-FAILED!~n```',[]),Failed=failed),!,
  format('~n~n```prolog ',[]),
  mu:print_tree_nl(ResultsExpected),
  format('```~n~n',[]),!.

  



opencog_call_ex((X,Y)):- !, opencog_call_ex(X), opencog_call_ex(Y).
opencog_call_ex((X;Y)):- !, opencog_call_ex(X); opencog_call_ex(Y).
%opencog_call_ex(X=Y):- !, opencog_close_enough(X,Y). %-> true; (print_tree(failure(X=Y)),!,fail).
opencog_call_ex(L):- is_list(L),!, maplist(opencog_call_ex,L).
opencog_call_ex(X):- call(X)*-> true ; nop(print_tree_nl(failed_opencog_call_ex(X))),fail.


opencog_z_check_results(L):- is_list(L),!, maplist(opencog_z_check_results,L).
opencog_z_check_results((R1;R2)):- !, opencog_z_check_results(R1); opencog_z_check_results(R2).
opencog_z_check_results((R1,RS)):- !, opencog_z_check_results(R1), opencog_z_check_results(RS).
opencog_z_check_results(R=V):- !, R=V*->true;opencog_close_enough(R,V).
opencog_z_check_results(X):- call(X)*-> true ; (fail, print_tree(test_failed(X)),fail).

opencog_close_enough(R,V):- R=@=V, !.
opencog_close_enough(R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
opencog_close_enough(R,V):- (\+ compound(R) ; \+ compound(V)),!, R==V.
opencog_close_enough([R|RT],[V|VT]):- !, opencog_close_enough(R,V),opencog_close_enough(RT,VT).
opencog_close_enough(R,V):- 
  compound_name_arguments(R,F,RA),
  compound_name_arguments(V,F,VA), !,
  maplist(opencog_close_enough,RA,VA).



%like to distinguish "eaten by tiger" from "eating tiger" (/, eat, tiger, _) vs. (/, eat, _, tiger)
%now: (eat /2 tiger) vs. (eat /1 tiger)

use_opencog_config_info(List):-  is_list(List), !, maplist(use_opencog_config_info, List).
use_opencog_config_info(element(_, [], List)):- !, use_opencog_config_info(List).
use_opencog_config_info(element(_, [name=Name, value=Value], _)):- !, use_opencog_config_info(Name=Value).
use_opencog_config_info(Name=Value):- opencog_string(Name), downcase_atom(Name, NameD), NameD\=Name, !, use_opencog_config_info(NameD=Value).
use_opencog_config_info(Name=Value):- opencog_string(Value), downcase_atom(Value, ValueD), ValueD\=Value, !, use_opencog_config_info(Name=ValueD).
use_opencog_config_info(Name=Value):-  atom(Value), atom_number(Value, Number), use_opencog_config_info(Name=Number).
%use_opencog_config_info(Ignore):- opencog_string(Ignore), !.
%use_opencog_config_info(NameValue):-  dmsg(use_opencog_config_info(NameValue)), fail.
%use_opencog_config_info(Name=Value):-  number(Value), !, nb_setval(Name, Value).
use_opencog_config_info(Name=Value):-  nb_setval(Name, Value).
use_opencog_config_info(_).


% default gobals
:-  use_opencog_config_info(volume=100).
:-  use_opencog_config_info(novelty_horizon=100000).
:-  use_opencog_config_info(decision_threshold=0.51).
:-  use_opencog_config_info(concept_bag_size=80000).
:-  use_opencog_config_info(concept_bag_levels=1000).
:-  use_opencog_config_info(duration=5).
:-  use_opencog_config_info(horizon=1).
:-  use_opencog_config_info(truth_epsilon=0.01).
:-  use_opencog_config_info(budget_epsilon=0.0001).
:-  use_opencog_config_info(budget_threshold=0.01).
:-  use_opencog_config_info(default_confirmation_expectation=0.6).
:-  use_opencog_config_info(always_create_concept=true).
:-  use_opencog_config_info(default_creation_expectation=0.66).
:-  use_opencog_config_info(default_creation_expectation_goal=0.6).
:-  use_opencog_config_info(default_judgment_confidence=0.9).
:-  use_opencog_config_info(default_judgment_priority=0.8).
:-  use_opencog_config_info(default_judgment_durability=0.5).
:-  use_opencog_config_info(default_question_priority=0.9).
:-  use_opencog_config_info(default_question_durability=0.9).
:-  use_opencog_config_info(default_goal_confidence=0.9).
:-  use_opencog_config_info(default_goal_priority=0.9).
:-  use_opencog_config_info(default_goal_durability=0.9).
:-  use_opencog_config_info(default_quest_priority=0.9).
:-  use_opencog_config_info(default_quest_durability=0.9).
:-  use_opencog_config_info(bag_threshold=1.0).
:-  use_opencog_config_info(forget_quality_relative=0.3).
:-  use_opencog_config_info(revision_max_occurrence_distance=10).
:-  use_opencog_config_info(task_link_bag_size=100).
:-  use_opencog_config_info(task_link_bag_levels=10).
:-  use_opencog_config_info(term_link_bag_size=100).
:-  use_opencog_config_info(term_link_bag_levels=10).
:-  use_opencog_config_info(term_link_max_matched=10).
:-  use_opencog_config_info(novel_task_bag_size=1000).
:-  use_opencog_config_info(novel_task_bag_levels=100).
:-  use_opencog_config_info(novel_task_bag_selections=100).
:-  use_opencog_config_info(sequence_bag_size=30).
:-  use_opencog_config_info(sequence_bag_levels=10).
:-  use_opencog_config_info(operation_bag_size=10).
:-  use_opencog_config_info(operation_bag_levels=10).
:-  use_opencog_config_info(operation_samples=6).
:-  use_opencog_config_info(projection_decay=0.1).
:-  use_opencog_config_info(maximum_evidental_base_length=20000).
:-  use_opencog_config_info(termlink_max_reasoned=3).
:-  use_opencog_config_info(term_link_record_length=10).
:-  use_opencog_config_info(concept_beliefs_max=28).
:-  use_opencog_config_info(concept_questions_max=5).
:-  use_opencog_config_info(concept_goals_max=7).
:-  use_opencog_config_info(reliance=0.9).
:-  use_opencog_config_info(discount_rate=0.5).
:-  use_opencog_config_info(immediate_eteratomspaceization=true).
:-  use_opencog_config_info(sequence_bag_attempts=10).
:-  use_opencog_config_info(condition_bag_attempts=10).
:-  use_opencog_config_info(derivation_priority_leak=0.4).
:-  use_opencog_config_info(derivation_durability_leak=0.4).
:-  use_opencog_config_info(curiosity_desire_confidence_mul=0.1).
:-  use_opencog_config_info(curiosity_desire_priority_mul=0.1).
:-  use_opencog_config_info(curiosity_desire_durability_mul=0.3).
:-  use_opencog_config_info(curiosity_for_operator_only=false).
:-  use_opencog_config_info(break_atomspace_hol_boundary=false).
:-  use_opencog_config_info(question_generation_on_decision_making=false).
:-  use_opencog_config_info(how_question_generation_on_decision_making=false).
:-  use_opencog_config_info(anticipation_confidence=0.1).
:-  use_opencog_config_info(anticipation_tolerance=100.0).
:-  use_opencog_config_info(retrospective_anticipations=false).
:-  use_opencog_config_info(satisfaction_treshold=0.0).
:-  use_opencog_config_info(complexity_unit=1.0).
:-  use_opencog_config_info(interval_adapt_speed=4.0).
:-  use_opencog_config_info(tasklink_per_content=4).
:-  use_opencog_config_info(default_feedback_priority=0.9).
:-  use_opencog_config_info(default_feedback_durability=0.5).
:-  use_opencog_config_info(concept_forget_durations=2.0).
:-  use_opencog_config_info(termlink_forget_durations=10.0).
:-  use_opencog_config_info(tasklink_forget_durations=4.0).
:-  use_opencog_config_info(event_forget_durations=4.0).
:-  use_opencog_config_info(variable_introduction_combinations_max=8).
:-  use_opencog_config_info(variable_introduction_confidence_mul=0.9).
:-  use_opencog_config_info(anticipations_per_concept_max=8).
:-  use_opencog_config_info(motor_babbling_confidence_threshold=0.8).
:-  use_opencog_config_info(threads_amount=1).
:-  use_opencog_config_info(milliseconds_per_step=0).
:-  use_opencog_config_info(steps_clock=true).
:-  use_opencog_config_info(derivation_durability_leak=0.4).
:-  use_opencog_config_info(derivation_priority_leak=0.4).

use_opencog_config(File):-  (\+ atom(File); \+ is_absolute_file_name(File)),
   absolute_file_name(File, Absolute), !, use_opencog_config(Absolute).
use_opencog_config(Absolute):-  open(Absolute, read, In),
    load_sgml(In, Dom,
                   [  dialect(html5),
                      attribute_value(string),
                      cdata(string),
                     opencog_system_entities(true),
                     opencog_space(remove),
                     opencog_syntax_errors(quiet),
                      case_preserving_attributes(false),
                      case_sensitive_attributes(false),
                   max_errors(-1)]), !,
    close(In),
    use_opencog_config_info(Dom), !.

 parse_config:-
   use_opencog_config(library('../config/mvpConfig.xml')).


