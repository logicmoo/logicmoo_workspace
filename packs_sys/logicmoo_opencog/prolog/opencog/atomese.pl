% atomese.pl

:- module(atomese, [read_atomese/2,read_atomese/1,run_opencog_example_tests/0]).


% read_atomese(X):- read(X).
read_atomese(X):- read_atomese(current_input, X).

:- set_module(class(library)).
:- set_module(base(system)).
:- use_module(library(logicmoo_common)).
:- use_module(library(wam_cl/sreader)).


read_atomese(S, A):- input_to_forms(S,X),s_to_atomese(X,A).

read_atomese_file(F, L):-  open(F,read,S),
   findall(X,read_atomese(S,X),L),dmsg(L).


% will change later to what we consider "enough" ground
opencog_z_ground(G):-  ground(G).

opencog_string(Name):- atom(Name) ; string(Name).

atomspace_examples:atomspace_example_test(read_atomese_file(File,O),is_list(O)):-
  filematch(library('../pln/tests/pln/rules/*.scm'),File).



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


