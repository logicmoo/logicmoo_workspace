
% nal.pl
% Non-Axiomatic Logic in Prolog
% Version: 1.1, September 2012
% GNU Lesser General Public License
% Author: Pei Wang

:-module(nal,[]).

:- set_module(class(library)).
:- set_module(base(system)).


use_nars_config_info(List):- is_list(List),!,maplist(use_nars_config_info, List).
use_nars_config_info([]):-!.
use_nars_config_info(element(_,[], List)):-!, use_nars_config_info(List).
use_nars_config_info(element(_,[name=Name,value=Value], _)):-!,
 use_nars_config_info(Name=Value).
use_nars_config_info(Name=Value):-  string(Name), downcase_atom(Name,NameD),   NameD\=Name,!,use_nars_config_info(NameD=Value).
use_nars_config_info(Name=Value):- string(Value), downcase_atom(Value,ValueD),ValueD\=Value,!,use_nars_config_info(Name=ValueD).
use_nars_config_info(Name=Value):- atom(Value), atom_number(Value,Number), use_nars_config_info(Name=Number).
%use_nars_config_info(Ignore):- string(Ignore),!.
%use_nars_config_info(NameValue):- dmsg(use_nars_config_info(NameValue)),fail.
use_nars_config_info(Name=Value):- number(Value), !, nb_setval(Name,Value).
use_nars_config_info(Name=Value):- nb_setval(Name, Value).
use_nars_config_info(_).

% default gobals
:-  use_nars_config_info(volume=100).
:-  use_nars_config_info(novelty_horizon=100000).
:-  use_nars_config_info(decision_threshold=0.51).
:-  use_nars_config_info(concept_bag_size=80000).
:-  use_nars_config_info(concept_bag_levels=1000).
:-  use_nars_config_info(duration=5).
:-  use_nars_config_info(horizon=1).
:-  use_nars_config_info(truth_epsilon=0.01).
:-  use_nars_config_info(budget_epsilon=0.0001).
:-  use_nars_config_info(budget_threshold=0.01).
:-  use_nars_config_info(default_confirmation_expectation=0.6).
:-  use_nars_config_info(always_create_concept=true).
:-  use_nars_config_info(default_creation_expectation=0.66).
:-  use_nars_config_info(default_creation_expectation_goal=0.6).
:-  use_nars_config_info(default_judgment_confidence=0.9).
:-  use_nars_config_info(default_judgment_priority=0.8).
:-  use_nars_config_info(default_judgment_durability=0.5).
:-  use_nars_config_info(default_question_priority=0.9).
:-  use_nars_config_info(default_question_durability=0.9).
:-  use_nars_config_info(default_goal_confidence=0.9).
:-  use_nars_config_info(default_goal_priority=0.9).
:-  use_nars_config_info(default_goal_durability=0.9).
:-  use_nars_config_info(default_quest_priority=0.9).
:-  use_nars_config_info(default_quest_durability=0.9).
:-  use_nars_config_info(bag_threshold=1.0).
:-  use_nars_config_info(forget_quality_relative=0.3).
:-  use_nars_config_info(revision_max_occurrence_distance=10).
:-  use_nars_config_info(task_link_bag_size=100).
:-  use_nars_config_info(task_link_bag_levels=10).
:-  use_nars_config_info(term_link_bag_size=100).
:-  use_nars_config_info(term_link_bag_levels=10).
:-  use_nars_config_info(term_link_max_matched=10).
:-  use_nars_config_info(novel_task_bag_size=1000).
:-  use_nars_config_info(novel_task_bag_levels=100).
:-  use_nars_config_info(novel_task_bag_selections=100).
:-  use_nars_config_info(sequence_bag_size=30).
:-  use_nars_config_info(sequence_bag_levels=10).
:-  use_nars_config_info(operation_bag_size=10).
:-  use_nars_config_info(operation_bag_levels=10).
:-  use_nars_config_info(operation_samples=6).
:-  use_nars_config_info(projection_decay=0.1).
:-  use_nars_config_info(maximum_evidental_base_length=20000).
:-  use_nars_config_info(termlink_max_reasoned=3).
:-  use_nars_config_info(term_link_record_length=10).
:-  use_nars_config_info(concept_beliefs_max=28).
:-  use_nars_config_info(concept_questions_max=5).
:-  use_nars_config_info(concept_goals_max=7).
:-  use_nars_config_info(reliance=0.9).
:-  use_nars_config_info(discount_rate=0.5).
:-  use_nars_config_info(immediate_eternalization=true).
:-  use_nars_config_info(sequence_bag_attempts=10).
:-  use_nars_config_info(condition_bag_attempts=10).
:-  use_nars_config_info(derivation_priority_leak=0.4).
:-  use_nars_config_info(derivation_durability_leak=0.4).
:-  use_nars_config_info(curiosity_desire_confidence_mul=0.1).
:-  use_nars_config_info(curiosity_desire_priority_mul=0.1).
:-  use_nars_config_info(curiosity_desire_durability_mul=0.3).
:-  use_nars_config_info(curiosity_for_operator_only=false).
:-  use_nars_config_info(break_nal_hol_boundary=false).
:-  use_nars_config_info(question_generation_on_decision_making=false).
:-  use_nars_config_info(how_question_generation_on_decision_making=false).
:-  use_nars_config_info(anticipation_confidence=0.1).
:-  use_nars_config_info(anticipation_tolerance=100.0).
:-  use_nars_config_info(retrospective_anticipations=false).
:-  use_nars_config_info(satisfaction_treshold=0.0).
:-  use_nars_config_info(complexity_unit=1.0).
:-  use_nars_config_info(interval_adapt_speed=4.0).
:-  use_nars_config_info(tasklink_per_content=4).
:-  use_nars_config_info(default_feedback_priority=0.9).
:-  use_nars_config_info(default_feedback_durability=0.5).
:-  use_nars_config_info(concept_forget_durations=2.0).
:-  use_nars_config_info(termlink_forget_durations=10.0).
:-  use_nars_config_info(tasklink_forget_durations=4.0).
:-  use_nars_config_info(event_forget_durations=4.0).
:-  use_nars_config_info(variable_introduction_combinations_max=8).
:-  use_nars_config_info(variable_introduction_confidence_mul=0.9).
:-  use_nars_config_info(anticipations_per_concept_max=8).
:-  use_nars_config_info(motor_babbling_confidence_threshold=0.8).
:-  use_nars_config_info(threads_amount=1).
:-  use_nars_config_info(milliseconds_per_step=0).
:-  use_nars_config_info(steps_clock=true).
:-  use_nars_config_info(derivation_durability_leak=0.4).
:-  use_nars_config_info(derivation_priority_leak=0.4).

use_nars_config(File):- (\+ atom(File); \+ is_absolute_file_name(File)),
  absolute_file_name(File,Absolute), !, use_nars_config(Absolute).
use_nars_config(Absolute):- open(Absolute,read,In),
   load_sgml(In, Dom,
                  [  dialect(html5),
                     attribute_value(string),
                     cdata(string),
                     system_entities(true),
                     space(remove),
                     syntax_errors(quiet),
                     case_preserving_attributes(false),
                     case_sensitive_attributes(false),
                  max_errors(-1)]),!,
   close(In),
   use_nars_config_info(Dom),!.

parse_config:-
  use_nars_config(library('../config/mvpConfig.xml')).



% This program covers the inference rules of upto NAL-6 in
% "Non-Axiomatic Logic: A Model of Intelligent Reasoning"
% For the details of syntax, see the "User's Guide of NAL"

%%% individual inference rules

% There are three types of inference rules in NAL:
% (1) "revision" merges its two premises into a conclusion;
% (2) "choice" selects one of its two premises as a conclusion;
% (3) "inference" generates a conclusion from one or two premises.

% revision

revision([S, T1], [S, T2], [S, T]) :-
	nal_f_rev(T1, T2, T).

% choice

choice([S, [F1, C1]], [S, [_F2, C2]], [S, [F1, C1]]) :-
	C1 >= C2, !.
choice([S, [_F1, C1]], [S, [F2, C2]], [S, [F2, C2]]) :-
	C1 < C2, !.
choice([S1, T1], [S2, T2], [S1, T1]) :-
	S1 \= S2, nal_f_exp(T1, E1), nal_f_exp(T2, E2), E1 >= E2, !.
choice([S1, T1], [S2, T2], [S2, T2]) :-
	S1 \= S2, nal_f_exp(T1, E1), nal_f_exp(T2, E2), E1 < E2, !.

% simplified version

infer(T1, T) :- inference([T1, [1, 0.9]], T).

infer(inheritance(W1, ext_image(ext_image(represent, [nil, inheritance(product([X, T2]), R)]), [nil, W2, W3])), inheritance(W1, ext_image(represent, [nil, X])), [inheritance(ext_image(represent, [nil, Y]), ext_image(ext_image(represent, [nil, inheritance(product([Y, T2]), R)]), [nil, W2, W3])), V])  :- nal_f_ind([1, 0.9], [1, 0.9], V), !.

infer(inheritance(W3, ext_image(ext_image(represent, [nil, inheritance(product([T1, X]), R)]), [W1, W2, nil])), inheritance(W3, ext_image(represent, [nil, X])), [inheritance(ext_image(represent, [nil, Y]), ext_image(ext_image(represent, [nil, inheritance(product([T1, Y]), R)]), [W1, W2, nil])), V])  :- nal_f_ind([1, 0.9], [1, 0.9], V), !.

infer(T1, T2, T) :- inference([T1, [1, 0.9]], [T2, [1, 0.9]], T).


% inference/2

%% immediate inference

inference([inheritance(S, P), T1], [inheritance(P, S), T]) :-
	nal_f_cnv(T1, T).
inference([implication(S, P), T1], [implication(P, S), T]) :-
	nal_f_cnv(T1, T).
inference([implication(negation(S), P), T1], [implication(negation(P), S), T]) :-
	nal_f_cnt(T1, T).

inference([negation(S), T1], [S, T]) :-
	nal_f_neg(T1, T).
inference([S, [F1, C1]], [negation(S), T]) :-
	F1 < 0.5, nal_f_neg([F1, C1], T).

%% structural inference

inference([S1, T], [S, T]) :-
	nal_reduce(S1, S), S1 \== S, !.
inference([S1, T], [S, T]) :-
	equivalence(S1, S); equivalence(S, S1).

inference(P, C) :-
	inference(P, [S, [1, 1]], C), call(S).
inference(P, C) :-
	inference([S, [1, 1]], P, C), call(S).


% inference/3

%% inheritance-based syllogism

inference([inheritance(M, P), T1], [inheritance(S, M), T2], [inheritance(S, P), T]) :-
	S \= P, nal_f_ded(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(S, P), T]) :-
	S \= P, nal_f_abd(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(S, P), T]) :-
	S \= P, nal_f_ind(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(M, S), T2], [inheritance(S, P), T]) :-
	S \= P, nal_f_exe(T1, T2, T).

%% similarity from inheritance

inference([inheritance(S, P), T1], [inheritance(P, S), T2], [similarity(S, P), T]) :-
	nal_f_int(T1, T2, T).

%% similarity-based syllogism

inference([inheritance(P, M), T1], [inheritance(S, M), T2], [similarity(S, P), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [similarity(S, P), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([inheritance(M, P), T1], [similarity(S, M), T2], [inheritance(S, P), T]) :-
	S \= P, nal_f_ana(T1, T2, T).
inference([inheritance(P, M), T1], [similarity(S, M), T2], [inheritance(P, S), T]) :-
	S \= P, nal_f_ana(T1, T2, T).
inference([similarity(M, P), T1], [similarity(S, M), T2], [similarity(S, P), T]) :-
	S \= P, nal_f_res(T1, T2, T).

%% inheritance-based composition

inference([inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]) :-
	S \= P, nal_reduce(int_intersection([P, S]), N), nal_f_int(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]) :-
	S \= P, nal_reduce(ext_intersection([P, S]), N), nal_f_uni(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]) :-
	S \= P, nal_reduce(int_difference(P, S), N), nal_f_dif(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]) :-
	S \= P, nal_reduce(ext_intersection([P, S]), N), nal_f_int(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]) :-
	S \= P, nal_reduce(int_intersection([P, S]), N), nal_f_uni(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]) :-
	S \= P, nal_reduce(ext_difference(P, S), N), nal_f_dif(T1, T2, T).

%% inheirance-based decomposition

inference([inheritance(S, M), T1], [inheritance(int_intersection(L), M), T2], [inheritance(P, M), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(int_intersection(N), P), nal_f_pnn(T1, T2, T).
inference([inheritance(S, M), T1], [inheritance(ext_intersection(L), M), T2], [inheritance(P, M), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(ext_intersection(N), P), nal_f_npp(T1, T2, T).
inference([inheritance(S, M), T1], [inheritance(int_difference(S, P), M), T2], [inheritance(P, M), T]) :-
	atom(S), atom(P), nal_f_pnp(T1, T2, T).
inference([inheritance(S, M), T1], [inheritance(int_difference(P, S), M), T2], [inheritance(P, M), T]) :-
	atom(S), atom(P), nal_f_nnn(T1, T2, T).
inference([inheritance(M, S), T1], [inheritance(M, ext_intersection(L)), T2], [inheritance(M, P), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(ext_intersection(N), P), nal_f_pnn(T1, T2, T).
inference([inheritance(M, S), T1], [inheritance(M, int_intersection(L)), T2], [inheritance(M, P), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(int_intersection(N), P), nal_f_npp(T1, T2, T).
inference([inheritance(M, S), T1], [inheritance(M, ext_difference(S, P)), T2], [inheritance(M, P), T]) :-
	atom(S), atom(P), nal_f_pnp(T1, T2, T).
inference([inheritance(M, S), T1], [inheritance(M, ext_difference(P, S)), T2], [inheritance(M, P), T]) :-
	atom(S), atom(P), nal_f_nnn(T1, T2, T).

%% implication-based syllogism

inference([implication(M, P), T1], [implication(S, M), T2], [implication(S, P), T]) :-
	S \= P, nal_f_ded(T1, T2, T).
inference([implication(P, M), T1], [implication(S, M), T2], [implication(S, P), T]) :-
	S \= P, nal_f_abd(T1, T2, T).
inference([implication(M, P), T1], [implication(M, S), T2], [implication(S, P), T]) :-
	S \= P, nal_f_ind(T1, T2, T).
inference([implication(P, M), T1], [implication(M, S), T2], [implication(S, P), T]) :-
	S \= P, nal_f_exe(T1, T2, T).

%% implication to equivalence

inference([implication(S, P), T1], [implication(P, S), T2], [equivalence(S, P), T]) :-
	nal_f_int(T1, T2, T).

%% equivalence-based syllogism

inference([implication(P, M), T1], [implication(S, M), T2], [equivalence(S, P), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([implication(M, P), T1], [implication(M, S), T2], [equivalence(S, P), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([implication(M, P), T1], [equivalence(S, M), T2], [implication(S, P), T]) :-
	S \= P, nal_f_ana(T1, T2, T).
inference([implication(P, M), T1], [equivalence(S, M), T2], [implication(P, S), T]) :-
	S \= P, nal_f_ana(T1, T2, T).
inference([equivalence(M, P), T1], [equivalence(S, M), T2], [equivalence(S, P), T]) :-
	S \= P, nal_f_res(T1, T2, T).

%% implication-based composition

inference([implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]) :-
	S \= P, nal_reduce(disjunction([P, S]), N), nal_f_int(T1, T2, T).
inference([implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]) :-
	S \= P, nal_reduce(conjunction([P, S]), N), nal_f_uni(T1, T2, T).
inference([implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]) :-
	S \= P, nal_reduce(conjunction([P, S]), N), nal_f_int(T1, T2, T).
inference([implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]) :-
	S \= P, nal_reduce(disjunction([P, S]), N), nal_f_uni(T1, T2, T).

%% implication-based decomposition

inference([implication(S, M), T1], [implication(disjunction(L), M), T2], [implication(P, M), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(disjunction(N), P), nal_f_pnn(T1, T2, T).
inference([implication(S, M), T1], [implication(conjunction(L), M), T2], [implication(P, M), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(conjunction(N), P), nal_f_npp(T1, T2, T).
inference([implication(M, S), T1], [implication(M, conjunction(L)), T2], [implication(M, P), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(conjunction(N), P), nal_f_pnn(T1, T2, T).
inference([implication(M, S), T1], [implication(M, disjunction(L)), T2], [implication(M, P), T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(disjunction(N), P), nal_f_npp(T1, T2, T).

%% conditional syllogism

inference([implication(M, P), T1], [M, T2], [P, T]) :-
	ground(P), nal_f_ded(T1, T2, T).
inference([implication(P, M), T1], [M, T2], [P, T]) :-
	ground(P), nal_f_abd(T1, T2, T).
inference([M, T1], [equivalence(S, M), T2], [S, T]) :-
	ground(S), nal_f_ana(T1, T2, T).

%% conditional composition

inference([P, T1], [S, T2], [C, T]) :-
	C == implication(S, P), nal_f_ind(T1, T2, T).
inference([P, T1], [S, T2], [C, T]) :-
	C == equivalence(S, P), nal_f_com(T1, T2, T).
inference([P, T1], [S, T2], [C, T]) :-
	nal_reduce(conjunction([P, S]), N), N == C, nal_f_int(T1, T2, T).
inference([P, T1], [S, T2], [C, T]) :-
	nal_reduce(disjunction([P, S]), N), N == C, nal_f_uni(T1, T2, T).

%% propositional decomposition

inference([S, T1], [conjunction(L), T2], [P, T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(conjunction(N), P), nal_f_pnn(T1, T2, T).
inference([S, T1], [disjunction(L), T2], [P, T]) :-
	ground(S), ground(L), member(S, L), delete(L, S, N), nal_reduce(disjunction(N), P), nal_f_npp(T1, T2, T).

%% multi-conditional syllogism

inference([implication(conjunction(L), C), T1], [M, T2], [implication(P, C), T]) :-
	nonvar(L), member(M, L), subtract(L, [M], A), A \= [], nal_reduce(conjunction(A), P), nal_f_ded(T1, T2, T).
inference([implication(conjunction(L), C), T1], [implication(P, C), T2], [M, T]) :-
	ground(L), member(M, L), subtract(L, [M], A), A \= [], nal_reduce(conjunction(A), P), nal_f_abd(T1, T2, T).
inference([implication(conjunction(L), C), T1], [M, T2], [S, T]) :-
	S == implication(conjunction([M|L]), C), nal_f_ind(T1, T2, T).

inference([implication(conjunction(Lm), C), T1], [implication(A, M), T2], [implication(P, C), T]) :-
	nonvar(Lm), nal_replace(Lm, M, La, A), nal_reduce(conjunction(La), P), nal_f_ded(T1, T2, T).
inference([implication(conjunction(Lm), C), T1], [implication(conjunction(La), C), T2], [implication(A, M), T]) :-
	nonvar(Lm), nal_replace(Lm, M, La, A), nal_f_abd(T1, T2, T).
inference([implication(conjunction(La), C), T1], [implication(A, M), T2], [implication(P, C), T]) :-
	nonvar(La), nal_replace(Lm, M, La, A), nal_reduce(conjunction(Lm), P), nal_f_ind(T1, T2, T).

%% variable introduction

inference([inheritance(M, P), T1], [inheritance(M, S), T2], [implication(inheritance(X, S), inheritance(X, P)), T]) :-
	S \= P, nal_f_ind(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [implication(inheritance(P, X), inheritance(S, X)), T]) :-
	S \= P, nal_f_abd(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [equivalence(inheritance(X, S), inheritance(X, P)), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [equivalence(inheritance(P, X), inheritance(S, X)), T]) :-
	S \= P, nal_f_com(T1, T2, T).
inference([inheritance(M, P), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)]), T]) :-
	S \= P, nal_f_int(T1, T2, T).
inference([inheritance(P, M), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))]), T]) :-
	S \= P, nal_f_int(T1, T2, T).

%% 2nd variable introduction

inference([implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [implication(conjunction([A, inheritance(X, S)]), inheritance(X, P)), T]) :-
	S \= P, M1 == M2, A \= inheritance(M2, S), nal_f_ind(T1, T2, T).
inference([implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [conjunction([implication(A, inheritance(var(Y, []), P)), inheritance(var(Y, []), S)]), T]) :-
	S \= P, M1 == M2, A \= inheritance(M2, S), nal_f_int(T1, T2, T).
inference([conjunction(L1), T1], [inheritance(M, S), T2], [implication(inheritance(Y, S), conjunction([inheritance(Y, P2)|L3])), T]) :-
	subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, dependent(P, Y, P2), dependent(L2, Y, L3), nal_f_ind(T1, T2, T).
inference([conjunction(L1), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)|L2]), T]) :-
	subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, nal_f_int(T1, T2, T).

inference([implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [implication(conjunction([A, inheritance(P, X)]), inheritance(S, X)), T]) :-
	S \= P, M1 == M2, A \= inheritance(S, M2), nal_f_abd(T1, T2, T).
inference([implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [conjunction([implication(A, inheritance(P, var(Y, []))), inheritance(S, var(Y, []))]), T]) :-
	S \= P, M1 == M2, A \= inheritance(S, M2), nal_f_int(T1, T2, T).
inference([conjunction(L1), T1], [inheritance(S, M), T2], [implication(inheritance(S, Y), conjunction([inheritance(P2, Y)|L3])), T]) :-
	subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, dependent(P, Y, P2), dependent(L2, Y, L3), nal_f_abd(T1, T2, T).
inference([conjunction(L1), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))|L2]), T]) :-
	subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, nal_f_int(T1, T2, T).

%% dependent variable elimination

inference([conjunction(L1), T1], [inheritance(M, S), T2], [C, T]) :-
	subtract(L1, [inheritance(var(N, D), S)], L2), L1 \= L2,
	replace_var(L2, var(N, D), L3, M), nal_reduce(conjunction(L3), C), nal_f_cnv(T2, T0), nal_f_ana(T1, T0, T).
inference([conjunction(L1), T1], [inheritance(S, M), T2], [C, T]) :-
	subtract(L1, [inheritance(S, var(N, D))], L2), L1 \= L2,
	replace_var(L2, var(N, D), L3, M), nal_reduce(conjunction(L3), C), nal_f_cnv(T2, T0), nal_f_ana(T1, T0, T).

replace_var([], _, [], _).
replace_var([inheritance(S1, P)|T1], S1, [inheritance(S2, P)|T2], S2) :-
	replace_var(T1, S1, T2, S2).
replace_var([inheritance(S, P1)|T1], P1, [inheritance(S, P2)|T2], P2) :-
	replace_var(T1, P1, T2, P2).
replace_all([H|T1], H1, [H|T2], H2) :-
	replace_var(T1, H1, T2, H2).



%%% Theorems in IL:

% inheritance

inheritance(ext_intersection(Ls), P) :-
	nal_include([P], Ls).
inheritance(S, int_intersection(Lp)) :-
	nal_include([S], Lp).
inheritance(ext_intersection(S), ext_intersection(P)) :-
	nal_include(P, S), P \= [_].
inheritance(int_intersection(S), int_intersection(P)) :-
	nal_include(S, P), S \= [_].
inheritance(ext_set(S), ext_set(P)) :-
	nal_include(S, P).
inheritance(int_set(S), int_set(P)) :-
	nal_include(P, S).

inheritance(ext_difference(S, P), S) :-
	ground(S), ground(P).
inheritance(S, int_difference(S, P)) :-
	ground(S), ground(P).

inheritance(product(L1), R) :-
	ground(L1), member(ext_image(R, L2), L1), nal_replace(L1, ext_image(R, L2), L2).
inheritance(R, product(L1)) :-
	ground(L1), member(int_image(R, L2), L1), nal_replace(L1, int_image(R, L2), L2).

% similarity

similarity(X, Y) :-
	ground(X), nal_reduce(X, Y), X \== Y, !.

similarity(ext_intersection(L1), ext_intersection(L2)) :-
	same_set(L1, L2).
similarity(int_intersection(L1), int_intersection(L2)) :-
	same_set(L1, L2).
similarity(ext_set(L1), ext_set(L2)) :-
	same_set(L1, L2).
similarity(int_set(L1), int_set(L2)) :-
	same_set(L1, L2).

% implication

implication(similarity(S, P), inheritance(S, P)).
implication(equivalence(S, P), implication(S, P)).

implication(conjunction(L), M) :-
	ground(L), member(M, L).
implication(M, disjunction(L)) :-
	ground(L), member(M, L).

implication(conjunction(L1), conjunction(L2)) :-
	ground(L1), ground(L2), subset(L2, L1).
implication(disjunction(L1), disjunction(L2)) :-
	ground(L1), ground(L2), subset(L1, L2).

implication(inheritance(S, P), inheritance(ext_intersection(Ls), ext_intersection(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(inheritance(S, P), inheritance(int_intersection(Ls), int_intersection(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(similarity(S, P), similarity(ext_intersection(Ls), ext_intersection(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(similarity(S, P), similarity(int_intersection(Ls), int_intersection(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).

implication(inheritance(S, P), inheritance(ext_difference(S, M), ext_difference(P, M))):-
	ground(M).
implication(inheritance(S, P), inheritance(int_difference(S, M), int_difference(P, M))):-
	ground(M).
implication(similarity(S, P), similarity(ext_difference(S, M), ext_difference(P, M))):-
	ground(M).
implication(similarity(S, P), similarity(int_difference(S, M), int_difference(P, M))):-
	ground(M).
implication(inheritance(S, P), inheritance(ext_difference(M, P), ext_difference(M, S))):-
	ground(M).
implication(inheritance(S, P), inheritance(int_difference(M, P), int_difference(M, S))):-
	ground(M).
implication(similarity(S, P), similarity(ext_difference(M, P), ext_difference(M, S))):-
	ground(M).
implication(similarity(S, P), similarity(int_difference(M, P), int_difference(M, S))):-
	ground(M).

implication(inheritance(S, P), negation(inheritance(S, ext_difference(M, P)))) :-
	ground(M).
implication(inheritance(S, ext_difference(M, P)), negation(inheritance(S, P))) :-
	ground(M).
implication(inheritance(S, P), negation(inheritance(int_difference(M, S), P))) :-
	ground(M).
implication(inheritance(int_difference(M, S), P), negation(inheritance(S, P))) :-
	ground(M).

implication(inheritance(S, P), inheritance(ext_image(S, M), ext_image(P, M))) :-
	ground(M).
implication(inheritance(S, P), inheritance(int_image(S, M), int_image(P, M))) :-
	ground(M).
implication(inheritance(S, P), inheritance(ext_image(M, Lp), ext_image(M, Ls))) :-
	ground(Ls), ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).
implication(inheritance(S, P), inheritance(int_image(M, Lp), int_image(M, Ls))) :-
	ground(Ls), ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).

implication(negation(M), negation(conjunction(L))) :-
	nal_include([M], L).
implication(negation(disjunction(L)), negation(M)) :-
	nal_include([M], L).

implication(implication(S, P), implication(conjunction(Ls), conjunction(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(implication(S, P), implication(disjunction(Ls), disjunction(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(equivalence(S, P), equivalence(conjunction(Ls), conjunction(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).
implication(equivalence(S, P), equivalence(disjunction(Ls), disjunction(Lp))):-
	ground(Ls), ground(Lp), nal_replace(Ls, S, L, P), nal_same(L, Lp).


% equivalence

equivalence(X, Y) :-
	ground(X), nal_reduce(X, Y), X \== Y, !.

equivalence(similarity(S, P), similarity(P, S)).

equivalence(inheritance(S, ext_set([P])), similarity(S, ext_set([P]))).
equivalence(inheritance(int_set([S]), P), similarity(int_set([S]), P)).

equivalence(inheritance(S, ext_intersection(Lp)), conjunction(L)) :-
	findall(inheritance(S, P), member(P, Lp), L).
equivalence(inheritance(int_intersection(Ls), P), conjunction(L)) :-
	findall(inheritance(S, P), member(S, Ls), L).

equivalence(inheritance(S, ext_difference(P1, P2)),
	    conjunction([inheritance(S, P1), negation(inheritance(S, P2))])).
equivalence(inheritance(int_difference(S1, S2), P),
	    conjunction([inheritance(S1, P), negation(inheritance(S2, P))])).

equivalence(inheritance(product(Ls), product(Lp)), conjunction(L)) :-
	equ_product(Ls, Lp, L).

equivalence(inheritance(product([S|L]), product([P|L])), inheritance(S, P)) :-
	ground(L).
equivalence(inheritance(S, P), inheritance(product([H|Ls]), product([H|Lp]))) :-
	ground(H), equivalence(inheritance(product(Ls), product(Lp)), inheritance(S, P)).

equivalence(inheritance(product(L), R), inheritance(T, ext_image(R, L1))) :-
	nal_replace(L, T, L1).
equivalence(inheritance(R, product(L)), inheritance(int_image(R, L1), T)) :-
	nal_replace(L, T, L1).

equivalence(equivalence(S, P), equivalence(P, S)).

equivalence(equivalence(negation(S), P), equivalence(negation(P), S)).

equivalence(conjunction(L1), conjunction(L2)) :-
	same_set(L1, L2).
equivalence(disjunction(L1), disjunction(L2)) :-
	same_set(L1, L2).

equivalence(implication(S, conjunction(Lp)), conjunction(L)) :-
	findall(implication(S, P), member(P, Lp), L).
equivalence(implication(disjunction(Ls), P), conjunction(L)) :-
	findall(implication(S, P), member(S, Ls), L).

equivalence(T1, T2) :-
	not(atom(T1)), not(atom(T2)), ground(T1), ground(T2),
	T1 =.. L1, T2 =.. L2, equivalence_list(L1, L2).

equivalence_list(L, L).
equivalence_list([H|L1], [H|L2]) :-
	equivalence_list(L1, L2).
equivalence_list([H1|L1], [H2|L2]) :-
	similarity(H1, H2), equivalence_list(L1, L2).
equivalence_list([H1|L1], [H2|L2]) :-
	equivalence(H1, H2), equivalence_list(L1, L2).

% compound term structure reduction

nal_reduce(similarity(ext_set([S]), ext_set([P])), similarity(S, P)) :-
	!.
nal_reduce(similarity(int_set([S]), int_set([P])), similarity(S, P)) :-
	!.

nal_reduce(instance(S, P), inheritance(ext_set([S]), P)) :-
	!.
nal_reduce(property(S, P), inheritance(S, int_set([P]))) :-
	!.
nal_reduce(inst_prop(S, P), inheritance(ext_set([S]), int_set([P]))) :-
	!.

nal_reduce(ext_intersection([T]), T) :-
	!.
nal_reduce(int_intersection([T]), T) :-
	!.

nal_reduce(ext_intersection([ext_intersection(L1), ext_intersection(L2)]), ext_intersection(L)) :-
	union(L1, L2, L), !.
nal_reduce(ext_intersection([ext_intersection(L1), L2]), ext_intersection(L)) :-
	union(L1, [L2], L), !.
nal_reduce(ext_intersection([L1, ext_intersection(L2)]), ext_intersection(L)) :-
	union([L1], L2, L), !.
nal_reduce(ext_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)) :-
	intersection(L1, L2, L), !.
nal_reduce(ext_intersection([int_set(L1), int_set(L2)]), int_set(L)) :-
	union(L1, L2, L), !.

nal_reduce(int_intersection([int_intersection(L1), int_intersection(L2)]), int_intersection(L)) :-
	union(L1, L2, L), !.
nal_reduce(int_intersection([int_intersection(L1), L2]), int_intersection(L)) :-
	union(L1, [L2], L), !.
nal_reduce(int_intersection([L1, int_intersection(L2)]), int_intersection(L)) :-
	union([L1], L2, L), !.
nal_reduce(int_intersection([int_set(L1), int_set(L2)]), int_set(L)) :-
	intersection(L1, L2, L), !.
nal_reduce(int_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)) :-
	union(L1, L2, L), !.

nal_reduce(ext_difference(ext_set(L1), ext_set(L2)), ext_set(L)) :-
	subtract(L1, L2, L), !.
nal_reduce(int_difference(int_set(L1), int_set(L2)), int_set(L)) :-
	subtract(L1, L2, L), !.

nal_reduce(product(product(L), T), product(L1)) :-
	append(L, [T], L1), !.

nal_reduce(ext_image(product(L1), L2), T1) :-
	member(T1, L1), nal_replace(L1, T1, L2), !.
nal_reduce(int_image(product(L1), L2), T1) :-
	member(T1, L1), nal_replace(L1, T1, L2), !.

nal_reduce(negation(negation(S)), S) :-
	!.

nal_reduce(conjunction([T]), T) :-
	!.
nal_reduce(disjunction([T]), T) :-
	!.

nal_reduce(conjunction([conjunction(L1), conjunction(L2)]), conjunction(L)) :-
	union(L1, L2, L), !.
nal_reduce(conjunction([conjunction(L1), L2]), conjunction(L)) :-
	union(L1, [L2], L), !.
nal_reduce(conjunction([L1, conjunction(L2)]), conjunction(L)) :-
	union([L1], L2, L), !.

nal_reduce(disjunction(disjunction(L1), disjunction(L2)), disjunction(L)) :-
	union(L1, L2, L), !.
nal_reduce(disjunction(disjunction(L1), L2), disjunction(L)) :-
	union(L1, [L2], L), !.
nal_reduce(disjunction(L1, disjunction(L2)), disjunction(L)) :-
	union([L1], L2, L), !.

nal_reduce(X, X).


%%% Argument processing

equ_product([], [], []).
equ_product([T|Ls], [T|Lp], L) :-
	equ_product(Ls, Lp, L), !.
equ_product([S|Ls], [P|Lp], [inheritance(S, P)|L]) :-
	equ_product(Ls, Lp, L).

same_set(L1, L2) :-
	L1 \== [], L1 \== [_], nal_same(L1, L2), L1 \== L2.

nal_same([], []).
nal_same(L, [H|T]) :-
	member(H, L), subtract(L, [H], L1), nal_same(L1, T).

nal_include(L1, L2) :-
	ground(L2), include1(L1, L2), L1 \== [], L1 \== L2.

include1([], _).
include1([H|T1], [H|T2]) :-
	include1(T1, T2).
include1([H1|T1], [H2|T2]) :-
	H2 \== H1, include1([H1|T1], T2).

nal_not_member(_, []).
nal_not_member(C, [C|_]) :- !, fail.
nal_not_member([S, T], [[S1, T]|_]) :- equivalence(S, S1), !, fail.
nal_not_member(C, [_|L]) :- nal_not_member(C, L).

nal_replace([T|L], T, [nil|L]).
nal_replace([H|L], T, [H|L1]) :-
	nal_replace(L, T, L1).

nal_replace([H1|T], H1, [H2|T], H2).
nal_replace([H|T1], H1, [H|T2], H2) :-
	nal_replace(T1, H1, T2, H2).

dependent(var(V, L), Y, var(V, [Y|L])) :-
	!.
dependent([H|T], Y, [H1|T1]) :-
	dependent(H, Y, H1), dependent(T, Y, T1), !.
dependent(inheritance(S, P), Y, inheritance(S1, P1)) :-
	dependent(S, Y, S1), dependent(P, Y, P1), !.
dependent(ext_image(R, A), Y, ext_image(R, A1)) :-
	dependent(A, Y, A1), !.
dependent(int_image(R, A), Y, int_image(R, A1)) :-
	dependent(A, Y, A1), !.
dependent(X, _, X).


%%% Truth-value functions

nal_f_rev([F1, C1], [F2, C2], [F, C]) :-
     C1 < 1,
     C2 < 1,
	M1 is C1 / (1 - C1),
	M2 is C2 / (1 - C2),
	F is (M1 * F1 + M2 * F2) / (M1 + M2),
	C is (M1 + M2) / (M1 + M2 + 1).

nal_f_exp([F, C], E) :-
	E is C * (F - 0.5) + 0.5.

nal_f_neg([F1, C1], [F, C1]) :-
	u_not(F1, F).

nal_f_cnv([F1, C1], [1, C]) :-
     u_and([F1, C1], W),
	u_w2c(W, C).

nal_f_cnt([F1, C1], [0, C]) :-
	u_not(F1, F0),
     u_and([F0, C1], W),
	u_w2c(W, C).

nal_f_ded([F1, C1], [F2, C2], [F, C]) :-
	u_and([F1, F2], F),
	u_and([C1, C2, F], C).

nal_f_ana([F1, C1], [F2, C2], [F, C]) :-
	u_and([F1, F2], F),
	u_and([C1, C2, F2], C).

nal_f_res([F1, C1], [F2, C2], [F, C]) :-
	u_and([F1, F2], F),
	u_or([F1, F2], F0),
	u_and([C1, C2, F0], C).

nal_f_abd([F1, C1], [F2, C2], [F2, C]) :-
	u_and([F1, C1, C2], W),
	u_w2c(W, C).

nal_f_ind(T1, T2, T) :-
	nal_f_abd(T2, T1, T).

nal_f_exe([F1, C1], [F2, C2], [1, C]) :-
	u_and([F1, C1, F2, C2], W),
	u_w2c(W, C).

nal_f_com([0, _C1], [0, _C2], [0, 0]).
nal_f_com([F1, C1], [F2, C2], [F, C]) :-
	u_or([F1, F2], F0),
	F0 > 0,
	F is F1 * F2 / F0,
	u_and([F0, C1, C2], W),
	u_w2c(W, C).

nal_f_int([F1, C1], [F2, C2], [F, C]) :-
	u_and([F1, F2], F),
	u_and([C1, C2], C).

nal_f_uni([F1, C1], [F2, C2], [F, C]) :-
	u_or([F1, F2], F),
	u_and([C1, C2], C).

nal_f_dif([F1, C1], [F2, C2], [F, C]) :-
	u_not(F2, F0),
	u_and([F1, F0], F),
	u_and([C1, C2], C).

nal_f_pnn([F1, C1], [F2, C2], [F, C]) :-
	u_not(F2, F2n),
	u_and([F1, F2n], Fn),
	u_not(Fn, F),
	u_and([Fn, C1, C2], C).

nal_f_npp([F1, C1], [F2, C2], [F, C]) :-
	u_not(F1, F1n),
	u_and([F1n, F2], F),
	u_and([F, C1, C2], C).

nal_f_pnp([F1, C1], [F2, C2], [F, C]) :-
	u_not(F2, F2n),
	u_and([F1, F2n], F),
	u_and([F, C1, C2], C).

nal_f_nnn([F1, C1], [F2, C2], [F, C]) :-
	u_not(F1, F1n),
	u_not(F2, F2n),
	u_and([F1n, F2n], Fn),
	u_not(Fn, F),
	u_and([Fn, C1, C2], C).

% Utility functions

u_not(N0, N) :-
	N is (1 - N0), !.

u_and([N], N).
u_and([N0 | Nt], N) :-
	u_and(Nt, N1), N is N0 * N1, !.

u_or([N], N).
u_or([N0 | Nt], N) :-
	u_or(Nt, N1), N is (N0 + N1 - N0 * N1), !.

u_w2c(W, C) :-
	K = 1, C is (W / (W + K)), !.

:- fixup_exports.


/*
User's Guide of NAL
Input/Output Language
In this demonstration, NAL implements the following formal language, Narsese. The input and output of the system are Narsese judgments.
           <judgment> ::= [<statement> [frequency-value, confidence-value]]
          <statement> ::= <relation>(<term>, <term>)
                        | <compound-statement>
                        | <term>
               <term> ::= <word>
                        | <variable>
                        | <compound-term>
                        | <statement>
           <variable> ::= <independent-variable>
                        | <dependent-variable>(<independent-variable>*)
           <relation> ::= inheritance
                        | similarity
                        | implication
                        | equivalence
                        | instance
                        | property
                        | inst_prop                                // instance-property
 <compound-statement> ::= negation(<statement>)
                        | conjunction([<statement>, <statement>+])
                        | disjunction([<statement>, <statement>+])
      <compound-term> ::= ext_set([<term>+])                       // extensional set
                        | int_set([<term>+])                       // intensional set
                        | ext_intersection([<term>, <term>+])      // extensional intersection
                        | int_intersection([<term>, <term>+])      // intensional intersection
                        | ext_difference(<term>, <term>)           // extensional difference
                        | int_difference(<term>, <term>)           // intensional difference
                        | product([<term>, <term>+])
                        | ext_image(<term>, <term>)                // extensional image
                        | int_image(<term>, <term>)                // intensional image
The frequency-value is a real number in [0, 1]; the confidence-value a real number in (0, 1).
User Interface
The program can be invoked in the following ways:
revision(J1, J2, J).
Judgment J is the result of a revision between judgments J1 and J2. The three judgments all have the nal_same statement in them.
choice(J1, J2, J).
Judgment J is the result of a choice between judgments J1 and J2.
inference(J1, J2, J).
Judgment J is the conclusion derived from judgments J1 and J2 as premises.
inference(J1, J).
Judgment J is the conclusion derived from judgment J1 as single premise.

*/

