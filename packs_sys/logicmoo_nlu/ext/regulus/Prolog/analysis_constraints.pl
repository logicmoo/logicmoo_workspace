
% analysis_constraints.pl

%---------------------------------------------------------------

:- module(analysis_constraints,
	  [check_analysis_constraints/2,
	   preference_score_for_lf_and_tree/3
          ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%---------------------------------------------------------------

preference_score_for_lf_and_tree(LFAndTree, Score, Trace) :-
	findall([SubScore, TraceElt],
		individual_preference_score(LFAndTree, SubScore, TraceElt),
		Pairs),
	extract_sub_scores_and_trace_elts(Pairs, SubScores, Trace),
	safe_sum_list(SubScores, Score).

extract_sub_scores_and_trace_elts([], [], []).
extract_sub_scores_and_trace_elts([[SubScore, TraceElt] | R], [SubScore | R1], [TraceElt | R2]) :-
	extract_sub_scores_and_trace_elts(R, R1, R2).

individual_preference_score(LFAndTree, SubScore, TraceElt) :-
	member(full_tree=Tree, LFAndTree),
	frequency_preference_information_available_in_term(Tree),
	rule_frequency_preference_score(Tree, Score),
	SubScore is 0.01 * Score,
	TraceElt = rule_frequency-SubScore.
individual_preference_score(LFAndTree, SubScore, TraceElt) :-
	current_predicate(user:parse_preference_score/2),
	user:parse_preference_score(Pattern, SubScore),
	check_analysis_constraint(Pattern, LFAndTree),
	TraceElt = Pattern-SubScore.

%---------------------------------------------------------------

check_analysis_constraints([], _LFAndTree) :-
	!.
check_analysis_constraints([F | R], LFAndTree) :-
	check_analysis_constraint(F, LFAndTree),
	!,
	check_analysis_constraints(R, LFAndTree).

check_analysis_constraint((P, Q), LFAndTree) :-
	!,
	check_analysis_constraint(P, LFAndTree),
	check_analysis_constraint(Q, LFAndTree).
check_analysis_constraint((P ; Q), LFAndTree) :-
	!,
	(   check_analysis_constraint(P, LFAndTree) ;
	    check_analysis_constraint(Q, LFAndTree)
	).
check_analysis_constraint((\+ (P)), LFAndTree) :-
	!,
	\+ check_analysis_constraint(P, LFAndTree).
check_analysis_constraint(lf_includes_structure=Term, LFAndTree) :-
	!,
	member(lf=LF, LFAndTree),
	term_contains_subterm(LF, Term).
check_analysis_constraint(lf_doesnt_include_structure=Term, LFAndTree) :-
	!,
	member(lf=LF, LFAndTree),
	\+ term_contains_subterm(LF, Term).
check_analysis_constraint(tree_includes_structure=Term, LFAndTree) :-
	!,
	member(tree=Tree, LFAndTree),
	term_contains_subterm(Tree, Term).
check_analysis_constraint(tree_doesnt_include_structure=Term, LFAndTree) :-
	!,
	member(tree=Tree, LFAndTree),
	\+ term_contains_subterm(Tree, Term).
check_analysis_constraint(Constraint, _LFAndTree) :-
	format('~N*** Warning: unknown constraint: ~w~n', [Constraint]),
	fail.

