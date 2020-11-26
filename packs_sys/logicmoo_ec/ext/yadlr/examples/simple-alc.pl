%% Example usage of fuzzy ALC reasoner
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 6-2-2007
%% This file is in the public domain.

:- assert( use_inference_engine(resolution) ).
:- assert( use_algebra(alg_lukasiewicz) ).
:- use_module('../pl/yadlr').

:- consult(domain).

:- set_proof_tree_log( yes ).


preparation:-
	symbol_declarations,
	crisp_domain_axioms,
	domain_facts.

/* query the degree of an instance's belonging to a concept,
   or check that a given degree is plausible */

test_check_membership( D1, R1, D2, R2 ) :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	check_membership( kb, superleague91, competition, D1, R1 ),
	check_membership( kb, superleague91, good_competition, D2, R2 ).

/* query the types (concepts) that an instance belongs to */

test_check_types( C, D, R ) :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	check_types( kb, superleague91, D, C, R ).

test_check_members :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	load_tbox1,
	check_members( kb, good_competition, [0.3,0.3], [superleague91,serieA91], R1 ),
	check_members( kb, good_competition, [0.3],     [superleague91], R2 ).
