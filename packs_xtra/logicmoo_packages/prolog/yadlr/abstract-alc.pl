%% Example usage of fuzzy ALC reasoner
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 6-2-2007
%% This file is in the public domain.


:- assert_if_new( use_inference_engine(resolution) ).
:- assert_if_new( use_algebra(alg_lukasiewicz) ).
%:- use_module('pl/yadlr').

:- consult(domain).

:- set_proof_tree_log( yes ).

preparation:-
	symbol_declarations,
	crisp_domain_axioms,
	domain_facts.

/* query the degree of an instance's belonging to a concept */

test1a :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	yadlr_assert( kb, ( good(superleague91) ), _X ),
	check_membership( kb, superleague91, good_competition, _Degree, Restr ),
	fmt( 'Restrictions: = ~q~n', [Restr] ).


test1b :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	yadlr_concept( kb, bad ),
	yadlr_assert( kb, all(X, dlequiv(dlnot(bad(X)), good(X)) ), 1.0 ),
	yadlr_assert( kb, ( bad(superleague91) ), _X ),
	check_membership( kb, superleague91, good_competition, _Degree, Restr ),
	fmt( 'Restrictions: = ~q~n', [Restr] ).

/* check that a given degree is plausible */

test2 :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	check_membership( kb, superleague91, good_competition, 0.7, Restr ),
	fmt( 'Restrictions: = ~q~n', [Restr] ).

/* query the types (concepts) that an instance belongs to */

test3 :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	check_types( kb, superleague91, D, C, R ),
	fmt( 'Types: = ~q~n', [C] ),
	fmt( 'Degrees: = ~q~n', [D] ),
	fmt( 'Restrictions: = ~q~n', [R] ).

/* retrieve the members of a concept */

test4 :-
	set_proof_tree_log( yes ),
	yadlr_retract( kb ),
	preparation,
	check_members( kb, good_competition, [0.3,0.3], [superleague91,serieA87], _R1 ),
	check_members( kb, good_competition, _D,         _I,                        _R2 ).
