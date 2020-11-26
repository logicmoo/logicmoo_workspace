%% Example usage of fuzzy SHOIQ reasoner
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 15-6-2007
%% This file is in the public domain.


:- assert_if_new( use_inference_engine(resolution) ).
:- assert_if_new( use_algebra(alg_lukasiewicz) ).
%:- use_module('pl/yadlr').


:- consult(domain).

preparation:-
	yadlr_init( kb ),
	symbol_declarations,
	crisp_domain_axioms,
	domain_facts,
	relation_declarations,
	relational_domain_axioms,
	relational_domain_facts.

test1a :-
	set_proof_tree_log( yes ),
	preparation,
	yadlr_assert( kb, ( good(superleague91) ), 0.6 ),
	check_membership( kb, superleague91, good_competition, _Degree, Restr ),
	fmt( 'Restrictions: = ~q~n', [Restr] ).

test1b :-
	set_proof_tree_log( yes ),
	preparation,
	yadlr_assert( kb, ( good(superleague91) ), _X ),
	check_membership( kb, superleague91, good_competition, _Degree, Restr ),
	fmt( 'Restrictions: = ~q~n', [Restr] ).

% find the relation between x and d
test2 :-
	set_proof_tree_log( 'test2.log' ),
	preparation,
	yadlr_assert( kb, ( good(superleague91) ), _X ),
	check_membership( kb, panathinaikos, good_team, Degree, Restr1 ),
	fmt( 'Restrictions: = ~q~n', [Restr1] ),
	check_membership( kb, napoli, good_team, Degree, Restr2 ),
	fmt( 'Restrictions: = ~q~n', [Restr2] ),
	unset_proof_tree_log.

% find out x required in order to achieve a certain d
% NOTE how check_membership( kb, napoli, good_team, 0.85, Restr1 )
% finds a different solution that above
test3 :-
	set_proof_tree_log( 'test3.log' ),
	preparation,
	check_membership( kb, napoli, good_team, 0.85, Restr1 ),
	fmt( 'Restrictions: = ~q~n', [Restr1] ),
	yadlr_assert( kb, ( good(superleague91) ), _X ),
	check_membership( kb, panathinaikos, good_team, 0.85, Restr2 ),
	fmt( 'Restrictions: = ~q~n', [Restr2] ),
	unset_proof_tree_log.

% try -N constructs (at-least, at-most)
test4 :-
	set_proof_tree_log( yes ),
	%set_proof_tree_log( 'test4.log' ),
	preparation,
	extended_quantification_axioms,
	check_membership( kb, napoli, very_good_team, _Deg, Restr1 ),
	fmt( 'Restrictions: = ~q~n', [Restr1] ),
	check_membership( kb, panathinaikos, very_good_team, _D, Restr2 ),
	fmt( 'Restrictions: = ~q~n', [Restr2] ),
	unset_proof_tree_log.

