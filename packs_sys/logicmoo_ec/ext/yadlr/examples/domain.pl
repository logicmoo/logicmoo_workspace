%% Usage examples usage of multi-valued DL reasoner
%% This file contains the kb of the domain.
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 6-2-2007
%% This file is in the public domain.


:- assert( use_inference_engine(resolution) ).
:- assert( use_algebra(alg_lukasiewicz) ).
:- use_module('../pl/yadlr').


/*
CURRENTLY SUPPORTED SYNTAX:
F :== dlnot(F)
F :== dland(F, F)
F :== dlor(F, F)
F :== dlimplies(F, F)
F :== dlequiv(F, F)
F :== C(I)
C :== PredicateName  
I :== InstanceName(s)

TODO:
F :== all(X, F)
F :== exists(X, F)
F :==  atmost(X, N, F)
F :== atleast(X, N, F)
F :== box F
F :== dia F
F :== cir F
F :== until(F, F)

X :== VariableName
N :== Integer
*/

symbol_declarations :-
	yadlr_concept( kb, team ),
	yadlr_concept( kb, sports_club ),
	yadlr_concept( kb, national_team ),
	yadlr_concept( kb, competition ),
	yadlr_concept( kb, world_cup_final ),
	yadlr_concept( kb, champions_league ),
	yadlr_concept( kb, uefa_cup ),
	yadlr_concept( kb, serieA ),
	yadlr_concept( kb, superleague ),
	yadlr_concept( kb, good ),
	yadlr_concept( kb, good_team ),
	yadlr_concept( kb, very_good_team ),
	yadlr_concept( kb, good_competition ),
	yadlr_instance( kb, panathinaikos ),
	yadlr_instance( kb, napoli ),
	yadlr_instance( kb, argentina ),
	yadlr_instance( kb, ec88 ),
	yadlr_instance( kb, ec92 ),
	yadlr_instance( kb, uefa89 ),
	yadlr_instance( kb, serieA87 ),
	yadlr_instance( kb, serieA88 ),
	yadlr_instance( kb, serieA89 ),
	yadlr_instance( kb, serieA90 ),
	yadlr_instance( kb, serieA91 ),
	yadlr_instance( kb, superleague91 ).


crisp_domain_axioms :-
	yadlr_assert( kb, all(X, dlimplies(sports_club(X), team(X)) ), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(national_team(X), team(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(world_cup_final(X), competition(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(champions_league(X), competition(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(uefa_cup(X), competition(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(serieA(X), competition(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(superleague(X), competition(X))), 1.0 ),
	yadlr_assert( kb, all(X, dlimplies(dland(good(X),competition(X)), good_competition(X))), 1.0 ).

domain_facts :-
	yadlr_assert( kb, ( national_team(argentina) ), 1.0 ),
	yadlr_assert( kb, ( sports_club(panathinaikos) ), 1.0 ),
	yadlr_assert( kb, ( sports_club(napoli) ), 1.0 ),
	yadlr_assert( kb, ( champions_league(ec88) ), 1.0 ),
	yadlr_assert( kb, ( champions_league(ec92) ), 1.0 ),
	yadlr_assert( kb, ( uefa_cup(uefa89) ), 1.0 ),
	yadlr_assert( kb, ( serieA(serieA87) ), 1.0 ),
	yadlr_assert( kb, ( serieA(serieA88) ), 1.0 ),
	yadlr_assert( kb, ( superleague(superleague91) ), 1.0 ),
	yadlr_assert( kb, ( good(serieA87) ), 0.8 ),
	yadlr_assert( kb, ( good(serieA88) ), 0.8 ),
	yadlr_assert( kb, ( good(serieA89) ), 0.8 ),
	yadlr_assert( kb, ( good(serieA90) ), 0.75 ),
	yadlr_assert( kb, ( good(uefa89) ), 0.9 ),
	yadlr_assert( kb, ( good(ec88) ), 0.95 ),
	yadlr_assert( kb, ( good(ec92) ), 0.95 ).

relation_declarations :-
	yadlr_relation( kb, has_won ),
	yadlr_relation( kb, has_participated ).
	
relational_domain_axioms :-
	% good teams have won at least one competition
	yadlr_assert( kb, all(X, dlimplies(exists(Y,dland(good_competition(Y),has_won(X,Y))),
					   good_team(X)) ), 1.0 ).

relational_domain_facts :-
	yadlr_assert( kb, ( has_participated(panathinaikos,superleague91) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(panathinaikos,ec92) ), 1.0 ),
	yadlr_assert( kb, ( has_won(panathinaikos,superleague91) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,serieA87) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,serieA88) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,serieA89) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,serieA90) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,ec88) ), 1.0 ),
	yadlr_assert( kb, ( has_participated(napoli,uefa89) ), 1.0 ),
	yadlr_assert( kb, ( has_won(napoli,serieA87) ), 1.0 ),
	yadlr_assert( kb, ( has_won(napoli,serieA90) ), 1.0 ),
	yadlr_assert( kb, ( has_won(napoli,uefa89) ), 1.0 ).

extended_quantification_axioms :-
	% very good teams have won at least two competitions
	yadlr_assert( kb,
		      all(X,
			  dlimplies(exists(Y1,
					   exists(Y2,
						  dland(dland(dland(has_won(X,Y1),
								    good_competition(Y1)),
							      dland(has_won(X,Y2),
								    good_competition(Y2))),
							dlalldifferent([Y1,Y2]) ))),
				    very_good_team(X)) ), 1.0 ).

