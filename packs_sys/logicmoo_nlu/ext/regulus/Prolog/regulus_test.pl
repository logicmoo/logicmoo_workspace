% regulus_test.pl

% Test predicates for Regulus

test0 :-
	regulus2nuance(
	toy_regulus_grammars(toy0),
	nuance_grammars(eng_recogniser)
    ).

test_eurospeech0 :-
	regulus2nuance(
	toy_regulus_grammars(toy_for_eurospeech),
	nuance_grammars(eng_recogniser)
    ).

test1 :-
	regulus2nuance(
	toy_regulus_grammars(toy1),
	nuance_grammars(eng_recogniser)
    ).

test2 :-
	regulus2nuance(
	toy_regulus_grammars(toy2),
	nuance_grammars(eng_recogniser)
    ).

test3 :-
	regulus2nuance(
	toy_regulus_grammars(toy3),
	nuance_grammars(eng_recogniser)
    ).

test_cat_val :-
	regulus2nuance(
	toy_regulus_grammars(toy_cat_val),
	nuance_grammars(eng_recogniser)
    ).

test_include :-
	regulus2nuance(
	toy_regulus_grammars(toy_include),
	nuance_grammars(eng_recogniser)
    ).

test_include1 :-
	regulus2nuance(
	toy_regulus_grammars(toy_include1),
	nuance_grammars(eng_recogniser)
    ).

test_external :-
	regulus2nuance(
	toy_regulus_grammars(toy_external_grammar),
	nuance_grammars(eng_recogniser)
    ).

test_arith :-
	regulus2nuance(
	toy_regulus_grammars(toy_arith),
	nuance_grammars(eng_recogniser)
    ).

test_ling :-
	regulus2nuance(
	toy_regulus_grammars(toy_ling),
	nuance_grammars(eng_recogniser)
    ).

test_swe_ling :-
	regulus2nuance(
	toy_regulus_grammars(swe_ling),
	nuance_grammars(swe_recogniser)
    ).

test_swe_ling1 :-
	regulus2nuance(
	toy_regulus_grammars(swe_ling1),
	nuance_grammars(swe_recogniser)
    ).

test_ah0_swe_coarse :-
	regulus2nuance(
	ah0_swe_regulus_grammars(ah0_componentised_coarse_grained),
	nuance_grammars(swe_recogniser)
    ).

test_ah1_swe_coarse :-
	regulus2nuance(
	ah1_swe_regulus_grammars(ah0_componentised_coarse_grained),
	nuance_grammars(swe_recogniser)
    ).

test_ah0_swe_fine :-
	regulus2nuance(
	ah0_swe_regulus_grammars(ah0_componentised_fine_grained),
	nuance_grammars(swe_recogniser)
    ).

test_ah1_swe_fine :-
	regulus2nuance(
	ah1_swe_regulus_grammars(ah0_componentised_fine_grained),
	nuance_grammars(swe_recogniser)
    ).

test_ah0_swe_no_sorts :-
	regulus2nuance(
	ah0_swe_regulus_grammars(ah0_componentised_no_sorts),
	nuance_grammars(swe_recogniser)
    ).

test_ah0_eng_coarse :-
	regulus2nuance(
	ah0_eng_regulus_grammars(ah0_componentised_coarse_grained),
	nuance_grammars(eng_recogniser)
    ).

test_ah0_eng_fine :-
	regulus2nuance(
	ah0_eng_regulus_grammars(ah0_componentised_fine_grained),
	nuance_grammars(eng_recogniser)
    ).

test_ah0_eng_no_sorts :-
	regulus2nuance(
	ah0_eng_regulus_grammars(ah0_componentised_no_sorts),
	nuance_grammars(eng_recogniser)
    ).

test_ah0_eng_coarse_no_agr :-
	regulus2nuance(
	ah0_eng_regulus_grammars(ah0_componentised_coarse_grained_no_agr),
	nuance_grammars(eng_recogniser)
    ).


test_psa :-
	regulus2nuance(
	psa_grammars(psa),
	nuance_grammars(eng_recogniser)
    ).

test_psa_full :-
	regulus2nuance(
	psa_grammars(psa_full),
	nuance_grammars(eng_recogniser)
    ).

test_psa_general :-
	regulus2nuance(
	general_grammars(psa),
	nuance_grammars(eng_recogniser)
    ).
