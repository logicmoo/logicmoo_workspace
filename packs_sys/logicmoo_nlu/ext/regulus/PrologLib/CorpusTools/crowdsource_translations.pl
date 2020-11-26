
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- module(crowdsource_translations,
	  [four_column_csv_to_amt_input_from_command_line/1,
	   four_column_csv_to_amt_input/1,
	   four_column_csv_to_amt_input/3,

	   four_column_csv_and_amt_output_to_results_from_command_line/1,
	   four_column_csv_and_amt_output_to_results/3,

	   set_majority_judgement_type/1,
	   source_tr1_tr2_to_csv/4,
	   amt_translation_judgements_file_to_prolog/3,
	   amt_translation_judgements_file_to_prolog/4,
	   amt_translation_judgements_file_to_prolog/7,

	   add_sentence_bleu_reference_and_preedited_to_amt_results_csv/8,
	  
	   test_four_column_csv_to_amt_input/1,
	   test_source_tr1_tr2_to_csv/1,
	   test_process_amt/1]
    ).

:- use_module('$REGULUS/PrologLib/CorpusTools/manipulate_ngram_files').

:- use_module('$REGULUS/PrologLib/amt_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(random)).

%======================================================================

test_four_column_csv_to_amt_input(combined) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Evaluations/combined_RSource_RTrans_PrEdTrans.csv', 0'\t, 50).

test_four_column_csv_to_amt_input(a_vs_a) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Evaluations/a_vs_a_RSource_RTrans_PrEdTrans.csv', 0'\t, 20).

test_four_column_csv_to_amt_input(a_vs_a_50) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Evaluations/a_vs_a_RSource_RTrans_PrEdTrans.csv', 0'\t, 50).

test_four_column_csv_to_amt_input(accord_phrase_nominale) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Evaluations/accord_phrase_nominale_RSource_RTrans_PrEdTrans.csv', 0'\t, 20).

test_four_column_csv_to_amt_input(forme_verbale_incorrecte) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Evaluations/forme_verbale_incorrecte_RSource_RTrans_PrEdTrans.csv', 0'\t, 20).


%======================================================================

test_four_column_csv_to_amt_input(homophones_restart) :-
	four_column_csv_to_amt_input('$ACCEPT/MT/Homophones/Data/Restart/bu-1.csv', 0'\t, 20),
	four_column_csv_to_amt_input('$ACCEPT/MT/Homophones/Data/Restart/bu-2.csv', 0'\t, 20),
	four_column_csv_to_amt_input('$ACCEPT/MT/Homophones/Data/Restart/bb-1.csv', 0'\t, 20),
	four_column_csv_to_amt_input('$ACCEPT/MT/Homophones/Data/Restart/bb-2.csv', 0'\t, 20).

%======================================================================

test_source_tr1_tr2_to_csv(tu) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_baseline.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_ez.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(tu_v2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_v2_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(devtest_b_baseline_pre_edited) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/devtest_b.fr',
			      '$ACCEPT/MT/Europarl/TestResults/devtest_b_baseline.en',
			      '$ACCEPT/MT/Europarl/TestResults/devtest_b_pre_edited.en',
			      '$ACCEPT/MT/Europarl/TestResults/devtest_b_baseline_pre_edited_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/devtest_b_baseline_pre_edited_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/devtest_b_baseline_pre_edited.csv').

test_source_tr1_tr2_to_csv(homophones_baseline_uniq) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
			      '$ACCEPT/MT/Homophones/Data/baseline_final.en',
			      '$ACCEPT/MT/Homophones/Data/uniq_final.en',
			      '$ACCEPT/MT/Homophones/Data/homophones_baseline_uniq_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Homophones/Data/homophones_baseline_uniq_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Homophones/Data/homophones_baseline_uniq.csv').
			    
test_source_tr1_tr2_to_csv(homophones_baseline_bigram2_filtered) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
			      '$ACCEPT/MT/Homophones/Data/baseline_final.en',
			      '$ACCEPT/MT/Homophones/Data/bigram2_filtered_final.en',
			      '$ACCEPT/MT/Homophones/Data/homophones_baseline_bigram2_filtered_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Homophones/Data/homophones_baseline_bigram2_filtered_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Homophones/Data/homophones_baseline_bigram2_filtered.csv').
			    
test_source_tr1_tr2_to_csv(homophones_uniq_bigram2_filtered) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
			      '$ACCEPT/MT/Homophones/Data/uniq_final.en',
			      '$ACCEPT/MT/Homophones/Data/bigram2_filtered_final.en',
			      '$ACCEPT/MT/Homophones/Data/homophones_uniq_bigram2_filtered_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Homophones/Data/homophones_uniq_bigram2_filtered_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Homophones/Data/homophones_uniq_bigram2_filtered.csv').
			    
test_source_tr1_tr2_to_csv(tu_ez_preedited) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_ez.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_preedited.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_ez_preedited_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_ez_preedited_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_ez_preedited_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(tu_baseline_preedited_v2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline_preedited_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline_preedited_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline_preedited.csv').

test_source_tr1_tr2_to_csv(tu_preedited_minimal_preedited_v2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_minimal_preedited.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited_minimal_preedited_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited_minimal_preedited_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited_minimal_preedited.csv').


test_source_tr1_tr2_to_csv(tu_ez_preedited_v2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited.en',
			      '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez_preedited_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez_preedited_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez_preedited_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(tu_ez_plus_preedited_preedited_v2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/tu_200_v2.txt',
			      '$ACCEPT/MT/Europarl/TestResults/Nov2012Data/out.train_base_test_vous.txt',
			      '$ACCEPT/MT/Europarl/TestResults/Nov2012Data/out.train_tu_test_vous.txt',
			      '$ACCEPT/MT/Europarl/TestResults/Nov2012Data/tu_200_v2_ez_plus_preedited_preedited_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/Nov2012Data/tu_200_v2_ez_plus_preedited_preedited_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/Nov2012Data/tu_200_v2_ez_plus_preedited_preedited.csv').


test_source_tr1_tr2_to_csv(est_ce_que) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/est_ce_que_200.txt',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_baseline.en',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_est_ce_que.en',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(est_ce_que_cleaned) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/est_ce_que_200_cleaned.txt',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_baseline_cleaned.en',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_est_ce_que_cleaned.en',
			      '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_cleaned_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_cleaned_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_cleaned_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(plus_to_pas) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_source.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_variant_target.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(plus_to_pas2) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_source2.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target2.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_variant_target2.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet_raw2.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet_raw2.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/plus_to_pas_randomised_spreadsheet2.csv').

test_source_tr1_tr2_to_csv(bigram_clitics) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_source.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_original_target.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_variant_target.txt',
			      '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_randomised_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_randomised_spreadsheet_raw.csv',
			    'UTF-8',
			    20,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_randomised_spreadsheet.csv').

test_source_tr1_tr2_to_csv(tu_factored) :-
	source_tr1_tr2_to_csv('$ACCEPT/MT/Evaluations/Factored/tu.fr',
			      '$ACCEPT/MT/Evaluations/Factored/tu.baseline.en',
			      '$ACCEPT/MT/Evaluations/Factored/tu.backoff.en',
			      '$ACCEPT/MT/Evaluations/Factored/tu_spreadsheet_raw.csv'),

	header_line(Header),

	reorganise_cvs_file('$ACCEPT/MT/Evaluations/Factored/tu_spreadsheet_raw.csv',
			    'UTF-8',
			    50,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    '$ACCEPT/MT/Evaluations/Factored/tu_spreadsheet.csv').

%======================================================================
			    
test_process_amt(tu) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_summary.txt').

test_process_amt(tu_canadian) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_canadian.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_canadian_summary.txt').

test_process_amt(tu_v2_baseline_ez) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_baseline_ez.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_baseline_ez_summary.txt').

test_process_amt(tu_v2_ez_pre_edited) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_ez_pre_edited.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_ez.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_ez_pre_edited_summary.txt').

test_process_amt(tu_v2_ez_plus_preedited_preedited) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_ez_plus_pre_edited_pre_edited.csv',
						  '$ACCEPT/MT/Europarl/TestResults/Nov2012Data/out.train_base_test_vous.txt',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_ez_plus_pre_edited_pre_edited_summary.txt').

test_process_amt(tu_v2_baseline_pre_edited) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_baseline_pre_edited.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_baseline_pre_edited_summary.txt').
	
test_process_amt(tu_v2_pre_edited_minimal_pre_edited) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_pre_edited_minimal_pre_edited.csv',
						  '$ACCEPT/MT/Europarl/TestResults/tu_200_v2_preedited.en',
						  '$ACCEPT/MT/Europarl/AMTResults/tu_200_v2_pre_edited_minimal_pre_edited_summary.txt').

test_process_amt(est_ce_que) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/est_ce_que_200.csv',
						  '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/est_ce_que_200_summary.txt').
	
test_process_amt(est_ce_que_canadian) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/est_ce_que_200_canadian.csv',
						  '$ACCEPT/MT/Europarl/TestResults/est_ce_que_200_baseline_cleaned.en',
						  '$ACCEPT/MT/Europarl/AMTResults/est_ce_que_200_canadian_summary.txt').

test_process_amt(devtest_b_baseline_pre_edited) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Europarl/AMTResults/devtest_b_baseline_pre_edited.csv',
						  '$ACCEPT/MT/Europarl/TestResults/devtest_b_baseline.en',
						  '$ACCEPT/MT/Europarl/AMTResults/devtest_b_baseline_pre_edited_summary.txt').

test_process_amt(tu_factored) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Evaluations/Factored/AMTResults.csv',
						  '$ACCEPT/MT/Evaluations/Factored/tu.baseline.en',
						  '$ACCEPT/MT/Evaluations/Factored/results_summary.txt').
	
test_process_amt(homophones_baseline_uniq) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_uniq.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_summary.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_results.csv').

test_process_amt(homophones_baseline_uniq_all) :-
	test_process_amt(homophones_baseline_uniq_all_relaxed),
	test_process_amt(homophones_baseline_uniq_all_strict),
	test_process_amt(homophones_baseline_uniq_all_unanimous).
	
test_process_amt(homophones_baseline_uniq_all_relaxed) :-
	set_majority_judgement_type(relaxed),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_uniq_all.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_summary_relaxed.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_results.csv').
	
test_process_amt(homophones_baseline_uniq_all_strict) :-
	set_majority_judgement_type(strict),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_uniq_all.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_summary_strict.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_results.csv').
	
test_process_amt(homophones_baseline_uniq_all_unanimous) :-
	set_majority_judgement_type(unanimous),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_uniq_all.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_summary_unanimous.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_uniq_all_results.csv').

test_process_amt(homophones_baseline_bigrams2) :-
	test_process_amt(homophones_baseline_bigrams2_relaxed),
	test_process_amt(homophones_baseline_bigrams2_strict),
	test_process_amt(homophones_baseline_bigrams2_unanimous).
	
test_process_amt(homophones_baseline_bigrams2_relaxed) :-
	set_majority_judgement_type(relaxed),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_summary_relaxed.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_results.csv').
	
test_process_amt(homophones_baseline_bigrams2_strict) :-
	set_majority_judgement_type(strict),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_summary_strict.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_results.csv').
	
test_process_amt(homophones_baseline_bigrams2_unanimous) :-
	set_majority_judgement_type(unanimous),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_summary_unanimous.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_results.csv').

test_process_amt(homophones_uniq_bigrams2) :-
	test_process_amt(homophones_uniq_bigrams2_relaxed),
	test_process_amt(homophones_uniq_bigrams2_strict),
	test_process_amt(homophones_uniq_bigrams2_unanimous).
	
test_process_amt(homophones_uniq_bigrams2_relaxed) :-
	set_majority_judgement_type(relaxed),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/uniq_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/uniq_final.en',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_summary_relaxed.txt',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv').

test_process_amt(homophones_uniq_bigrams2_strict) :-
	set_majority_judgement_type(strict),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/uniq_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/uniq_final.en',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_summary_strict.txt',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv').

test_process_amt(homophones_uniq_bigrams2_unanimous) :-
	set_majority_judgement_type(unanimous),
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/uniq_bigram2.csv',
						  '$ACCEPT/MT/Homophones/Data/uniq_final.en',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_summary_unanimous.txt',
						  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv').

test_process_amt(homophones_uniq_bigrams2_bleu) :-
	test_process_amt(homophones_uniq_bigrams2_bleu_relaxed),
	test_process_amt(homophones_uniq_bigrams2_bleu_strict),
	test_process_amt(homophones_uniq_bigrams2_bleu_unanimous).
	
test_process_amt(homophones_uniq_bigrams2_bleu_relaxed) :-
	set_majority_judgement_type(relaxed),
	add_sentence_bleu_reference_and_preedited_to_full_judgements_file('$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_unique.txt',
									  '$ACCEPT/MT/Homophones/Results/ref.tok',
									  '$ACCEPT/MT/Homophones/Results/uniq.bleu',
									  '$ACCEPT/MT/Homophones/Results/bigram2.bleu',
									  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results_with_bleu_relaxed.html',
									  'UTF-8').	

test_process_amt(homophones_uniq_bigrams2_bleu_strict) :-
	set_majority_judgement_type(strict),
	add_sentence_bleu_reference_and_preedited_to_full_judgements_file('$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_unique.txt',
									  '$ACCEPT/MT/Homophones/Results/ref.tok',
									  '$ACCEPT/MT/Homophones/Results/uniq.bleu',
									  '$ACCEPT/MT/Homophones/Results/bigram2.bleu',
									  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results_with_bleu_strict.html',
									  'UTF-8').	
	
test_process_amt(homophones_uniq_bigrams2_bleu_unanimous) :-
	set_majority_judgement_type(unanimous),
	add_sentence_bleu_reference_and_preedited_to_full_judgements_file('$ACCEPT/MT/Homophones/Results/uniq_bigram2_results.csv',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
									  '$ACCEPT/MT/Homophones/Data/devtest_b_unique.txt',
									  '$ACCEPT/MT/Homophones/Results/ref.tok',
									  '$ACCEPT/MT/Homophones/Results/uniq.bleu',
									  '$ACCEPT/MT/Homophones/Results/bigram2.bleu',
									  '$ACCEPT/MT/Homophones/Results/uniq_bigram2_results_with_bleu_unanimous.html',
									  'UTF-8').	
	
test_process_amt(homophones_baseline_bigrams2_all) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/Homophones/Results/baseline_bigram2_all.csv',
						  '$ACCEPT/MT/Homophones/Data/baseline_final.en',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_all_summary.txt',
						  '$ACCEPT/MT/Homophones/Results/baseline_bigram2_all_results.csv').
	
test_process_amt(plus_to_pas_v1) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v1.csv',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target.txt',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison.pl',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v1_summary.txt',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v1_summary.pl').

test_process_amt(plus_to_pas_v2) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v2.csv',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_original_target2.txt',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev2.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_plus_or_quoi_dev_comparison2.pl',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v2_summary.txt',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/plus_to_pas_v2_summary.pl').

test_process_amt(bigram_clitics_v1) :-
	amt_translation_judgements_file_to_prolog('$ACCEPT/MT/GTFeb2012/AMTResults/bigram_clitics_v1.csv',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_original_target.txt',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/forum_ngrams_versus_tm_plus_europarl.pl',
						  '$ACCEPT/MT/GTFeb2012/CleanFrenchVersions/bigrams_clitics_dev_comparison.pl',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/bigram_clitics_v1_summary.txt',
						  '$ACCEPT/MT/GTFeb2012/AMTResults/bigram_clitics_v1_summary.pl').

test_process_amt(a_vs_a) :-
	four_column_csv_and_amt_output_to_results('$ACCEPT/MT/Evaluations/a_vs_a_RSource_RTrans_PrEdTrans.csv', 0'\t).
		
test_process_amt(combined) :-
	four_column_csv_and_amt_output_to_results('$ACCEPT/MT/Evaluations/combined_RSource_RTrans_PrEdTrans.csv', 0'\t).

test_process_amt(a_vs_a_to_ce_vs_ce) :-
	four_column_csv_and_amt_output_to_results('$ACCEPT/MT/Evaluations/a_vs_a_to_ce_vs_ce.csv', 0'\t).

test_process_amt(u_group2) :-
	four_column_csv_and_amt_output_to_results('$ACCEPT/MT/Evaluations/u_group2.csv', 0'\t).

%======================================================================

header_line(['ORIGINAL', 'FIRST-TRANSLATION', 'SECOND-TRANSLATION']).	    

%======================================================================

four_column_csv_to_amt_input_from_command_line([FourColumnCSV]) :-
	!,
	four_column_csv_to_amt_input(FourColumnCSV).
four_column_csv_to_amt_input_from_command_line([FourColumnCSV, NItemsAtom]) :-
	coerce_atom_to_int(NItemsAtom, NItems),
	!,
	four_column_csv_to_amt_input(FourColumnCSV, 0'\t, NItems).
four_column_csv_to_amt_input_from_command_line(_Args) :-
	format('~N~nUsage: sicstus -l four_column_csv_to_amt_input.pl -a <CSVFile> ?<NItemsInHIT>~2n', []).
	
%======================================================================

four_column_csv_to_amt_input(FourColumnCSV) :-
	four_column_csv_to_amt_input(FourColumnCSV, 0'\t, 20).

four_column_csv_to_amt_input(FourColumnCSV, Separator, NItemsInHIT) :-
	derived_file_names(FourColumnCSV, '',
			   [source, baseline, variant, randomised_spreadsheet_raw, randomised_spreadsheet],
			   [txt, txt, txt, csv, csv],
			   [Source, Baseline, Variant, RawSpreadsheet, RandomisedSpreadsheet]),

	%csv_file_columns_to_files(FourColumnCSV, Separator, [1-Source, 2-Baseline, 3-Variant]),
	csv_file_columns_to_files_randomising_order(FourColumnCSV, Separator, [1-Source, 2-Baseline, 3-Variant]),

	source_tr1_tr2_to_csv(Source, Baseline, Variant, RawSpreadsheet),

	header_line(Header),

	reorganise_cvs_file(RawSpreadsheet,
			    'UTF-8',
			    NItemsInHIT,
			    Header,
			    ['NO ITEM', 'NO ITEM', 'NO ITEM'],
			    RandomisedSpreadsheet),

	format('~N--- AMT input spreadsheet written to ~w~n', [RandomisedSpreadsheet]).

%======================================================================

four_column_csv_and_amt_output_to_results_from_command_line([FourColumnCSV]) :-
	!,
	four_column_csv_and_amt_output_to_results(FourColumnCSV, 0'\t).
four_column_csv_and_amt_output_to_results_from_command_line(_Args) :-
	format('~N~nUsage: sicstus -l four_column_csv_and_amt_output_to_results.pl -a <CSVFile>~2n', []).
	
%======================================================================

four_column_csv_and_amt_output_to_results(FourColumnCSV, Separator) :-
	derived_file_names(FourColumnCSV, '',
			   [source, baseline, variant, labels, amt, results],
			   [txt, txt, txt, txt, csv, txt],
			   [SourceFile, BaselineFile, VariantFile, LabelsFile, AMTFile, BaseResultsFile]),

	csv_file_columns_to_files(FourColumnCSV, Separator, [1-SourceFile, 2-BaselineFile, 3-VariantFile, 4-LabelsFile]),

	get_all_labels_from_file(LabelsFile, Labels),
	
	amt_translation_judgements_file_to_prolog_for_labels(Labels, AMTFile, SourceFile, BaselineFile, VariantFile, LabelsFile, BaseResultsFile).

amt_translation_judgements_file_to_prolog_for_labels([], _AMTFile, _SourceFile, _BaselineFile, _VariantFile, _LabelsFile, _BaseResultsFile).
amt_translation_judgements_file_to_prolog_for_labels([Label | Labels], AMTFile, SourceFile, BaselineFile, VariantFile, LabelsFile, BaseResultsFile) :-
	amt_translation_judgements_file_to_prolog_for_label(Label, AMTFile, SourceFile, BaselineFile, VariantFile, LabelsFile, BaseResultsFile),
	!,
	amt_translation_judgements_file_to_prolog_for_labels(Labels, AMTFile, SourceFile, BaselineFile, VariantFile, LabelsFile, BaseResultsFile).

amt_translation_judgements_file_to_prolog_for_label(Label, AMTFile, SourceFile, BaselineFile, VariantFile, LabelsFile, BaseResultsFile) :-
	format('~N--- Processing AMT results for "~w"~n~n', [Label]),
	derived_file_names(BaseResultsFile, '',
			   [Label],
			   [txt],
			   [ResultsFile]),

	amt_translation_judgements_file_to_prolog(AMTFile, [SourceFile, BaselineFile, VariantFile, LabelsFile, Label], ResultsFile),

	format('~N--- AMT results for "~w" written to ~w~n~n~n', [Label, ResultsFile]).

get_all_labels_from_file(File, Labels) :-
	safe_absolute_file_name(File, AbsFile),
	read_unicode_file_to_atom_list(AbsFile, List),
	sort(List, Labels),
	!.
		
%======================================================================

/*

Input: SourceFile, Target1File, Target2File are UTF-8 encoded files with the same number of records, one sentence per line.
Target1File and Target2File offer different translations of SourceFile.

Output: CVSFile is a CSV file with the same number of records, where each line is of the form

<Source, Translation, OtherTranslation>

The translations are in random order.

*/

source_tr1_tr2_to_csv(SourceFile, Target1File, Target2File, CVSFile) :-
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(Target1File, AbsTarget1File),
	safe_absolute_file_name(Target2File, AbsTarget2File),
	safe_absolute_file_name(CVSFile, AbsCVSFile),

	read_translation_file(AbsSourceFile, SourceList, NSource),
	read_translation_file(AbsTarget1File, Target1List, NTarget1),
	read_translation_file(AbsTarget2File, Target2List, NTarget2),

	(   all_numbers_are_same([NSource, NTarget1, NTarget2]) ->
	    true
	;
	    format('~N*** Error: files do not contain the same number of records~n', []),
	    fail
	),

	source_and_targets_to_csv_records(SourceList, Target1List, Target2List, CSVList, 0-NonTrivialComparisons, 0-NDiffs),
	length(CSVList, NCSVList),
	(   NonTrivialComparisons > 0 ->
	    AvDiffs is NDiffs / NonTrivialComparisons
	;
	    AvDiffs is 0.0
	),

	list_of_lists_to_csv_file(CSVList, AbsCVSFile, 'UTF-8'),
	format('~N--- Written CSV file (~d records) ~w~n', [NCSVList, AbsCVSFile]),
	format('~N--- Average number of differences per non-trivial comparison: ~2f~n', [AvDiffs]),
	!.

read_translation_file(File, List, N) :-
	read_unicode_file_to_atom_list(File, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, File]),
	!.

source_and_targets_to_csv_records([], [], [], [], C-C, N-N).
source_and_targets_to_csv_records([_F | R], [F1 | R1], [F2 | R2], R3, CIn-COut, NIn-NOut) :-
	identical_translations(F1, F2),
	!,
	source_and_targets_to_csv_records(R, R1, R2, R3, CIn-COut, NIn-NOut).
source_and_targets_to_csv_records([F | R], [F1 | R1], [F2 | R2], [F3 | R3], CIn-COut, NIn-NOut) :-
	source_and_targets_to_csv_record(F, F1, F2, F3, NDiffs),
	(   NDiffs > 0 ->
	    CNext is CIn + 1
	;
	    otherwise ->
	    CNext is CIn
	),
	NNext is NIn + NDiffs,
	!,
	source_and_targets_to_csv_records(R, R1, R2, R3, CNext-COut, NNext-NOut).

source_and_targets_to_csv_record(Source, Target1, Target2, CSVRecord, NDiffs) :-
	mark_diffs(Target1, Target2, Target1Marked, Target2Marked, NDiffs),
	random_permutation([Target1Marked, Target2Marked], RandomisedTargets),
	CSVRecord = [Source | RandomisedTargets].

mark_diffs(Target1, Target2, Target1Marked, Target2Marked, NDiffs) :-
	split_atom_into_words(Target1, Target1Words),
	split_atom_into_words(Target2, Target2Words),
	mark_diff_items(Target1Words, Target2Words, AnnotatedTarget1Words, AnnotatedTarget2Words, NDiffs),
	join_with_spaces(AnnotatedTarget1Words, Target1Marked),
	join_with_spaces(AnnotatedTarget2Words, Target2Marked),
	!.

mark_diff_items(Target1Words, Target2Words, AnnotatedTarget1Words, AnnotatedTarget2Words, NDiffs) :-
	insertions_deletions_substitutions_and_matches(Target1Words, Target2Words, NDiffs, _I, _D, _S, Matches),
	mark_diff_items1(Matches, AnnotatedTarget1Words, AnnotatedTarget2Words),
	!.
mark_diff_items(Target1Words, Target2Words, AnnotatedTarget1Words, AnnotatedTarget2Words, NDiffs) :-
	format('~N*** Error: bad call: ~w~n', [mark_diff_items(Target1Words, Target2Words, AnnotatedTarget1Words, AnnotatedTarget2Words, NDiffs)]),
	fail.

mark_diff_items1([], [], []).
mark_diff_items1([same(X) | R1], [X | R2], [X | R3]) :-
	!,
	mark_diff_items1(R1, R2, R3).
mark_diff_items1([sub(X, Y) | R1], [MarkedX | R2], [MarkedY | R3]) :-
	!,
	mark_atom(X, MarkedX),
	mark_atom(Y, MarkedY),
	mark_diff_items1(R1, R2, R3).
mark_diff_items1([del(X) | R1], R2, [MarkedX | R3]) :-
	!,
	mark_atom(X, MarkedX),
	mark_diff_items1(R1, R2, R3).
mark_diff_items1([ins(X) | R1], [MarkedX | R2], R3) :-
	!,
	mark_atom(X, MarkedX),
	mark_diff_items1(R1, R2, R3).

mark_atom(X, MarkedX) :-
	%format_to_atom('<b>~w</b>', [X], MarkedX). % bold
	format_to_atom('<font color="#ff0033">~w</font>', [X], MarkedX).  %red

%======================================================================

amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo, SummaryFile) :-
	amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo,
						  '*no_file*', '*no_file*', '*no_file*', 
						  SummaryFile, '*no_file*', '*no_file*').

amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo, SummaryFile, FullJudgementsFile) :-
	amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo,
						  '*no_file*', '*no_file*', '*no_file*', 
						  SummaryFile, FullJudgementsFile, '*no_file*').

amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo,
					  NGramExamplesFile, NGramFrequenciesFile, ComparisonFile,
					  SummaryFile, PrologSummaryFile) :-
	amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo,
					  NGramExamplesFile, NGramFrequenciesFile, ComparisonFile,
					  SummaryFile, '*no_file*', PrologSummaryFile).

amt_translation_judgements_file_to_prolog(AMTFile, Version1FileInfo,
					  NGramExamplesFile, NGramFrequenciesFile, ComparisonFile,
					  SummaryFile, FullJudgementsFile, PrologSummaryFile) :-
	safe_absolute_file_name(AMTFile, AbsAMTFile),
	safe_absolute_file_name(SummaryFile, AbsSummaryFile),
	(   PrologSummaryFile = '*no_file*' ->
	    AbsPrologSummaryFile = '*no_file*'
	;
	    otherwise ->
	    safe_absolute_file_name(PrologSummaryFile, AbsPrologSummaryFile)
	),

	maybe_load_ngram_examples_file_and_ngram_frequencies_file(NGramExamplesFile, NGramFrequenciesFile, ComparisonFile),

	load_version1_file_info(Version1FileInfo),

	amt_csv_file_to_list_of_alists(AbsAMTFile, 0'", 0',, 'UTF-8', ParsedAMTList), %' "
	process_amt_list(ParsedAMTList, ProcessedAMTData),

	maybe_write_full_judgements_file(ProcessedAMTData, FullJudgementsFile),
	
	%list_to_prolog_file_with_encoding(ProcessedAMTData, AbsSummaryFile, 'UTF-8').
	print_processed_amt_judgements(ProcessedAMTData, AbsSummaryFile, PrologSummaryFile).

%======================================================================

maybe_load_ngram_examples_file_and_ngram_frequencies_file(NGramExamplesFile, NGramFrequenciesFile, ComparisonFile) :-
	member('*no_file*', [NGramExamplesFile, NGramFrequenciesFile, ComparisonFile]),
	!.
maybe_load_ngram_examples_file_and_ngram_frequencies_file(NGramExamplesFile, NGramFrequenciesFile, ComparisonFile) :-
	load_ngram_examples_file(NGramExamplesFile),
	load_combined_ngrams(NGramFrequenciesFile),
	load_comparison_file(ComparisonFile),
	!.

:- dynamic ngram_example/2.
:- dynamic variant_translation_with_details/6.

load_comparison_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	retractall(variant_translation_with_details(_,_,_,_,_,_)),
	safe_prolog_file_to_list_printing_statistics(AbsFile, List, 'UTF-8'),
	load_comparison_file1(List),
	!.
load_comparison_file(File) :-
	format('~N*** Error: bad call: ~w~n', [load_comparison_file(File)]),
	fail,
	!.

load_comparison_file1([]).
load_comparison_file1([F | R]) :-
	load_comparison_file_example(F),
	!,
	load_comparison_file1(R).

load_comparison_file_example(variant_translation_with_details(Source, OriginalTarget, VariantSource, VariantTarget, Ngram, Tag)) :-
	assertz(variant_translation_with_details(Source, OriginalTarget, VariantSource, VariantTarget, Ngram, Tag)),
	!.
load_comparison_file_example(Other) :-
	format('~N*** Error: bad call: ~w~n', [load_comparison_file_example(Other)]),
	fail.

ngram_examples_loaded :-
	ngram_example(_, _),
	!.

load_ngram_examples_file(File) :-
	retractall(ngram_example(_, _)),
	safe_absolute_file_name(File, AbsFile),
	safe_prolog_file_to_list_printing_statistics(AbsFile, List, 'UTF-8'),
	load_ngram_examples_file1(List),
	!.
load_ngram_examples_file(File) :-
	format('~N*** Error: bad call: ~w~n', [load_ngram_examples_file(File)]),
	fail.

load_ngram_examples_file1([]).
load_ngram_examples_file1([F | R]) :-
	load_ngram_example(F),
	!,
	load_ngram_examples_file1(R).

% ngram_example([a,plus],1,'Non, il n\'y a plus aucun Norton installé.').

load_ngram_example(ngram_example(NGram, _Multiplicity, Example)) :-
	assertz(ngram_example(Example, NGram)),
	!.
load_ngram_example(F) :-
	format('~N*** Error: bad call: ~w~n', [load_ngram_example(F)]),
	fail.

ngram_and_frequencies_for_example(_Example, _Ngram, _Freq1, _Freq2) :-
	\+ ngram_examples_loaded,
	!,
	fail.
ngram_and_frequencies_for_example(Example, NGram, Freq1, Freq2) :-
	ngram_example(Example, NGram),
	normalised_frequencies_for_ngram(NGram, Freq1, Freq2),
	!.
ngram_and_frequencies_for_example(Example, Ngram, Freq1, Freq2) :-
	format('~N*** Warning: bad call: ~w~n', [ngram_and_frequencies_for_example(Example, Ngram, Freq1, Freq2)]),
	fail.

variant_translation_info_for_example(Source, OriginalTarget, VariantTarget, VariantSource, Tag) :-
	variant_translation_with_details(Source, OriginalTarget, VariantSource, VariantTarget, _Ngram, Tag),
	!.
variant_translation_info_for_example(Source, OriginalTarget, VariantTarget, VariantSource, Tag) :-
	format('~N*** Warning: bad call: ~w~n', [variant_translation_info_for_example(Source, OriginalTarget, VariantTarget, VariantSource, Tag)]),
	fail.

%======================================================================

:- dynamic version1_translation/2.
:- dynamic source_version1_version2/4.

load_version1_file_info([SourceFile, Version1File, Version2File, LabelFile, Label]) :-
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(Version1File, AbsVersion1File),
	safe_absolute_file_name(Version2File, AbsVersion2File),
	safe_absolute_file_name(LabelFile, AbsLabelFile),
	retractall(version1_translation(_, _)),
	retractall(source_version1_version2(_, _, _, _)),
	read_translation_file(AbsSourceFile, SourceList, NSource),
	read_translation_file(AbsVersion1File, Version1List, NVersion1),
	read_translation_file(AbsVersion2File, Version2List, NVersion2),
	read_translation_file(AbsLabelFile, LabelList, NLabels),
	sort([NSource, NVersion1, NVersion2, NLabels], FileLengths),
	length(FileLengths, NLengths),
	(   NLengths = 1 ->
	    load_source_version1_version2_and_label_lists(SourceList, Version1List, Version2List, LabelList, Label),
	    format('~N--- Stored source, version 1 and version 2 translations and labels (~d items)~n', [NSource])
	;
	    format('~N*** Error: source, version1, version2 and label files have different numbers of entries~n', []),
	    fail
	),
	print_number_of_source_version1_version2_used(Label),
	!.
load_version1_file_info(Version1File) :-
	safe_absolute_file_name(Version1File, AbsVersion1File),
	retractall(version1_translation(_, _)),
	read_translation_file(AbsVersion1File, List, N),
	load_version1_list(List),
	format('~N--- Stored version 1 translations (~d items)~n', [N]),
	!.
load_version1_file_info(File) :-
	format('~N*** Error: bad call: ~w~n', [load_version1_file_info(File)]),
	fail.

load_source_version1_version2_and_label_lists([], [], [], [], _Label).
load_source_version1_version2_and_label_lists([F | R], [F1 | R1], [F2 | R2], [F3 | R3], Label) :-
	load_source_version1_version2_and_label_line(F, F1, F2, F3, Label),
	!,
	load_source_version1_version2_and_label_lists(R, R1, R2, R3, Label).

load_source_version1_version2_and_label_line(Source, T1, T2, Label, LabelToUse) :-
	normalise_translation(Source, NormalisedSource),
	normalise_translation(T1, NormalisedT1),
	normalise_translation(T2, NormalisedT2),	
	(   Label = LabelToUse ->
	    Use = use
	;
	    Use = dont_use
	),
	assertz(source_version1_version2(NormalisedSource, NormalisedT1, NormalisedT2, Use)),
	!.
load_source_version1_version2_and_label_line(Source, T1, T2, Label, LabelToUse) :-
	format('~N*** Error: bad call: ~w~n', [load_source_version1_version2_and_label_line(Source, T1, T2, Label, LabelToUse)]),
	fail.

load_version1_list([]).
load_version1_list([F | R]) :-
	load_version1_item(F),
	!,
	load_version1_list(R).

load_version1_item(Item) :-
	normalise_translation(Item, Item1),
	assertz(version1_translation(Item1, use)),
	!.
load_version1_item(Item) :-
	format('~N*** Error: bad call: ~w~n', [load_version1_item(Item)]),
	fail.

normalise_translation(T0, T1) :-
	remove_bad_chars_from_atom(T0, T),
	split_atom_into_words(T, TWords),
	join_with_spaces(TWords, T1).

remove_bad_chars_from_atom(Atom, Atom1) :-
	atom_codes(Atom, Str),
	remove_bad_chars(Str, Str1, BadChars-[]),
	(   BadChars = [] ->
	    Atom1 = Atom
	;
	    format('~N*** Warning: suspicious chars ~w removed from "~w"~n', [BadChars, Atom]),
	    atom_codes(Atom1, Str1)
	),
	!.
remove_bad_chars_from_atom(Atom, Atom1) :-
	format('~N*** Error: bad call: ~w~n', [remove_bad_chars_from_atom(Atom, Atom1)]),
	fail.

remove_bad_chars([], [], Bad-Bad).
remove_bad_chars([BadChar | R], Out, [BadChar | BadNext]-BadOut) :-
	bad_char(BadChar),
	!,
	remove_bad_chars(R, Out, BadNext-BadOut).
remove_bad_chars([F | R], [F | R1], BadIn-BadOut) :-
	!,
	remove_bad_chars(R, R1, BadIn-BadOut).

% We seem to get odd chars with values around 65000 from Excel - we don't know why! Delete them...
bad_char(BadChar) :-
	BadChar > 65000.

print_number_of_source_version1_version2_used(Label) :-
	findall([NormalisedSource, NormalisedT1, NormalisedT2],
		source_version1_version2(NormalisedSource, NormalisedT1, NormalisedT2, use),
		Tuples),
	length(Tuples, NTuples),
	format('~N--- ~d stored examples for ~w~n', [NTuples, Label]),
	!.

%======================================================================

process_amt_list(ParsedAMTList, ProcessedAMTData) :-
	pre_process_amt_list(ParsedAMTList, PreProcessedAMTList),
	group_amt_list(PreProcessedAMTList, ProcessedAMTData).

pre_process_amt_list([], []).
pre_process_amt_list([F | R], [F1Sorted | R1]) :-
	pre_process_amt_line(F, F1),
	sort(F1, F1Sorted),
	!,
	pre_process_amt_list(R, R1).

pre_process_amt_line([], []).
pre_process_amt_line([F | R], [F1 | R1]) :-
	pre_process_amt_line_item(F, F1),
	!,
	pre_process_amt_line(R, R1).
pre_process_amt_line([_F | R], R1) :-
	!,
	pre_process_amt_line(R, R1).

pre_process_amt_line_item('WorkerId'-Id, 'WorkerId'-Id) :-
	!.
pre_process_amt_line_item(input(Tag)-Value, [I, BasicTag]-CleanValue) :-
	decompose_tag(Tag, I, BasicTag),
	clean_value(Value, CleanValue),
	!.
pre_process_amt_line_item(answer(Tag)-Value, [I, BasicTag]-CleanValue) :-
	decompose_tag(Tag, I, BasicTag),
	clean_value(Value, CleanValue),
	!.

decompose_tag(Tag, I, BasicTag) :-
	atom_codes(Tag, TagStr),
	basic_tag_string(BasicTagStr),
	append(BasicTagStr, NumChars, TagStr),
	number_codes(I, NumChars),
	atom_codes(BasicTag, BasicTagStr),
	!.

basic_tag_string("Item").
basic_tag_string("ORIGINAL").
basic_tag_string("FIRST-TRANSLATION").
basic_tag_string("SECOND-TRANSLATION").


clean_value(Value, NormalisedCleanValue) :-
	atom_codes(Value, ValueStr),
	clean_value_str(ValueStr, CleanValueStr),
	atom_codes(CleanValue, CleanValueStr),
	normalise_translation(CleanValue, NormalisedCleanValue).

clean_value_str([], []).
clean_value_str(In, Out) :-
	string_to_clean(Clean),
	append(Clean, Next, In),
	!,
	clean_value_str(Next, Out).
clean_value_str([F | R], [F | R1]) :-
	!,
	clean_value_str(R, R1).

string_to_clean("<font color=\"#ff0033\">").
string_to_clean("</font>").

%======================================================================

group_amt_list(PreProcessedAMTList, ProcessedAMTData) :-
	group_amt_list_by_sheet(PreProcessedAMTList, GroupedAMTList),
	group_amt_sheets(GroupedAMTList, ProcessedAMTData).

group_amt_list_by_sheet([], []).
group_amt_list_by_sheet([F | R], [F1 | R1]) :-
	group_amt_sheet(F, F1),
	!,
	group_amt_list_by_sheet(R, R1).

group_amt_sheet(Sheet, GroupedSheet) :-
	member('WorkerId'-WorkerId, Sheet),
	all_ids_in_sheet(Sheet, Ids),
	%length(Ids, NIds),
	%format('~NFound ~d IDs in sheet: ~w~n', [NIds, Ids]),
	group_amt_sheet1(Ids, Sheet, WorkerId, GroupedSheet),
	!.
group_amt_sheet(Sheet, GroupedSheet) :-
	format('~N*** Error: bad call: ~w~n', [group_amt_sheet(Sheet, GroupedSheet)]),
	fail.

all_ids_in_sheet(Sheet, Ids) :-
	findall(Id,
		member([Id, _Tag]-_Value, Sheet),
		Ids0),
	sort(Ids0, Ids).

group_amt_sheet1([], _Sheet, _WorkerId, []).
group_amt_sheet1([Id | Ids], Sheet, WorkerId, [Group | Groups]) :-
	group_data_in_sheet_for_id(Id, Sheet, WorkerId, Group),
	!,
	group_amt_sheet1(Ids, Sheet, WorkerId, Groups).

group_data_in_sheet_for_id(Id, Sheet, WorkerId, Group) :-
	member([Id, 'ORIGINAL']-Source, Sheet),
	member([Id, 'FIRST-TRANSLATION']-FirstTranslation, Sheet),
	member([Id, 'SECOND-TRANSLATION']-SecondTranslation, Sheet),
	member([Id, 'Item']-Judgement, Sheet),
	Group = [source=Source, trans1=FirstTranslation, trans2=SecondTranslation, judgement=[Judgement, WorkerId]],
	!.
group_data_in_sheet_for_id(Id, Sheet, WorkerId, Group) :-
	format('~N*** Error: bad call: ~w~n', [group_data_in_sheet_for_id(Id, Sheet, WorkerId, Group)]),
	fail.

%======================================================================

group_amt_sheets(GroupedAMTList, ProcessedAMTData) :-
	all_source_t1_t2_in_list(GroupedAMTList, SourcesEtc),
	group_amt_sheets1(SourcesEtc, GroupedAMTList, ProcessedAMTData0),
	safe_remove_duplicates_preserving_order(ProcessedAMTData0, ProcessedAMTData),
	length(ProcessedAMTData, NProcessedAMTData),
	format('~N--- Found ~d relevant triples in sheets~n', [NProcessedAMTData]).

all_source_t1_t2_in_list(GroupedAMTList, SourcesEtc) :-
	findall([Source, T1, T2],
		(   member(GroupedSheet, GroupedAMTList),
		    member(Group, GroupedSheet),
		    member(source=Source, Group),
		    Source \== 'NO ITEM',
		    member(trans1=T1, Group),
		    member(trans2=T2, Group)
		),
		SourcesEtc0),
	sort(SourcesEtc0, SourcesEtc),
	length(SourcesEtc, NSourcesEtc),
	format('~N--- Found ~d <source, t1, t2> triples in sheets~n', [NSourcesEtc]),
	warn_if_source_t1_t2_triples_not_loaded(SourcesEtc, 0-NNotLoaded),
	(   NNotLoaded > 0 ->
	    format('~N*** Warning: ~d triples from AMT file not in original data~n', [NNotLoaded])
	;
	    otherwise ->
	    true
	).	    

group_amt_sheets1([], _GroupedAMTList, []).
group_amt_sheets1([SourceEtc | SourcesEtc], GroupedAMTList, Out) :-
	group_for_source_etc(SourceEtc, GroupedAMTList, Group),
	(   member(judgements=dont_use, Group) ->
	    Out = Groups
	;
	    otherwise ->
	    Out = [Group | Groups]
	),
	!,
	group_amt_sheets1(SourcesEtc, GroupedAMTList, Groups).

group_for_source_etc([Source, FirstTranslation, SecondTranslation], GroupedAMTList, Group1WithSummary) :-
	findall(JudgementPair,
		(   member(GroupedSheet, GroupedAMTList),
		    member([source=Source, trans1=FirstTranslation, trans2=SecondTranslation | Rest], GroupedSheet),
		    member(judgement=JudgementPair, Rest)
		),
		JudgementPairs),
	Group0 = [source=Source, trans1=FirstTranslation, trans2=SecondTranslation, judgements=JudgementPairs],
	reverse_order_in_group_if_necessary(Group0, Group1),
	add_summary_judgements(Group1, Group1WithSummary),
	!.
group_for_source_etc(Source, GroupedAMTList, Group1) :-
	format('~N*** Error: bad call: ~w~n', [group_for_source_etc(Source, GroupedAMTList, Group1)]),
	fail.

warn_if_source_t1_t2_triples_not_loaded([], N-N).
warn_if_source_t1_t2_triples_not_loaded([F | R], NIn-NOut) :-
	warn_if_source_t1_t2_triple_not_loaded(F, NIn-NNext),
	!,
	warn_if_source_t1_t2_triples_not_loaded(R, NNext-NOut).

warn_if_source_t1_t2_triple_not_loaded([Source, T1, T2], NIn-NOut) :-
	(   source_t1_t2_triple_loaded(Source, T1, T2) ->
	    NOut = NIn
	;
	    otherwise ->
	    format('~N*** Warning: triple ~n<"~w",~n "~w",~n "~w">~n from AMT file not in original data~n',
		   [Source, T1, T2]),
	    NOut is NIn + 1
	).

source_t1_t2_triple_loaded(Source, T1, T2) :-
	source_version1_version2(Source, T1, T2, _Use),
	!.
source_t1_t2_triple_loaded(Source, T1, T2) :-
	source_version1_version2(Source, T2, T1, _Use),
	!.
source_t1_t2_triple_loaded(_Source, T1, _T2) :-
	version1_translation(T1, _Use),
	!.
source_t1_t2_triple_loaded(_Source, _T1, T2) :-
	version1_translation(T2, _Use),
	!.

%======================================================================

reverse_order_in_group_if_necessary(InGroup, OutGroup) :-
	InGroup = [source=Source, trans1=T1, trans2=T2, judgements=JudgementPairs],
	(   (   source_version1_version2(Source, T1, T2, use)
	    ;
		version1_translation(T1, use) ) ->
	    OutGroup = InGroup
	;
	    (   source_version1_version2(Source, T2, T1, use)
	    ;
		version1_translation(T2, use) ) ->
	    reverse_judgement_pairs(JudgementPairs, ReversedJudgementPairs),
	    OutGroup = [source=Source, trans1=T2, trans2=T1, judgements=ReversedJudgementPairs]
	;
	    (   source_version1_version2(Source, T1, T2, dont_use)
	    ;   source_version1_version2(Source, T2, T1, dont_use)
	    ;   version1_translation(T1, dont_use)
	    ;   version1_translation(T2, dont_use) ) ->
	    OutGroup = [source=Source, trans1=T2, trans2=T1, judgements=dont_use]
	;
	    warn_missing_translation(Source, T1, T2),
	    OutGroup = [source=Source, trans1=T2, trans2=T1, judgements=dont_use]
	),
	!.
reverse_order_in_group_if_necessary(InGroup, OutGroup) :-
	format('~N*** Error: bad call: ~w~n', [reverse_order_in_group_if_necessary(InGroup, OutGroup)]),
	fail.

reverse_judgement_pairs([], []).
reverse_judgement_pairs([F | R], [F1 | R1]) :-
	reverse_judgement_pair(F, F1),
	!,
	reverse_judgement_pairs(R, R1).

reverse_judgement_pair([Judgement, WorkerId], [ReversedJudgement, WorkerId]) :-
	reverse_judgement(Judgement, ReversedJudgement),
	!.
reverse_judgement_pair(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [reverse_judgement_pair(F, F1)]),
	fail.

reverse_judgement('FirstClearlyBetter', 'SecondClearlyBetter') :- !.
reverse_judgement('FirstSlightlyBetter', 'SecondSlightlyBetter') :- !.
reverse_judgement('Equal', 'Equal') :- !.
reverse_judgement('SecondClearlyBetter', 'FirstClearlyBetter') :- !.
reverse_judgement('SecondSlightlyBetter', 'FirstSlightlyBetter') :- !.

warn_missing_translation(Source, T1, T2) :-
	format('~N*** Warning: unable to determine which of "~w" nor "~w" is version 1 of "~w"~n', [T1, T2, Source]).
	%format('~N~q~n', [source_version1_version2(Source, T1, T2, use)]),
	%atom_codes(Source, SourceStr),
	%atom_codes(T1, T1Str),
	%atom_codes(T1, T2Str),
	%format('~NSource = ~q~nT1 = ~q~nT2 = ~q~n', [SourceStr, T1Str, T2Str]),
	%format('~N~q~n', [source_version1_version2(Source, T1, T2, use)]),
	%format('~N~q~n', [source_version1_version2(Source, T2, T1, use)]),

%======================================================================

% Remove suspect judgements and recompute

revise_all_summary_judgements_if_necessary(Group, RevisedGroup, ChangedP) :-
	(   \+ suspect_judge(_) ->
	    Group = RevisedGroup,
	    ChangedP = not_changed,
	    format('~N--- Unnecessary to revise figures since no suspect judges~n', [])
	;
	    otherwise ->
	    revise_all_summary_judgements(Group, RevisedGroup, 0-N),
	    ChangedP = changed,
	    format('~N--- Revised figures to remove suspect judges. ~d majority judgements changed ~n', [N])
	).

revise_all_summary_judgements([], [], N-N).
revise_all_summary_judgements([F | R], [F1 | R1], In-Out) :-
	revise_summary_judgements(F, F1, MajorityJudgementChanged),
	(   MajorityJudgementChanged = changed ->
	    Next is In + 1
	;
	    otherwise ->
	    Next = In
	),
	!,
	revise_all_summary_judgements(R, R1, Next-Out).

revise_summary_judgements(Group, RevisedGroup, MajorityJudgementChanged) :-
	member(judgements=OldJudgementPairs, Group),
	member(majority_judgement=OldMajority, Group),
	remove_judgments_majority_and_agreement(Group, ReducedGroup),
	remove_suspect_judgements(OldJudgementPairs, RevisedJudgementPairs),
	majority_judgement(RevisedJudgementPairs, RevisedMajority),
	agreement_in_judgement(RevisedJudgementPairs, RevisedAgreement),
	append(ReducedGroup,
	       [judgements=RevisedJudgementPairs, majority_judgement=RevisedMajority, agreement=RevisedAgreement],
	       RevisedGroup),
	(   OldMajority = RevisedMajority ->
	    MajorityJudgementChanged = not_changed
	;
	    otherwise ->
	    MajorityJudgementChanged = changed
	),
	!.
revise_summary_judgements(Group, RevisedGroup, MajorityJudgementChanged) :-
	format('~N*** Error: bad call: ~w~n',
	       [revise_summary_judgements(Group, RevisedGroup, MajorityJudgementChanged)]),
	fail.

remove_judgments_majority_and_agreement([], []).
remove_judgments_majority_and_agreement([Key=_Value | R], R1) :-
	member(Key, [judgements, majority_judgement, agreement]),
	!,
	remove_judgments_majority_and_agreement(R, R1).
remove_judgments_majority_and_agreement([F | R], [F | R1]) :-
	!,
	remove_judgments_majority_and_agreement(R, R1).

remove_suspect_judgements([], []).
remove_suspect_judgements([[_Judgement, Judge] | R], R1) :-
	suspect_judge(Judge),
	!,
	remove_suspect_judgements(R, R1).
remove_suspect_judgements([F | R], [F | R1]) :-
	!,
	remove_suspect_judgements(R, R1).

%======================================================================

add_summary_judgements(Group, Group) :-
	member(judgements=dont_use, Group),
	!.
add_summary_judgements(Group, GroupWithSummary) :-
	member(judgements=JudgementPairs, Group),
	majority_judgement(JudgementPairs, Majority),
	agreement_in_judgement(JudgementPairs, Agreement),
	append(Group,
	       [majority_judgement=Majority, agreement=Agreement],
	       GroupWithSummary),
	!.
add_summary_judgements(Group, GroupWithSummary) :-
	format('~N*** Error: bad call: ~w~n', [add_summary_judgements(Group, GroupWithSummary)]),
	fail.

%======================================================================

:- dynamic majority_judgement_type/1.

set_majority_judgement_type(Type) :-
	check_known_majority_judgement_type(Type),
	retractall(majority_judgement_type(_)),
	assertz(majority_judgement_type(Type)).

get_majority_judgement_type(Type) :-
	majority_judgement_type(Type1),
	!,
	Type = Type1.
get_majority_judgement_type(Type) :-
	Type = strict.

check_known_majority_judgement_type(Type) :-
	known_majority_judgement_type(Type),
	!.
check_known_majority_judgement_type(Type) :-
	format('~N*** Error: unknown majority judging type "~w"~n', [Type]),
	fail.

known_majority_judgement_type(relaxed).
known_majority_judgement_type(strict).
known_majority_judgement_type(unanimous).

%======================================================================

majority_judgement(JudgementPairs, Majority) :-
	get_majority_judgement_type(Type),
	majority_judgement(Type, JudgementPairs, Majority).

majority_judgement(relaxed, JudgementPairs, Majority) :-
	count_first_better(JudgementPairs, NFirstBetter),
	count_second_better(JudgementPairs, NSecondBetter),
	(   NFirstBetter > NSecondBetter ->
	    Majority = 'FirstBetter'
	;
	    NSecondBetter > NFirstBetter ->
	    Majority = 'SecondBetter'
	;
	    otherwise ->
	    Majority = 'Unclear'
	),
	!.
majority_judgement(strict, JudgementPairs, Majority) :-
	length(JudgementPairs, NJudgementPairs),
	NJudgementPairsOver2 is NJudgementPairs / 2.0,
	count_equal(JudgementPairs, NEqual),
	count_first_better(JudgementPairs, NFirstBetter),
	count_second_better(JudgementPairs, NSecondBetter),
	(   NEqual > NJudgementPairsOver2 ->
	    Majority = 'Equal'
	;
	    NFirstBetter > NJudgementPairsOver2 ->
	    Majority = 'FirstBetter'
	;
	    NSecondBetter > NJudgementPairsOver2 ->
	    Majority = 'SecondBetter'
	;
	    otherwise ->
	    Majority = 'Unclear'
	),
	!.
majority_judgement(unanimous, JudgementPairs, Majority) :-
	length(JudgementPairs, NJudgementPairs),
	count_equal(JudgementPairs, NEqual),
	count_first_better(JudgementPairs, NFirstBetter),
	count_second_better(JudgementPairs, NSecondBetter),
	(   NEqual = NJudgementPairs ->
	    Majority = 'Equal'
	;
	    NFirstBetter = NJudgementPairs ->
	    Majority = 'FirstBetter'
	;
	    NSecondBetter = NJudgementPairs ->
	    Majority = 'SecondBetter'
	;
	    otherwise ->
	    Majority = 'Unclear'
	),
	!.
		
agreement_in_judgement(JudgementPairs, Agreement) :-
	length(JudgementPairs, N),
	count_first_better(JudgementPairs, NFirstBetter),
	count_second_better(JudgementPairs, NSecondBetter),
	count_first_clearly_better(JudgementPairs, NFirstClearlyBetter),
	count_second_clearly_better(JudgementPairs, NSecondClearlyBetter),
	(   ( NFirstBetter = N ; NSecondBetter = N ; ( NFirstBetter = 0, NSecondBetter = 0) ) ->
	    Agreement = 'Unanimous'
	;
	    ( ( NFirstBetter > 0, NSecondBetter = 0 ) ; ( NFirstBetter = 0, NSecondBetter > 0 ) ) ->
	    Agreement = 'Agree'
	;
	    ( NFirstClearlyBetter > 0, NSecondClearlyBetter > 0) ->
	    Agreement = 'StrongDisagree'
	;
	    otherwise ->
	    Agreement = 'WeakDisagree'
	),
	!.

count_equal(JudgementPairs, NEqual) :-
	findall(x,
		(   member([Judgement, _], JudgementPairs),
		    member(Judgement, ['Equal'])
		),
		Xs),
	length(Xs, NEqual).

count_first_better(JudgementPairs, NFirstBetter) :-
	findall(x,
		(   member([Judgement, _], JudgementPairs),
		    member(Judgement, ['FirstClearlyBetter', 'FirstSlightlyBetter'])
		),
		Xs),
	length(Xs, NFirstBetter).

count_second_better(JudgementPairs, NSecondBetter) :-
	findall(x,
		(   member([Judgement, _], JudgementPairs),
		    member(Judgement, ['SecondClearlyBetter', 'SecondSlightlyBetter'])
		),
		Xs),
	length(Xs, NSecondBetter).

count_first_clearly_better(JudgementPairs, NFirstBetter) :-
	findall(x,
		(   member([Judgement, _], JudgementPairs),
		    member(Judgement, ['FirstClearlyBetter'])
		),
		Xs),
	length(Xs, NFirstBetter).

count_second_clearly_better(JudgementPairs, NSecondBetter) :-
	findall(x,
		(   member([Judgement, _], JudgementPairs),
		    member(Judgement, ['SecondClearlyBetter'])
		),
		Xs),
	length(Xs, NSecondBetter).

%======================================================================

maybe_write_full_judgements_file(ProcessedAMTData, FullJudgementsFile) :-
	(   FullJudgementsFile = '*no_file*' ->
	    true
	;
	    write_full_judgements_file(ProcessedAMTData, FullJudgementsFile)
	).

write_full_judgements_file(ProcessedAMTData, File) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S, [encoding('UTF-8')]),

	length(ProcessedAMTData, NTriples),
	count_judgements(ProcessedAMTData, NJudgements),

	write_full_judgements_list(ProcessedAMTData, S),

	close(S),
	format('~N--- Written judgements file (~d triples, ~d judgements): ~w~n',
	       [NTriples, NJudgements, AbsFile]).

count_judgements(ProcessedAMTData, NJudgements) :-
	findall(JudgementPair,
		(   member(Record, ProcessedAMTData),
		    member(judgements=JudgementPairs, Record),
		    member(JudgementPair, JudgementPairs)
		),
		AllJudgementPair),
	length(AllJudgementPair, NJudgements).

write_full_judgements_list([], _S).
write_full_judgements_list([F | R], S) :-
	write_full_judgements_item(F, S),
	!,
	write_full_judgements_list(R, S).

write_full_judgements_item(Item, S) :-
	member(source=Source, Item),
	member(trans1=T1, Item),
	member(trans2=T2, Item),
	member(judgements=JudgementPairs0, Item),
	format(S, '~N~w\t~w\t~w', [Source, T1, T2]),
	add_null_judgements_if_necessary(JudgementPairs0, JudgementPairs),
	write_judgements_pairs(JudgementPairs, S),
	format(S, '~n', []),
	!.
write_full_judgements_item(Item, S) :-
	format('~N*** Error: bad call: ~w~n', [write_full_judgements_item(Item, S)]),
	fail.

add_null_judgements_if_necessary([], [[null, null], [null, null], [null, null]]) :-
	!.
add_null_judgements_if_necessary([X], [X, [null, null], [null, null]]) :-
	!.
add_null_judgements_if_necessary([X, Y], [X, Y, [null, null]]) :-
	!.
add_null_judgements_if_necessary([X, Y, Z | _Rest], [X, Y, Z]) :-
	!.

write_judgements_pairs([], _S).
write_judgements_pairs([F | R], S) :-
	write_judgements_pair(F, S),
	!,
	write_judgements_pairs(R, S).

write_judgements_pair([Judgement, _Judge], S) :-
	format(S, '\t~w', [Judgement]),
	!.

%======================================================================

add_sentence_bleu_reference_and_preedited_to_full_judgements_file(InFile, SourceFile, PreeditedFile, RefFile, Bleu1File, Bleu2File, OutFile, Encoding) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	store_source_preedited_associations(SourceFile, PreeditedFile),
	store_source_reference_associations(SourceFile, RefFile),
	store_source_bleu_associations(SourceFile, Bleu1File, Bleu2File),
	
	csv_file_to_list_of_lists(AbsInFile, Encoding, 0'|, 0'\t, CSVListIn),  %"'
	length(CSVListIn, InN),
	format('~N--- Read CSV file (~d records) ~w~n', [InN, AbsInFile]),
	
	add_reference_preedited_and_sentence_bleu_to_list(CSVListIn, CSVListOut),
	show_bleu_majority_agreement(CSVListOut),

	sentence_bleu_reference_and_preedited_header_line(HeaderLine),
	list_to_html_file([HeaderLine | CSVListOut], AbsOutFile),
	length(CSVListOut, OutN),
	format('~N--- Written HTML file (~d records) ~w~n', [OutN, AbsOutFile]),
	!.

add_reference_preedited_and_sentence_bleu_to_list([], []).
add_reference_preedited_and_sentence_bleu_to_list([F | R], [F1 | R1]) :-
	add_reference_preedited_and_sentence_bleu_to_line(F, F1),
	!,
	add_reference_preedited_and_sentence_bleu_to_list(R, R1).

sentence_bleu_reference_and_preedited_header_line(['Source', 'Preedited', 'Target1', 'Target2', 'Reference', 'Majority', 'B-Diff', 'BLEU1', 'BLEU2', 'Judge1', 'Judge2','Judge3']).

add_reference_preedited_and_sentence_bleu_to_line(Line, Line1) :-
	Line = [Source, Tr1, Tr2 | Judgements],
	mark_diffs(Tr1, Tr2, Tr1Marked, Tr2Marked, _NDiffsTr1Tr2),

	get_preedited_for_source(Source, Preedited),
	get_reference_for_source(Source, Reference),
	
	mark_diffs(Source, Preedited, SourceMarked, PreeditedMarked, _NDiffsSourcePre),
	
	get_bleu_for_source(Source, Bleu1, Bleu2),
	
	add_dummy_judges_to_judgements(Judgements, JudgementPairs),
	majority_judgement(JudgementPairs, Majority),
	majority_judgement_to_atom(Majority, MajorityAtom),
	BleuDiff is Bleu1 - Bleu2,
	bleu_diff_to_atom(BleuDiff, BleuDiffAtom),
	format_to_atom('~3f', [Bleu1], Bleu1Atom),
	format_to_atom('~3f', [Bleu2], Bleu2Atom),
	
	Line1 = [SourceMarked, PreeditedMarked, Tr1Marked, Tr2Marked, Reference, MajorityAtom, BleuDiffAtom, Bleu1Atom, Bleu2Atom | Judgements],
	!.
add_reference_preedited_and_sentence_bleu_to_line(Line, Line1) :-
	format('~N*** Error: bad call: ~w~n', [add_reference_preedited_and_sentence_bleu_to_line(Line, Line1)]),
	fail.

bleu_diff_to_atom(BleuDiff, BleuDiffAtom) :-
	format_to_atom('~3f', [BleuDiff], BleuDiffAtom0),
	(   BleuDiff > 0 ->
	    color_code_atom(BleuDiffAtom0, blue, BleuDiffAtom)
	;
	    BleuDiff < 0 ->
	    color_code_atom(BleuDiffAtom0, red, BleuDiffAtom)
	;
	    otherwise ->
	    BleuDiffAtom = BleuDiffAtom0
	).

majority_judgement_to_atom(Majority, MajorityAtom) :-
	(   Majority = 'FirstBetter' ->
	    color_code_atom(Majority, blue, MajorityAtom)
	;
	    Majority = 'SecondBetter' ->
	    color_code_atom(Majority, red, MajorityAtom)
	;
	    otherwise ->
	    MajorityAtom = Majority
	).

color_code_atom(Atom, Color, ColouredAtom) :-
	color_id_to_html_color(Color, HTMLColor),
	format_to_atom('<FONT COLOR="~w">~w</FONT>', [HTMLColor, Atom], ColouredAtom).

color_id_to_html_color(red, '#ff0033').       
color_id_to_html_color(blue, '#0000cc').   

add_dummy_judges_to_judgements([], []).
add_dummy_judges_to_judgements([F | R], [[F, judge] | R1]) :-
	add_dummy_judges_to_judgements(R, R1).

:- dynamic source_preedited/2.

get_preedited_for_source(Source, Preedited) :-
	source_preedited(Source, Preedited),
	!.
get_preedited_for_source(Source, Preedited) :-
	format('~N*** Error: bad call: ~w~n', [get_preedited_for_source(Source, Preedited)]),
	fail.

store_source_preedited_associations(SourceFile, PreeditedFile) :-
	retractall(source_preedited(_, _)),
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(PreeditedFile, AbsPreeditedFile),

	read_translation_file(AbsSourceFile, SourceList, NSource),
	read_translation_file(AbsPreeditedFile, PreeditedList, NPreedited),

	(   all_numbers_are_same([NSource, NPreedited]) ->
	    true
	;
	    format('~N*** Error: files not same length~n', []),
	    fail
	),

	store_source_preedited_associations1(SourceList, PreeditedList),
	format('~N--- Stored source/preedited associations~n', []).

store_source_preedited_associations1([], []).
store_source_preedited_associations1([F | R], [F1 | R1]) :-
	store_source_preedited_association(F, F1),
	!,
	store_source_preedited_associations1(R, R1).

store_source_preedited_association(SourceAtom, PreeditedAtom) :-
	normalise_translation(SourceAtom, NormalisedSourceAtom),
	normalise_translation(PreeditedAtom, NormalisedPreeditedAtom),
	assertz(source_preedited(NormalisedSourceAtom, NormalisedPreeditedAtom)),
	!.
store_source_preedited_association(SourceAtom, PreeditedAtom) :-
	format('~N*** Error: bad call: ~w~n', [store_source_preedited_association(SourceAtom, PreeditedAtom)]),
	fail.

:- dynamic source_reference/2.

get_reference_for_source(Source, Reference) :-
	source_reference(Source, Reference),
	!.
get_reference_for_source(Source, Reference) :-
	format('~N*** Error: bad call: ~w~n', [get_reference_for_source(Source, Reference)]),
	fail.

store_source_reference_associations(SourceFile, ReferenceFile) :-
	retractall(source_reference(_, _)),
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(ReferenceFile, AbsReferenceFile),

	read_translation_file(AbsSourceFile, SourceList, NSource),
	read_translation_file(AbsReferenceFile, ReferenceList, NReference),

	(   all_numbers_are_same([NSource, NReference]) ->
	    true
	;
	    format('~N*** Error: files not same length~n', []),
	    fail
	),

	store_source_reference_associations1(SourceList, ReferenceList),
	format('~N--- Stored source/reference associations~n', []).

store_source_reference_associations1([], []).
store_source_reference_associations1([F | R], [F1 | R1]) :-
	store_source_reference_association(F, F1),
	!,
	store_source_reference_associations1(R, R1).

store_source_reference_association(SourceAtom, ReferenceAtom) :-
	normalise_translation(SourceAtom, NormalisedSourceAtom),
	normalise_translation(ReferenceAtom, NormalisedReferenceAtom),
	assertz(source_reference(NormalisedSourceAtom, NormalisedReferenceAtom)),
	!.
store_source_reference_association(SourceAtom, ReferenceAtom) :-
	format('~N*** Error: bad call: ~w~n', [store_source_reference_association(SourceAtom, ReferenceAtom)]),
	fail.

:- dynamic source_bleu1_bleu2/3.

get_bleu_for_source(SourceAtom, Bleu1, Bleu2) :-
	normalise_translation(SourceAtom, NormalisedSourceAtom),
	source_bleu1_bleu2(NormalisedSourceAtom, Bleu1, Bleu2),
	!.
get_bleu_for_source(SourceAtom, Bleu1, Bleu2) :-
	format('~N*** Error: bad call: ~w~n', [get_bleu_for_source(SourceAtom, Bleu1, Bleu2)]),
	fail.

store_source_bleu_associations(SourceFile, Bleu1File, Bleu2File) :-
	retractall(source_bleu1_bleu2(_, _, _)),
	safe_absolute_file_name(SourceFile, AbsSourceFile),
	safe_absolute_file_name(Bleu1File, AbsBleu1File),
	safe_absolute_file_name(Bleu2File, AbsBleu2File),

	read_translation_file(AbsSourceFile, SourceList, NSource),
	read_translation_file(AbsBleu1File, Bleu1List, NBleu1),
	read_translation_file(AbsBleu2File, Bleu2List, NBleu2),

	(   all_numbers_are_same([NSource, NBleu1, NBleu2]) ->
	    true
	;
	    format('~N*** Error: files not all same length~n', []),
	    fail
	),

	store_source_bleu_associations1(SourceList, Bleu1List, Bleu2List),
	format('~N--- Stored source/BLEU associations~n', []).

store_source_bleu_associations1([], [], []).
store_source_bleu_associations1([F | R], [F1 | R1], [F2 | R2]) :-
	store_source_bleu_association(F, F1, F2),
	!,
	store_source_bleu_associations1(R, R1, R2).

store_source_bleu_association(SourceAtom, Bleu1Atom, Bleu2Atom) :-
	normalise_translation(SourceAtom, NormalisedSourceAtom),
	atom_codes(Bleu1Atom, Bleu1Codes),
	safe_number_codes(Bleu1, Bleu1Codes),
	atom_codes(Bleu2Atom, Bleu2Codes),
	safe_number_codes(Bleu2, Bleu2Codes),
	assertz(source_bleu1_bleu2(NormalisedSourceAtom, Bleu1, Bleu2)),
	!.
store_source_bleu_association(SourceAtom, Bleu1Atom, Bleu2Atom) :-
	format('~N*** Error: bad call: ~w~n', [store_source_bleu_association(SourceAtom, Bleu1Atom, Bleu2Atom)]),
	fail.

%======================================================================

% [SourceMarked, PreeditedMarked, Tr1Marked, Tr2Marked, Reference, MajorityAtom, BleuDiffAtom, Bleu1Atom, Bleu2Atom | Judgements],

show_bleu_majority_agreement(HTMLList) :-
	restore_bleu_to_numbers_in_list(HTMLList, HTMLList1),
	count_bleu_majority_agreement(HTMLList1, 0-Agree, 0-Disagree),
	average_bleu_diff_for_majority(HTMLList1, 'FirstBetter', FirstBetterAv),
	average_bleu_diff_for_majority(HTMLList1, 'Equal', EqualAv),
	average_bleu_diff_for_majority(HTMLList1, 'Unclear', UnclearAv),
	average_bleu_diff_for_majority(HTMLList1, 'SecondBetter', SecondBetterAv),
	format('~NBLEU diff and majority agree:    ~d~n', [Agree]),
	format('~NBLEU diff and majority disagree: ~d~n', [Disagree]),
	format('~NAverage BLEU diff for:~n', []),
	format('~N  FirstBetter:  ~3f~n', [FirstBetterAv]),
	format('~N  SecondBetter: ~3f~n', [SecondBetterAv]),
	format('~N  Equal:        ~3f~n', [EqualAv]),
	format('~N  Unclear:      ~3f~n', [UnclearAv]),
	!.

restore_bleu_to_numbers_in_list([], []).
restore_bleu_to_numbers_in_list([F | R], [F1 | R1]) :-
	restore_bleu_to_numbers_in_record(F, F1),
	!,
	restore_bleu_to_numbers_in_list(R, R1).

restore_bleu_to_numbers_in_record(F, F1) :-
	F = [SourceMarked, PreeditedMarked, Tr1Marked, Tr2Marked, Reference, MajorityAtom, BleuDiffAtom, Bleu1Atom, Bleu2Atom | Judgements],
	F1 = [SourceMarked, PreeditedMarked, Tr1Marked, Tr2Marked, Reference, MajorityAtom1, BleuDiff, Bleu1Atom, Bleu2Atom | Judgements],
	strip_colour_markings(MajorityAtom, MajorityAtom1),
	color_marked_number_atom_to_number(BleuDiffAtom, BleuDiff),
	!.
restore_bleu_to_numbers_in_record(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [restore_bleu_to_numbers_in_record(F, F1)]),
	fail.

color_marked_number_atom_to_number(BleuDiffAtom, BleuDiff) :-
	strip_colour_markings(BleuDiffAtom, BleuDiffAtom1),
	atom_codes(BleuDiffAtom1, BleuDiffChars),
	number_codes(BleuDiff, BleuDiffChars),
	!.

strip_colour_markings(BleuDiffAtom, BleuDiff) :-
	atom_codes(BleuDiffAtom, BleuDiffCharsIn),
	append(BleuDiffChars1, "</FONT>", BleuDiffCharsIn),
	(   append("<FONT COLOR=\"#ff0033\">", BleuDiffCharsOut, BleuDiffChars1)
	;
	    append("<FONT COLOR=\"#0000cc\">", BleuDiffCharsOut, BleuDiffChars1)
	),
	atom_codes(BleuDiff, BleuDiffCharsOut),
	!.
strip_colour_markings(BleuDiffCharsIn, BleuDiffCharsIn).	

count_bleu_majority_agreement([], Agree-Agree, Disagree-Disagree).
count_bleu_majority_agreement([F | R], AgreeIn-AgreeOut, DisagreeIn-DisagreeOut) :-
	count_bleu_majority_agreement_record(F, AgreeIn-AgreeNext, DisagreeIn-DisagreeNext),
	!,
	count_bleu_majority_agreement(R, AgreeNext-AgreeOut, DisagreeNext-DisagreeOut).

count_bleu_majority_agreement_record(F, AgreeIn-AgreeOut, DisagreeIn-DisagreeOut) :-
	(   bleu_agrees_with_majority(F) ->
	    AgreeOut is AgreeIn + 1,
	    DisagreeOut is DisagreeIn
	;
	    otherwise ->
	    AgreeOut is AgreeIn,
	    DisagreeOut is DisagreeIn + 1
	),
	!.

bleu_agrees_with_majority(F) :-
	F = [_SourceMarked, _PreeditedMarked, _Tr1Marked, _Tr2Marked, _Reference, MajorityAtom, BleuDiff | _],
	bleu_agrees_with_majority1(MajorityAtom, BleuDiff).

bleu_agrees_with_majority1('FirstBetter', BleuDiff) :-
	BleuDiff > 0.0,
	!.
bleu_agrees_with_majority1('SecondBetter', BleuDiff) :-
	BleuDiff < 0.0,
	!.
bleu_agrees_with_majority1('Equal', BleuDiff) :-
	-0.01 < BleuDiff, BleuDiff < 0.01,
	!.

average_bleu_diff_for_majority(HTMLList1, MajorityAtom, Average) :-
	findall(BleuDiff,
		(   member(Record, HTMLList1),
		    Record = [_SourceMarked, _PreeditedMarked, _Tr1Marked, _Tr2Marked, _Reference, MajorityAtom, BleuDiff | _]
		),
		BleuDiffs),
	safe_sum_list(BleuDiffs, Total),
	length(BleuDiffs, N),
	(   N > 0 ->
	    Average is Total / N
	;
	    otherwise ->
	    Average = 0.0
	).

%======================================================================

print_processed_amt_judgements(ProcessedAMTData0, SummaryFile, PrologSummaryFile) :-
	open(SummaryFile, write, S, [encoding('UTF-8')]),
	
	format(S, '~NSUMMARY~n', []),
	print_processed_amt_judgements_summary(ProcessedAMTData0, S),
	print_amt_judge_summary(ProcessedAMTData0, S),
	
	maybe_add_ngram_and_frequency_information(ProcessedAMTData0, ProcessedAMTData0WithNgrams),
	print_processed_amt_judgements1(S, ProcessedAMTData0WithNgrams),
	
	mark_suspect_judges(ProcessedAMTData0),
	revise_all_summary_judgements_if_necessary(ProcessedAMTData0, ProcessedAMTData, ChangedP),

	(   ChangedP = changed ->
	    format(S, '~NSUMMARY AFTER REMOVING OUTLIERS~n', []),
	    print_processed_amt_judgements_summary(ProcessedAMTData, S),
	    print_amt_judge_summary(ProcessedAMTData, S),

	    maybe_add_ngram_and_frequency_information(ProcessedAMTData, ProcessedAMTData1),
	    print_processed_amt_judgements1(S, ProcessedAMTData1)
	;
	    otherwise ->
	    true
	),
	
	close(S),
	
	format('~N--- Written summary to ~w~n', [SummaryFile]),

	print_prolog_summary_file_if_necessary(ProcessedAMTData1, PrologSummaryFile),
	!.
print_processed_amt_judgements(_ProcessedAMTData, SummaryFile) :-
	format('~N*** Error: bad call: ~w~n', [print_processed_amt_judgements('...', SummaryFile)]),
	fail.

%======================================================================

print_processed_amt_judgements_summary(ProcessedAMTData, S) :-
	majority_judgement_multiset(ProcessedAMTData, MajorityJudgementMultiset),
	agreement_multiset(ProcessedAMTData, AgreementMultiset),
	format(S, '~N~nConsolidated judgements:~n~n', []),
	print_multiset(MajorityJudgementMultiset, S),
	format(S, '~N~nInter-annotator agreement:~n~n', []),
	print_multiset(AgreementMultiset, S),
	format(S, '~N~n---------------------------------------------~n~n', []),
	!.

%======================================================================

print_processed_amt_judgements1(_S, []).
print_processed_amt_judgements1(S, [F | R]) :-
	print_processed_amt_judgement_record(S, F),
	format(S, '~N~n', []),
	!,
	print_processed_amt_judgements1(S, R).

print_processed_amt_judgement_record(_S, []).
print_processed_amt_judgement_record(S, [F | R]) :-
	print_processed_amt_judgement_record_component(S, F),
	!,
	print_processed_amt_judgement_record(S, R).

print_processed_amt_judgement_record_component(S, Key=Value) :-
	float(Value),
	format(S, '~N~w:~20|~2f~n', [Key, Value]),
	!.
print_processed_amt_judgement_record_component(S, Key=Value) :-
	format(S, '~N~w:~20|~w~n', [Key, Value]),
	!.
print_processed_amt_judgement_record_component(S, F) :-
	format('~N*** Error: bad call: ~w~n', [print_processed_amt_judgement_record_component(S, F)]),
	fail.

%======================================================================

print_prolog_summary_file_if_necessary(_ProcessedAMTData, File) :-
	File = '*no_file*',
	!.
print_prolog_summary_file_if_necessary(ProcessedAMTData, File) :-
	safe_absolute_file_name(File, AbsFile),
	list_to_prolog_file_with_encoding(ProcessedAMTData, AbsFile, 'UTF-8'),
	length(ProcessedAMTData, N),
	format('~N--- Written prolog summary file (~d records): ~w', [N, AbsFile]),
	!.
print_prolog_summary_file_if_necessary(_ProcessedAMTData, File) :-
	format('~N*** Error: bad call: ~w~n', [print_prolog_summary_file_if_necessary('...', File)]),
	fail.

%======================================================================


maybe_add_ngram_and_frequency_information(ProcessedAMTDataIn, ProcessedAMTDataOut) :-
	\+ ngram_examples_loaded,
	!,
	ProcessedAMTDataIn = ProcessedAMTDataOut.
maybe_add_ngram_and_frequency_information(ProcessedAMTDataIn, ProcessedAMTDataOut) :-
	add_ngram_and_frequency_information_to_records(ProcessedAMTDataIn, ProcessedAMTDataNext1),
	store_ngram_transformation_score_data(ProcessedAMTDataNext1),
	add_transformation_score_data_to_records(ProcessedAMTDataNext1, ProcessedAMTDataNext2),
	
	%sort_records_by_contrasting_ngram_frequency(ProcessedAMTDataNext, ProcessedAMTDataOut),
	sort_records_by_ngram_transformation_score(ProcessedAMTDataNext2, ProcessedAMTDataOut),
	!.
maybe_add_ngram_and_frequency_information(_ProcessedAMTDataIn, _ProcessedAMTDataOut) :-
	format('~N*** Error: bad call: ~w~n', [maybe_add_ngram_and_frequency_information('...', '...')]),
	fail.

add_ngram_and_frequency_information_to_records([], []).
add_ngram_and_frequency_information_to_records([F | R], [F1 | R1]) :-
	add_ngram_and_frequency_info_to_record(F, F1),
	!,
	add_ngram_and_frequency_information_to_records(R, R1).
add_ngram_and_frequency_information_to_records([_F | R], R1) :-
	!,
	add_ngram_and_frequency_information_to_records(R, R1).

add_ngram_and_frequency_info_to_record(Record, RecordOut) :-
	member(source=Source, Record),
	member(trans1=OriginalTarget, Record),
	member(trans2=VariantTarget, Record),
	ngram_and_frequencies_for_example(Source, Ngram, Freq1, Freq2),
	variant_translation_info_for_example(Source, OriginalTarget, VariantTarget, VariantSource, Tag),
	add_source2_to_record(Record, VariantSource, Record1),
	append([ngram=Ngram, tag=Tag, freq1=Freq1, freq2=Freq2], Record1, RecordOut),
	!.
add_ngram_and_frequency_info_to_record(Record, Record1) :-
	format('~N*** Warning: bad call: ~w~n', [add_ngram_and_frequency_info_to_record(Record, Record1)]),
	fail.

add_source2_to_record([], VariantSource, [source2=VariantSource]).
add_source2_to_record([source=Source | R], VariantSource, [source=Source, source2=VariantSource | R]) :-
	!.
add_source2_to_record([F | R], VariantSource, [F | R1]) :-
	!,
	add_source2_to_record(R, VariantSource, R1).

add_transformation_score_data_to_records([], []).
add_transformation_score_data_to_records([F | R], [F1 | R1]) :-
	add_transformation_score_data_to_record(F, F1),
	!,
	add_transformation_score_data_to_records(R, R1).

add_transformation_score_data_to_record(Record, Record1) :-
	member(ngram=Ngram, Record),
	average_ngram_transformation_score(Ngram, NgramTransformationScore),
	append([ngram_score=NgramTransformationScore], Record, Record1),
	!.
add_transformation_score_data_to_record(Record, Record1) :-
	format('~N*** Error: bad call: ~w~n', [add_transformation_score_data_to_record(Record, Record1)]),
	fail.

%======================================================================

sort_records_by_ngram_transformation_score(ProcessedAMTDataIn, ProcessedAMTDataOut) :-
	tag_records_by_ngram_transformation_score(ProcessedAMTDataIn, ProcessedAMTDataTagged),
	keysort(ProcessedAMTDataTagged, ProcessedAMTDataTaggedSorted),
	unkey_list(ProcessedAMTDataTaggedSorted, ProcessedAMTDataOut).

tag_records_by_ngram_transformation_score([], []).
tag_records_by_ngram_transformation_score([F | R], [F1 | R1]) :-
	tag_record_by_ngram_transformation_score(F, F1),
	!,
	tag_records_by_ngram_transformation_score(R, R1).
	
tag_record_by_ngram_transformation_score(Record, Key-Record) :-
	member(ngram_score=NgramTransformationScore, Record),
	member(freq1=Freq1, Record),
	Key is -1 * ( NgramTransformationScore + ( 0.001 * Freq1 ) ),
	!.
tag_record_by_ngram_transformation_score(Record, Record1) :-
	format('~N*** Error: bad call: ~w~n', [tag_record_by_ngram_transformation_score(Record, Record1)]),
	fail.
	
%======================================================================

:- dynamic score_for_ngram_transformation/2.

average_ngram_transformation_score(Ngram, NgramTransformationScore) :-
	findall(Score,
		score_for_ngram_transformation(Ngram, Score),
		Scores),
	(   Scores = [] ->
	    NgramTransformationScore = 0.0
	;
	    otherwise ->
	    safe_sum_list(Scores, TotalScore),
	    length(Scores, N),
	    NgramTransformationScore is TotalScore / N
	).

store_ngram_transformation_score_data(ProcessedAMTData) :-
	retractall(score_for_ngram_transformation(_, _)),
	store_ngram_transformation_score_data1(ProcessedAMTData),
	!.
store_ngram_transformation_score_data(_ProcessedAMTData) :-
	format('~N*** Error: bad call: ~w~n', [store_ngram_transformation_score_data('...')]),
	fail.

store_ngram_transformation_score_data1([]).
store_ngram_transformation_score_data1([F | R]) :-
	store_ngram_transformation_score_data_for_record(F),
	!,
	store_ngram_transformation_score_data1(R).

/*
ngram:              [ai,plus]
freq1:              121.8
freq2:              1.23
source:             depuis je n'ai plus mon logiciel norton alors que j'avais renouvellé mon abonnemet
trans1:             Since I no longer have my software norton while I had renouvellé abonnemet my
trans2:             Since I do not have my software norton while I had renouvellé abonnemet my
judgements:         [[FirstClearlyBetter,A2B6J1Z3IGHS18],[FirstClearlyBetter,AQVR0IQEYNVVT]]
majority_judgement: FirstBetter
agreement:          Unanimous
*/

store_ngram_transformation_score_data_for_record(Record) :-
	member(ngram=Ngram, Record),
	member(majority_judgement=Judgement, Record),
	member(agreement=Agreement, Record),
	transformation_score_for_example(Judgement, Agreement, Score),
	assertz(score_for_ngram_transformation(Ngram, Score)),
	!.
store_ngram_transformation_score_data_for_record(Record) :-
	format('~N*** Error: bad call: ~w~n', [store_ngram_transformation_score_data_for_record(Record)]),
	fail.

transformation_score_for_example(Judgement, Agreement, Score) :-
	transformation_score_for_judgement(Judgement, JudgementScore),
	transformation_score_for_agreement(Agreement, AgreementScore),
	(   JudgementScore > 0 ->
	    Score is JudgementScore * ( 3 + AgreementScore )
	;
	    otherwise ->
	    Score is ( 3 * JudgementScore )
	).

transformation_score_for_judgement('SecondBetter', 1.0) :-
	!.
transformation_score_for_judgement('FirstBetter', -1.0) :-
	!.
transformation_score_for_judgement(_Judgement, 0.0).

transformation_score_for_agreement('Unanimous', 2.0) :-
	!.
transformation_score_for_agreement('Agree', 1.0) :-
	!.
transformation_score_for_agreement(_Agreement, 0.0).

%======================================================================

sort_records_by_contrasting_ngram_frequency(ProcessedAMTDataIn, ProcessedAMTDataOut) :-
	tag_records_by_ngram_freq_difference(ProcessedAMTDataIn, ProcessedAMTDataTagged),
	keysort(ProcessedAMTDataTagged, ProcessedAMTDataTaggedSorted),
	unkey_list(ProcessedAMTDataTaggedSorted, ProcessedAMTDataOut).

tag_records_by_ngram_freq_difference([], []).
tag_records_by_ngram_freq_difference([F | R], [F1 | R1]) :-
	tag_record_by_ngram_freq_difference(F, F1),
	!,
	tag_records_by_ngram_freq_difference(R, R1).
	
tag_record_by_ngram_freq_difference(Record, Key-Record) :-
	member(freq1=Freq1, Record),
	member(freq2=Freq2, Record),
	smooth_zero_score(Freq2, SmoothedFreq2),
	Key is -1 * ( Freq1 / SmoothedFreq2 ),
	!.
tag_record_by_ngram_freq_difference(Record, Record1) :-
	format('~N*** Error: bad call: ~w~n', [tag_record_by_ngram_freq_difference(Record, Record1)]),
	fail.
	
smooth_zero_score(Count, SmoothedCount) :-
	(   Count < 0.01 ->
	    SmoothedCount = 0.01
	;
	    otherwise ->
	    SmoothedCount = Count
	).	

%======================================================================

majority_judgement_multiset(ProcessedAMTData, MajorityJudgementMultiset) :-
	findall(MajorityJudgement,
		(   member(Record, ProcessedAMTData),
		    member(majority_judgement=MajorityJudgement, Record)
		),
		MajorityJudgements),
	list_to_ordered_multiset(MajorityJudgements, MajorityJudgementMultiset).

agreement_multiset(ProcessedAMTData, AgreementMultiset) :-
	findall(Agreement,
		(   member(Record, ProcessedAMTData),
		    member(agreement=Agreement, Record)
		),
		Agreements),
	list_to_ordered_multiset(Agreements, AgreementMultiset).

print_multiset([], _S).
print_multiset([F | R], S) :-
	print_multiset_line(F, S),
	!,
	print_multiset(R, S).

print_multiset_line(Freq-Item, S) :-
	format(S, '~N~d~5|~w~n', [Freq, Item]),
	!.
print_multiset_line(F, S) :-
	format('~N*** Error: bad call: ~w~n', [print_multiset_line(F, S)]),
	fail.

%======================================================================

:- dynamic suspect_judge/1.

%threshold_for_suspect_judge(25.0).
threshold_for_suspect_judge(100.0).

mark_suspect_judges(ProcessedAMTData) :-
	retractall(suspect_judge(_)),
	all_judges(ProcessedAMTData, Judges),
	mark_suspect_judges1(Judges, ProcessedAMTData, 0-N),
	format('~N--- Marked ~d judges as suspect~n', [N]).

mark_suspect_judges1([], _ProcessedAMTData, N-N).
mark_suspect_judges1([F | R], ProcessedAMTData, In-Out) :-
	check_for_suspect_judge(F, ProcessedAMTData, In-Next),
	!,
	mark_suspect_judges1(R, ProcessedAMTData, Next-Out).

check_for_suspect_judge(Judge, ProcessedAMTData, In-Out) :-
	threshold_for_suspect_judge(Threshold),
	judgements_for_judge(Judge, ProcessedAMTData, Judgements),
	length(Judgements, NJudgements),
	count_minority_judgements_for_judge(Judge, ProcessedAMTData, NMinorityJudgements),
	PCMinorityJudgements is ( 100.0 * NMinorityJudgements ) / NJudgements,
	(   PCMinorityJudgements > Threshold ->
	    assertz(suspect_judge(Judge)),
	    format('~N--- Marked ~w as suspect (~d judgements, ~1f% minority judgements)~n',
		   [Judge, NJudgements, PCMinorityJudgements]),
	    Out is In + 1
	;
	    otherwise ->
	    Out = In
	),
	!.
check_for_suspect_judge(Judge, _ProcessedAMTData, InOut) :-
	format('~N*** Error: bad call: ~w~n', [check_for_suspect_judge(Judge, '...', InOut)]),
	fail.

%======================================================================

print_amt_judge_summary(ProcessedAMTData, S) :-
	all_judges(ProcessedAMTData, Judges),
	length(Judges, NJudges),
	format(S, '~NINFORMATION FOR ~d JUDGES:~n~n', [NJudges]),
	print_judge_list_info(Judges, ProcessedAMTData, S),
	format(S, '~N---------------------------------------------~n~n', []),
	!.

print_judge_list_info([], _ProcessedAMTData, _S).
print_judge_list_info([Judge | Judges], ProcessedAMTData, S) :-
	print_judge_info(Judge, ProcessedAMTData, S),
	format(S, '~N~n', []),
	!,
	print_judge_list_info(Judges, ProcessedAMTData, S).

print_judge_info(Judge, ProcessedAMTData, S) :-
	(   suspect_judge(Judge) ->
	    Suspect = ' (suspect - not counted)'
	;
	    otherwise ->
	    Suspect = ''
	),
	format(S, '~NInformation for judge ~w~w:~n', [Judge, Suspect]),
	judgements_for_judge(Judge, ProcessedAMTData, Judgements),
	length(Judgements, NJudgements),
	format(S, '~N~d~5|~w~n~n', [NJudgements, 'judgements']),
	list_to_ordered_multiset(Judgements, JudgementsMultiset),
	print_multiset(JudgementsMultiset, S),
	count_minority_judgements_for_judge(Judge, ProcessedAMTData, NMinorityJudgements),
	PCMinorityJudgements is ( 100.0 * NMinorityJudgements ) / NJudgements,
	format(S, '~N~n~d~5|~w (~1f%)~n', [NMinorityJudgements, 'MinorityJudgements', PCMinorityJudgements]),
	!.

all_judges(ProcessedAMTData, Judges) :-
	findall(Judge,
		(   member(Record, ProcessedAMTData),
		    member(judgements=JudgementPairs, Record),
		    member([_Judgement, Judge], JudgementPairs)
		),
		Judges0),
	sort(Judges0, Judges).

judgements_for_judge(Judge, ProcessedAMTData, Judgements) :-
	findall(Judgement,
		(   member(Record, ProcessedAMTData),
		    member(judgements=JudgementPairs, Record),
		    member([Judgement, Judge], JudgementPairs)
		),
		Judgements).

count_minority_judgements_for_judge(Judge, ProcessedAMTData, NMinorityJudgements) :-
	findall(x,
		(   member(Record, ProcessedAMTData),
		    member(majority_judgement=MajorityJudgement, Record),
		    MajorityJudgement \== 'Unclear',
		    member(judgements=JudgementPairs, Record),
		    member([Judgement, Judge], JudgementPairs),
		    \+ judgement_agree_with_majority_judgement(Judgement, MajorityJudgement)
		),
		Xs),
	length(Xs, NMinorityJudgements).

judgement_agree_with_majority_judgement('FirstClearlyBetter', 'FirstBetter').
judgement_agree_with_majority_judgement('FirstSlightlyBetter', 'FirstBetter').
judgement_agree_with_majority_judgement('SecondClearlyBetter', 'SecondBetter').
judgement_agree_with_majority_judgement('SecondSlightlyBetter', 'SecondBetter').	

%======================================================================

csv_file_columns_to_files_randomising_order(FourColumnCSV, Separator, List) :-
	safe_absolute_file_name(FourColumnCSV, AbsFourColumnCSV),
	csv_file_to_list_of_lists(AbsFourColumnCSV, 0'", Separator, CSVList), %"'
	random_permutation(CSVList, RandomisedCSVList),
	csv_file_columns_to_files1(List, RandomisedCSVList).

csv_file_columns_to_files(FourColumnCSV, Separator, List) :-
	safe_absolute_file_name(FourColumnCSV, AbsFourColumnCSV),
	csv_file_to_list_of_lists(AbsFourColumnCSV, 0'", Separator, CSVList), %"'
	csv_file_columns_to_files1(List, CSVList).

csv_file_columns_to_files1([], _List).
csv_file_columns_to_files1([N-File | R], List) :-
	csv_file_column_to_file(N, List, File),
	!,
	csv_file_columns_to_files1(R, List).

csv_file_column_to_file(N, List, File) :-
	safe_absolute_file_name(File, AbsFile),
	csv_file_column(List, N, Column),
	write_atom_list_to_unicode_file(Column, AbsFile, 'UTF-8').

csv_file_column([], _N, []).
csv_file_column([F | R], N, [F1 | R1]) :-
	nth1(N, F, F1),
	!,
	csv_file_column(R, N, R1).

%======================================================================

all_numbers_are_same(List) :-
	sort(List, SortedList),
	length(SortedList, N),
	N = 1,
	!.

identical_translations(T1, T2) :-
	split_atom_into_words(T1, T1Words),
	split_atom_into_words(T2, T2Words),
	T1Words = T2Words,
	!.

make_equal(N, M) :-
	N = M.

%======================================================================

list_to_html_file(Lines, File) :-
	safe_absolute_file_name(File, AbsFile),
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	print_html_table_opening(S),
	print_html_table1(Lines, S),
	print_html_table_closing(S),
	close(S),
	length(Lines, N),
	format('~N--- Written HTML reviewing table (~d lines) ~w~n', [N, AbsFile]),
	!.

print_html_table_opening(S) :-
	format(S, '~N<html>~n', []),
	format(S, '~N<body>~n', []),
	format(S, '~N<table  border="1">~n', []),
	!.

print_html_table_closing(S) :-
	format(S, '~N</table>~n', []),
	format(S, '~N</body>~n', []),
	format(S, '~N</html>~n', []),
	!.

print_html_table1([], _S).
print_html_table1([F | R], S) :-
	print_html_line(F, S),
	!,
	print_html_table1(R, S).

print_html_line(Line, S) :-
	print_html_line_opening(S),
	print_html_line1(Line, S),
	print_html_line_closing(S),
	!.
print_html_line(Line, S) :-
	format('~N*** Error: bad call: ~w~n', [print_html_line(Line, S)]),
	fail.

print_html_line_opening(S) :-
	format(S, '~N<tr>~n', []),
	!.

print_html_line_closing(S) :-
	format(S, '~N</tr>~n', []),
	!.

print_html_line1([], _S).
print_html_line1([F | R], S) :-
	print_html_line_element(F, S),
	!,
	print_html_line1(R, S).

print_html_line_element(Elt, S) :-
	format(S, '~N<td>', []),
	print_html_line_element1(Elt, S),
	format(S, '</td>~n', []),
	!.

print_html_line_element1([], _S) :-
	!.
print_html_line_element1(X, S) :-
	format(S, '~w', [X]),
	!.