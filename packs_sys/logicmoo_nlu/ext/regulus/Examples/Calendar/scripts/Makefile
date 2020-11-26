
default:
	${MAKE} name_lexicon
	${MAKE} nuance_grammar
	${MAKE} paraphrase_grammar
	${MAKE} slm
	${MAKE} recogniser
	# Need to run text_test to create the file used to build the help resources
	${MAKE} text_test
	${MAKE} help_resources
	${MAKE} paraphrase_csv_file

japanese:
	${MAKE} japanese_corpora
	${MAKE} japanese_name_lexicon
	${MAKE} japanese_wavfile_script
	${MAKE} japanese_nuance_grammar
	${MAKE} japanese_paraphrase_grammar
	${MAKE} japanese_slm
	${MAKE} japanese_recogniser
	# Need to run text_test to create the file used to build the help resources
	${MAKE} japanese_text_test
	${MAKE} japanese_help_resources

split_and_test:
	echo "============================================================="
	echo "STARTING NEW SPLIT AND TEST RUN"
	${MAKE} randomly_split_transcriptions
	${MAKE} transcriptions_data_to_sent_data_training_only
	${MAKE} name_lexicon
	${MAKE} nuance_grammar
	${MAKE} slm
	${MAKE} recogniser
	echo "RUNNING TEST"
	${MAKE} speech_test_test_only

japanese_corpora:
	sicstus -l make_japanese_corpora.pl

name_lexicon:
	sicstus -l build_name_lexicon.pl

japanese_name_lexicon:
	sicstus -l build_japanese_name_lexicon.pl

japanese_wavfile_script:
	sicstus -l make_japanese_wavfile_script.pl

nuance_grammar:
	sicstus -l build_nuance_files.pl

japanese_nuance_grammar:
	sicstus -l build_japanese_nuance_files.pl

japanese_nuance_grammar_no_sortal_feats:
	sicstus -l build_japanese_nuance_files_no_sortal_feats.pl

japanese_nuance_grammar_unspecialised:
	sicstus -l build_japanese_nuance_files_unspecialised.pl

paraphrase_grammar:
	sicstus -l build_paraphrase_grammar.pl

japanese_paraphrase_grammar:
	sicstus -l build_japanese_paraphrase_grammar.pl

paraphrase_recogniser:
	sicstus -l build_paraphrase_recogniser.pl

slm:
	cd ../SLM ; ${MAKE}

japanese_slm:
	cd ../SLM ; ${MAKE} japanese

recogniser:
	cp ../Generated/grammar_probs_data_file.txt .
	compute-grammar-probs -base_grammar_file ../Generated/recogniser -data grammar_probs_data_file.txt -output_dir ../Generated/pcfg_trained
	#nuance-compile ../Generated/pcfg_trained/recogniser English.UK -auto_pron -dont_flatten -o ../Generated/recogniser
	nuance-compile combined_glm_slm English.UK -auto_pron -dont_flatten -o ../Generated/recogniser

japanese_recogniser:
	cp ../Generated/japanese_grammar_probs_data_file.txt .
	compute-grammar-probs -base_grammar_file ../Generated/japanese_recogniser -data japanese_grammar_probs_data_file.txt -output_dir ../Generated/japanese_pcfg_trained
	#nuance-compile ../Generated/pcfg_trained/recogniser English.UK -auto_pron -dont_flatten -o ../Generated/recogniser
	nuance-compile japanese_combined_glm_slm Japanese -auto_pron -dont_flatten -o ../Generated/japanese_recogniser

japanese_recogniser_no_sortal_feats:
	cp ../Generated/japanese_grammar_probs_data_file.txt .
	compute-grammar-probs -base_grammar_file ../Generated/japanese_recogniser_no_sortal_feats -data japanese_grammar_probs_data_file.txt -output_dir ../Generated/japanese_pcfg_trained
	#nuance-compile ../Generated/pcfg_trained/recogniser English.UK -auto_pron -dont_flatten -o ../Generated/recogniser
	nuance-compile japanese_combined_glm_slm_no_sortal_feats Japanese -auto_pron -dont_flatten -o ../Generated/japanese_recogniser_no_sortal_feats

japanese_recogniser_unspecialised:
	#cp ../Generated/japanese_grammar_probs_data_file.txt .
	#compute-grammar-probs -base_grammar_file ../Generated/japanese_unspecialised_recogniser -data japanese_grammar_probs_data_file.txt -output_dir ../Generated/japanese_pcfg_trained
	#nuance-compile ../Generated/pcfg_trained/recogniser English.UK -auto_pron -dont_flatten -o ../Generated/recogniser
	nuance-compile ../Generated/japanese_unspecialised_recogniser Japanese -auto_pron -dont_flatten -o ../Generated/japanese_recogniser_unspecialised

rough_japanese_transcriptions:
	sicstus -l make_rough_japanese_transcriptions.pl
	cd ../corpora/JapDataCollSep2008 ; cat mayuko.txt ozawa.txt yuriko.txt kikuko.txt miki.txt > all_transcriptions.txt

update_transcriptions:
	cd ../corpora/speech ; ls -1 */*.wav > ../tmp_wavfiles_listing.txt
	sicstus -l update_transcriptions.pl

randomly_split_transcriptions:
	sicstus -l randomly_split_transcriptions_data_into_test_and_training.pl

transcriptions_data_to_sent_data:
	sicstus -l make_transcriptions_data_into_sent_data.pl

transcriptions_data_to_sent_data_training_only:
	sicstus -l make_transcriptions_data_into_sent_data_training_only.pl

text_test:
	sicstus -l run_text_test.pl

japanese_text_test:
	sicstus -l run_japanese_text_test.pl

japanese_text_test_sep_2008:
	sicstus -l run_japanese_text_test_sep_2008.pl

japanese_text_test_sep_2008_no_paraphrases:
	sicstus -l run_japanese_text_test_sep_2008_no_paraphrases.pl

japanese_text_test_sep_2008_no_datastructures:
	sicstus -l run_japanese_text_test_sep_2008_no_datastructures.pl

japanese_paraphrases_from_text_test:
	sicstus -l make_japanese_paraphrases_from_text_test.pl

speech_test:
	(unset SHELL ; sicstus -l run_speech_test.pl)

speech_test_full:
	(unset SHELL ; sicstus -l run_speech_test_full.pl)

speech_test_full_again:
	(unset SHELL ; sicstus -l run_speech_test_full_again.pl)

speech_test_slm:
	(unset SHELL ; sicstus -l run_speech_test_slm.pl)

japanese_speech_test:
	(unset SHELL ; sicstus -l run_japanese_speech_test.pl)
	grep '^%' ../Generated/japanese_nbest_training_data.pl > ../Generated/japanese_nbest_summary.txt

japanese_speech_test_no_sortal_feats:
	(unset SHELL ; sicstus -l run_japanese_speech_test_no_sortal_feats.pl)
	grep '^%' ../Generated/japanese_nbest_training_data_no_sortal_feats.pl > ../Generated/japanese_nbest_summary_no_sortal_feats.txt

japanese_speech_test_slm:
	(unset SHELL ; sicstus -l run_japanese_speech_test_slm.pl)
	#grep '^%' ../Generated/japanese_nbest_training_data_slm.pl > ../Generated/japanese_nbest_summary_slm.txt

japanese_speech_test_unspecialised:
	(unset SHELL ; sicstus -l run_japanese_speech_test_unspecialised.pl)
	%grep '^%' ../Generated/japanese_nbest_training_data_no_sortal_feats.pl > ../Generated/japanese_nbest_summary_no_sortal_feats.txt

speech_test_again:
	sicstus -l run_speech_test_again.pl
	grep '^%' ../Generated/nbest_training_data.pl > ../Generated/nbest_summary.txt

japanese_speech_test_again:
	sicstus -l run_japanese_speech_test_again.pl
	grep '^%' ../Generated/japanese_nbest_training_data.pl > ../Generated/japanese_nbest_summary.txt

speech_test_again_1_best:
	sicstus -l run_speech_test_again_1_best.pl
	grep '^%' ../Generated/nbest_training_data.pl > ../Generated/1_best_summary.txt

speech_test_pda:
	(unset SHELL ; sicstus -l run_speech_test_pda.pl)

speech_test_pda_1_best:
	(unset SHELL ; sicstus -l run_speech_test_pda_1_best.pl)

speech_test_test_only:
	(unset SHELL ; sicstus -l run_speech_test_test_only.pl)

speech_test_test_only_again:
	sicstus -l run_speech_test_test_only_again.pl

update_judgements:
	sicstus -l run_update_judgements.pl

update_judgements_jap_sep2008_no_datastructures:
	sicstus -l run_update_judgements_jap_sep2008_no_datastructures.pl

update_speech_judgements:
	sicstus -l run_update_speech_judgements.pl

update_speech_judgements_test_only:
	sicstus -l run_update_speech_judgements_test_only.pl

help_resources:
	sicstus -l run_make_help_resources.pl

japanese_help_resources:
	sicstus -l run_make_japanese_help_resources.pl

paraphrase_csv_file:
	sicstus -l run_make_paraphrase_csv_file.pl

random_generate:
	generate -package ../Generated/recogniser -grammar .MAIN -num 10000 > ../Generated/random_corpus.txt

clean:
	rm ../Generated/*
