
default:
	${MAKE} nuance_grammar
	#${MAKE} paraphrase_grammar
	#${MAKE} slm
	${MAKE} recogniser

nuance_grammar:
	sicstus -l build_nuance_files.pl

paraphrase_grammar:
	sicstus -l build_paraphrase_grammar.pl

slm:
	cd ../SLM ; ${MAKE}

recogniser:
	sicstus -l build_recogniser.pl

text_test:
	sicstus -l run_text_test.pl

speech_test:
	(unset SHELL ; sicstus -l run_speech_test.pl)

speech_test_again:
	sicstus -l run_speech_test_again.pl

update_judgements:
	sicstus -l run_update_judgements.pl

update_speech_judgements:
	sicstus -l run_update_speech_judgements.pl

clean:
	rm ../Generated/*
