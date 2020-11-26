
default:
	${MAKE} slm_corpus
	${MAKE} classes
	${MAKE} tagging_package
	${MAKE} tagged_corpus
	${MAKE} slm

japanese:
	${MAKE} japanese_slm_corpus
	${MAKE} japanese_classes
	${MAKE} japanese_tagging_package
	${MAKE} japanese_tagged_corpus
	${MAKE} japanese_slm

slm_corpus:
	sicstus -l make_slm_corpus.pl

japanese_slm_corpus:
	sicstus -l make_japanese_slm_corpus.pl

classes:
	sicstus -l scripts/make_tagging_grammars.pl

japanese_classes:
	sicstus -l scripts/make_japanese_tagging_grammars.pl

tagging_package:
	nl-compile generated_tagging.grammar English.UK -auto_pron

japanese_tagging_package:
	nl-compile japanese_generated_tagging.grammar Japanese -auto_pron

tagged_corpus:
	nl-tag-tool -package generated_tagging -grammar .MAIN_tagging -no_output < slm_sents.txt > tagged_sents.txt 

japanese_tagged_corpus:
	nl-tag-tool -package japanese_generated_tagging -grammar .MAIN_tagging -no_output < japanese_slm_sents.txt > japanese_tagged_sents.txt 

slm:
	train-slm -corpus tagged_sents.txt -slm slm

japanese_slm:
	train-slm -corpus japanese_tagged_sents.txt -slm japanese_slm

recogniser:
	echo "To build recogniser compile the whole package in ../scripts"
