
:- compile('$REGULUS/Prolog/extract_corpora.pl').

:- sents_file_or_files_to_slm_training_file(['$REGULUS/Examples/Calendar/corpora/japanese_calendar_dev_corpus.pl'],
					    '$REGULUS/Examples/Calendar/SLM/japanese_slm_sents.txt').

:- halt.
