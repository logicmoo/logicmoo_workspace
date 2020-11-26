
:- compile('$REGULUS/Prolog/extract_corpora.pl').

:- sents_file_or_files_to_slm_training_file(['$REGULUS/Examples/Calendar/corpora/calendar_dev_corpus.pl',
					     '$REGULUS/Examples/Calendar/corpora/dev_corpus_from_transcriptions.pl'],
					    '$REGULUS/Examples/Calendar/SLM/slm_sents.txt').

:- halt.
