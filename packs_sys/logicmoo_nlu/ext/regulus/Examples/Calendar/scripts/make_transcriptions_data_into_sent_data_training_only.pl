
:- ['$REGULUS/Examples/Calendar/Prolog/make_transcriptions_data_into_sents_data'].

:- transcriptions2sents('$REGULUS/Examples/Calendar/corpora/calendar_transcriptions_training.txt',
			'$REGULUS/Examples/Calendar/corpora/dev_corpus_from_transcriptions.pl').

:- halt.
