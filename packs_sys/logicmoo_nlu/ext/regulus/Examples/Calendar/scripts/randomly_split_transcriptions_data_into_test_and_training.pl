
:- ['$REGULUS/Examples/Calendar/Prolog/randomly_split_transcriptions_data'].

:- randomly_split_transcriptions('$REGULUS/Examples/Calendar/corpora/calendar_transcriptions.txt',
				 '$REGULUS/Examples/Calendar/corpora/calendar_transcriptions_training.txt',
				 '$REGULUS/Examples/Calendar/corpora/calendar_transcriptions_test.txt',
				 0.5).

:- halt.
