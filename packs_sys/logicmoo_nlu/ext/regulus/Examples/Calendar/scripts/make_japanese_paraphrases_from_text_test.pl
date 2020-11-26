
:- use_module('$REGULUS/Prolog/extract_paraphrases').

go :-
	extract_paraphrases('$REGULUS/Examples/Calendar/Generated/japanese_calendar_dev_corpus_results.pl',
			    '$REGULUS/Examples/Calendar/Generated/japanese_paraphrases.pl').

:- go.

:- halt.

	