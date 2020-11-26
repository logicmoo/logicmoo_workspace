:- use_module('$REGULUS/Prolog/dialogue').

go :- batch_dialogue_file_to_paraphrase_csv_file('$REGULUS/Examples/Calendar/Generated/calendar_dev_corpus_results.pl',
						 '$REGULUS/Examples/Calendar/Generated/calendar_paraphrase_summary.csv').

:- go.

:- halt.

						 