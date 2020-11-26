
:- compile('$REGULUS/PrologLib/CorpusTools/orthography_process_text').

go :-
	orthography_process_file('$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_fr.txt',
				 '$REGULUS/PrologLib/CorpusTools/french_orthography.pl',
				 '$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_orthography_fr.txt').

:- go.

:- halt.
