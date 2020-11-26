
:- compile('$REGULUS/PrologLib/CorpusTools/extract_bicorpus').

go :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_orthography_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_en.txt',
			 not("*** NO TRANSFORM - DISCARD ***"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions.fr',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions.en').

:- go.

:- halt.
