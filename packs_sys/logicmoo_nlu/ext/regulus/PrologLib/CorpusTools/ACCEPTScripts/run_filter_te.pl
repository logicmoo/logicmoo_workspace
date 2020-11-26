
:- compile('$REGULUS/PrologLib/CorpusTools/extract_bicorpus').

go :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Generated/europarl_vous_transformed_orthography_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_vous_filtered_en.txt',
			 not("*** NO TRANSFORM - DISCARD ***"),
			 '$ACCEPT/MT/Europarl/Generated/europarl_te.fr',
			 '$ACCEPT/MT/Europarl/Generated/europarl_te.en').

:- go.

:- halt.
