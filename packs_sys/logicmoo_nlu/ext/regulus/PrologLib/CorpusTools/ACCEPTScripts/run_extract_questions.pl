
:- compile('$REGULUS/PrologLib/CorpusTools/extract_bicorpus').

go :-
	extract_bicorpus('$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.fr',
			 '$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en.en',
			 %'$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.fr',
			 %'$ACCEPT/MT/Europarl/Data/europarl-v6.fr-en-small.en',
			 and("?",
			     or("-vous", "-nous", "-il", "-elle")),
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_fr.txt',
			 '$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_en.txt').

:- go.

:- halt.
