
:- compile('$REGULUS/PrologLib/CorpusTools/parse_mmorph').
:- compile('$REGULUS/PrologLib/CorpusTools/make_noun_transforms').

go :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/nouns.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/raw_nouns.pl'),

	make_noun_list('$ACCEPT/MT/Europarl/Mmorph/raw_nouns.pl',
		       '$ACCEPT/MT/Europarl/Mmorph/nouns.pl').
	
:- go.

:- halt.
