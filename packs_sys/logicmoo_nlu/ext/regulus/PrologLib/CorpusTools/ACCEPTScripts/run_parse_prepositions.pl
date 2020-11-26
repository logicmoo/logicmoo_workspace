
:- compile('$REGULUS/PrologLib/CorpusTools/parse_mmorph').

go :-
	parse_mmorph('$ACCEPT/MT/Europarl/Mmorph/prepositions.txt',
		     '$ACCEPT/MT/Europarl/Mmorph/raw_prepositions.pl').
	
:- go.

:- halt.
