
:- compile('$REGULUS/PrologLib/CorpusTools/make_verb_transforms').

go :-
	make_verb_transforms('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
			     '$ACCEPT/MT/Europarl/Mmorph/verb_transforms.pl'),
	make_verb_list('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
		       '$ACCEPT/MT/Europarl/Mmorph/verbs_with_mood.pl'),
	make_verb_taking_subjunctive_list('$ACCEPT/MT/Europarl/Mmorph/verbs.pl',
					  '$ACCEPT/MT/Europarl/Mmorph/verbs_taking_subjunctive.pl').
	
:- go.

:- halt.
