

:- compile('$REGULUS/PrologLib/CorpusTools/transform_text').

go :-
	TransformId = 'vous_te',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_vous_filtered_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_vous_transformed_fr.txt').

:- go.

:- halt.
