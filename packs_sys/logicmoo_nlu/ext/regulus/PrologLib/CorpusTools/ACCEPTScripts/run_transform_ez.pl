

:- compile('$REGULUS/PrologLib/CorpusTools/transform_text').

go :-
	TransformId = '2pl_to_2sing',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_ez_filtered_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_ez_transformed_fr.txt').

:- go.

:- halt.
