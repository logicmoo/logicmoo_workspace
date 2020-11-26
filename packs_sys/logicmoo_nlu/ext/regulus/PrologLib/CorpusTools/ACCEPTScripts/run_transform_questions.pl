
:- compile('$REGULUS/PrologLib/CorpusTools/transform_text').

go :-
	TransformId = 'inverted_to_est_ce_que',

	init_transform_text,
	transform_text_in_file('$ACCEPT/MT/Europarl/Generated/europarl_questions_filtered_fr.txt',
			       TransformId,
			       '$ACCEPT/MT/Europarl/Generated/europarl_questions_transformed_fr.txt').

:- go.

:- halt.
