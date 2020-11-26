
% Declare library directories
:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Prolog')).
:- asserta(library_directory('$REGULUS/Alterf/Prolog')).
:- asserta(library_directory('.')).

:- asserta(file_search_path(alterf_generated_files,'$REGULUS/Examples/Toy1Alterf/Generated')).

load :-
	compile(library(classifier_trainer)),

	% Compile application-specific Alterf files
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/target_model'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/surface_patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/tagging_grammar').

train :-
	train_classifier('$REGULUS/Examples/Toy1Alterf/corpora/alterf_training.txt', 
			 [1-[package='$REGULUS/Examples/Toy1Alterf/Generated/toy1_specialised_recogniser',
			     grammar='.MAIN', 'rec.Pruning'=1200]], 
			 [1-[hand_coded_patterns, 
			     hand_coded_surface_patterns,
			     class_bigrams,
			     class_trigrams]],
			 [discriminant_score_threshold=2.0,
			  mle_formula=carter],
			 '$REGULUS/Examples/Toy1Alterf/Generated/discriminants.pl',
				%rec([1-'batchrec_results_train1.pl'])
			 text
			).

:- load.

:- train.

:- halt.

