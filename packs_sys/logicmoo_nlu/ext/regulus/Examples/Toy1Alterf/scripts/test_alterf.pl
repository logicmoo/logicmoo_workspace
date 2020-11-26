
% Declare library directories
:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Prolog')).
:- asserta(library_directory('$REGULUS/Alterf/Prolog')).
:- asserta(library_directory('.')).

:- asserta(file_search_path(alterf_generated_files,'$REGULUS/Examples/Toy1Alterf/Generated')).

load :-
	compile(library(batch_decoder)),
	compile('$REGULUS/PrologLib/utilities'),
	use_module(library(system)),

	% Compile application-specific Alterf files
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/target_model'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/surface_patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/tagging_grammar'),

	Discriminants = '$REGULUS/Examples/Toy1Alterf/Generated/discriminants.pl',
	(   file_exists(Discriminants) ->
	    compile(Discriminants) ;
	    format('~N*** Error: can\'t find discriminants file ~w~n', [Discriminants]),
	    halt
	).

test :-
	batch_decode('$REGULUS/Examples/Toy1Alterf/corpora/alterf_training.txt', 
		     [1-[package='$REGULUS/Examples/Toy1Alterf/Generated/toy1_specialised_recogniser', grammar='.MAIN',
			 'rec.pass1.gp.WTW'=0, 'rec.GrammarWeight'=7.85, 'rec.Pruning'=2000,
			 '-print_confidence_scores', '-print_word_confidence_scores', 'rec.GenSlotConfidence'='TRUE']],		
		     [1-[hand_coded_patterns, 
			 hand_coded_surface_patterns, class_bigrams, class_trigrams]],
		     '$REGULUS/Examples/Toy1Alterf/Generated/test_decode_results.pl',
		     %rec([1-'$REGULUS/Examples/Toy1Alterf/Generated/stored_batchrec_results_test.pl']),
		     text,
		     45,
		     _Results
		    ).
:- load.

:- test.

:- halt.

