
% Declare library directories
:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Prolog')).
:- asserta(library_directory('$REGULUS/Alterf/Prolog')).
:- asserta(library_directory('.')).

:- asserta(file_search_path(alterf_generated_files,'$REGULUS/Examples/Toy1Alterf/Generated')).

load :-
	compile(library(alterf_patterns)),

	% Compile application-specific Alterf files
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/target_model'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/surface_patterns'),
	compile('$REGULUS/Examples/Toy1Alterf/Prolog/Alterf/tagging_grammar').


test :- check_alterf_surface_patterns('$REGULUS/Examples/Toy1Alterf/corpora/alterf_training.txt', 
				      [package='$REGULUS/Examples/Toy1Alterf/Generated/toy1_specialised_recogniser',
				       grammar='.MAIN', 'rec.Pruning'=1200],
				      '$REGULUS/Examples/Toy1Alterf/Generated/test_surface_pattern_results.pl'
				      ).

:- load.

:- test.

:- halt.


