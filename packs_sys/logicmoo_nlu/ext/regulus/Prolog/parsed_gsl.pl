
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(parsed_gsl,
	  [test_parsed_gsl/1,
	   dynamic_gsl_to_static/3,
	   merge_dynamic_and_weighted_static_gsl/5
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/parse_regulus_generated_gsl').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%------------------------------------------------------------------------------------

test_parsed_gsl(jackie_dynamic_to_static) :-
	dynamic_gsl_to_static('$JACKIE/Generated/jackie_specialised_recogniser_dynamic.grammar',
			      '$JACKIE/Generated',
			      '$JACKIE/Generated/jackie_specialised_recogniser_static.grammar').

test_parsed_gsl(merged_jackie) :-	
	merge_dynamic_and_weighted_static_gsl('$JACKIE/Generated/jackie_specialised_recogniser_dynamic.grammar',
					      '$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_static.grammar',
					      '$JACKIE/Generated',
					      constant_weight(0.1),
					      '$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_dynamic.grammar').

%------------------------------------------------------------------------------------

dynamic_gsl_to_static(DynamicGSLFile, WorkingDir, StaticGSLFile) :-
	safe_absolute_file_name(WorkingDir, AbsWorkingDir),
	format_to_atom('~w/tmp_dynamic_gsl.pl', [AbsWorkingDir], DynamicGSLPrologFile),
	format_to_atom('~w/tmp_static_gsl.pl', [AbsWorkingDir], StaticGSLPrologFile),
	parse_regulus_generated_gsl(DynamicGSLFile, DynamicGSLPrologFile),
	dynamic_gsl_prolog_to_static(DynamicGSLPrologFile, StaticGSLPrologFile),
	convert_parsed_gsl_to_normal_gsl(StaticGSLPrologFile, StaticGSLFile),
	!.

merge_dynamic_and_weighted_static_gsl(DynamicGSLFile,
				      WeightedStaticGSLFile,
				      WorkingDir,
				      MergingStrategy,
				      WeightedDynamicGSLFile) :-
	safe_absolute_file_name(WorkingDir, AbsWorkingDir),
	format_to_atom('~w/tmp_dynamic_gsl.pl', [AbsWorkingDir], DynamicGSLPrologFile),
	format_to_atom('~w/tmp_weighted_static_gsl.pl', [AbsWorkingDir], WeightedStaticGSLPrologFile),
	format_to_atom('~w/tmp_weighted_dynamic_gsl.pl', [AbsWorkingDir], WeightedDynamicGSLPrologFile),
	parse_regulus_generated_gsl(DynamicGSLFile, DynamicGSLPrologFile),
	parse_regulus_generated_gsl(WeightedStaticGSLFile, WeightedStaticGSLPrologFile),
	merge_dynamic_and_weighted_static_prolog_gsl(DynamicGSLPrologFile, WeightedStaticGSLPrologFile,
						     MergingStrategy, WeightedDynamicGSLPrologFile),
	convert_parsed_gsl_to_normal_gsl(WeightedDynamicGSLPrologFile, WeightedDynamicGSLFile).

%------------------------------------------------------------------------------------

dynamic_gsl_prolog_to_static(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	open(AbsInFile, read, SIn),
	open(AbsOutFile, write, SOut),

	format('~N~n--- Reading parsed dynamic GSL grammar from ~w~n', [AbsInFile]),
	dynamic_gsl_prolog_stream_to_static(SIn, SOut, 0-N),
	
	close(SIn),
	close(SOut),
	format('~N~n--- Written parsed static GSL grammar (~d non-terminals) to ~w~n', [N, AbsOutFile]),
	!.

dynamic_gsl_prolog_stream_to_static(SIn, SOut, InN-OutN) :-
	read(SIn, ParsedGSL),
	(   ParsedGSL = end_of_file ->
	    OutN = InN
	;
	    otherwise ->
	    dynamic_gsl_prolog_item_to_static(ParsedGSL, ParsedGSL1),
	    format(SOut, '~N~n', []),
	    prettyprintq_to_stream_unlimited_depth(SOut, ParsedGSL1),
	    format(SOut, '.~n', []),
	    NextN is InN + 1,
	    (   NextN > InN, 0 is NextN mod 100 ->
		format('~d ', [NextN]),
		flush_output(user)
	    ;
		true
	    ),
	    !,
	    dynamic_gsl_prolog_stream_to_static(SIn, SOut, NextN-OutN)
	).

dynamic_gsl_prolog_item_to_static(rule_group(LHS, RHS), rule_group(LHS1, RHS1)) :-
	dynamic_gsl_lhs_to_static(LHS, LHS1),
	dynamic_gsl_rhs_to_static(RHS, RHS1),
	!.
dynamic_gsl_prolog_item_to_static(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [dynamic_gsl_prolog_item_to_static(X, Y)]),
	fail.

dynamic_gsl_lhs_to_static(public_rule(Symbol), SymbolWithPrecedingPeriod) :-
	atom(Symbol),
	format_to_atom('.~w', [Symbol], SymbolWithPrecedingPeriod),
	!.
dynamic_gsl_lhs_to_static(Symbol, Symbol) :-
	atom(Symbol),
	!.
dynamic_gsl_lhs_to_static(LHS, LHS1) :-
	format2error('~N*** Error: bad call: ~w~n', [dynamic_gsl_lhs_to_static(LHS, LHS1)]),
	fail.

dynamic_gsl_rhs_to_static([], []) :-
	!.
dynamic_gsl_rhs_to_static([F | R], R1) :-
	external_grammar_rule(F),
	!,
	dynamic_gsl_rhs_to_static(R, R1).
dynamic_gsl_rhs_to_static([F | R], [F | R1]) :-
	!,
	dynamic_gsl_rhs_to_static(R, R1).

external_grammar_rule(rule(_Prob, external_grammar_non_terminal(_File, _Var), _Sem)).

%------------------------------------------------------------------------------------

merge_dynamic_and_weighted_static_prolog_gsl(DynamicGSLPrologFile, WeightedStaticGSLPrologFile,
					     MergingStrategy, WeightedDynamicGSLPrologFile) :-
	safe_absolute_file_name(DynamicGSLPrologFile, AbsDynamicGSLPrologFile),
	safe_absolute_file_name(WeightedStaticGSLPrologFile, AbsWeightedStaticGSLPrologFile),
	safe_absolute_file_name(WeightedDynamicGSLPrologFile, AbsWeightedDynamicGSLPrologFile),
	open(AbsDynamicGSLPrologFile, read, SInDynamic),
	open(AbsWeightedStaticGSLPrologFile, read, SInWeightedStatic),
	open(AbsWeightedDynamicGSLPrologFile, write, SOut),

	format('~N~n--- Reading parsed dynamic GSL grammar from ~w~n', [AbsDynamicGSLPrologFile]),
	format('~N~n--- Reading parsed weighted static GSL grammar from ~w~n', [WeightedStaticGSLPrologFile]),
	
	merge_dynamic_and_weighted_static_streams(SInDynamic, SInWeightedStatic, MergingStrategy, SOut, 0-N),
	
	close(SInDynamic),
	close(SInWeightedStatic),
	close(SOut),
	format('~N~n--- Written parsed weighted dynamic GSL grammar (~d non-terminals) to ~w~n',
	       [N, AbsWeightedDynamicGSLPrologFile]),
	!.

merge_dynamic_and_weighted_static_streams(SInDynamic, SInWeightedStatic, MergingStrategy, SOut, InN-OutN) :-
	read(SInDynamic, ParsedDynamicGSL),
	read(SInWeightedStatic, ParsedWeightedStaticGSL),
	(   ( ParsedDynamicGSL = end_of_file, ParsedWeightedStaticGSL = end_of_file ) ->
	    OutN = InN
	;
	    ( ParsedDynamicGSL = end_of_file ; ParsedWeightedStaticGSL = end_of_file ) ->
	    format2error('~N~n*** Error: dynamic and weighted static files have different numbers of entries~n', []),
	    fail
	;
	    otherwise ->
	    merge_dynamic_and_weighted_static_items(ParsedDynamicGSL, ParsedWeightedStaticGSL, MergingStrategy,
						    ParsedMergedGSL),
	    format(SOut, '~N~n', []),
	    prettyprintq_to_stream_unlimited_depth(SOut, ParsedMergedGSL),
	    format(SOut, '.~n', []),
	    NextN is InN + 1,
	    (   NextN > InN, 0 is NextN mod 100 ->
		format('~d ', [NextN]),
		flush_output(user)
	    ;
		true
	    ),
	    !,
	    merge_dynamic_and_weighted_static_streams(SInDynamic, SInWeightedStatic, MergingStrategy, SOut, NextN-OutN)
	).

merge_dynamic_and_weighted_static_items(Dynamic, WeightedStatic, Strategy, Merged) :-
	Dynamic = rule_group(DynamicLHS, DynamicBody),
	WeightedStatic = rule_group(WeightedStaticLHS, WeightedStaticBody),
	dynamic_and_weighted_static_lhs_are_compatible(DynamicLHS, WeightedStaticLHS),
	merge_dynamic_and_weighted_static_bodies(DynamicBody, WeightedStaticBody, Strategy, MergedBody),
	Merged = rule_group(DynamicLHS, MergedBody),
	!.
merge_dynamic_and_weighted_static_items(Dynamic, WeightedStatic, _Strategy, _Merged) :-
	format2error('~N*** Error: call to merge_dynamic_and_weighted_static_items/4 failed~n', []),
	format2error('~N~nDynamic item:~n', []),
	prettyprintq2error(Dynamic),
	format2error('~N~nWeighted static item:~n', []),
	prettyprintq2error(WeightedStatic),
	format2error('~N~n', []),
	fail.

dynamic_and_weighted_static_lhs_are_compatible(DynamicLHS, WeightedStaticLHS) :-
	DynamicLHS = WeightedStaticLHS,
	!.
dynamic_and_weighted_static_lhs_are_compatible(DynamicLHS, WeightedStaticLHS) :-
	DynamicLHS = public_rule(Symbol),
	atom(Symbol),
	format_to_atom('.~w', [Symbol], SymbolWithPrecedingPeriod),
	SymbolWithPrecedingPeriod = WeightedStaticLHS,
	!.
dynamic_and_weighted_static_lhs_are_compatible(DynamicLHS, WeightedStaticLHS) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [dynamic_and_weighted_static_lhs_are_compatible(DynamicLHS, WeightedStaticLHS)]),
	fail.

merge_dynamic_and_weighted_static_bodies([], [], _Strategy, []) :-
	!.
merge_dynamic_and_weighted_static_bodies([FDynamic | RDynamic], WeightedStatic, Strategy, [FMerged | RMerged]) :-
	FDynamic = rule(_OldProb, external_grammar_non_terminal(File, Var), Sem),
	FMerged = rule(Prob, external_grammar_non_terminal(File, Var), Sem),
	probability_for_external_grammar_rule(Strategy, Prob),
	!,
	merge_dynamic_and_weighted_static_bodies(RDynamic, WeightedStatic, Strategy, RMerged).
merge_dynamic_and_weighted_static_bodies([_FDynamic | RDynamic], [FWeightedStatic | RWeightedStatic], Strategy,
					 [FWeightedStatic | RMerged]) :-
	!,
	merge_dynamic_and_weighted_static_bodies(RDynamic, RWeightedStatic, Strategy, RMerged).
merge_dynamic_and_weighted_static_bodies(DynamicBody, WeightedStaticBody, _Strategy, _MergedBody) :-
	( DynamicBody = [] ; WeightedStaticBody = [] ),
	!,
	format2error('~N*** Error: bodies to be merged fail to match~n', []).

probability_for_external_grammar_rule(constant_weight(Prob), Prob) :-
	!.
probability_for_external_grammar_rule(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [probability_for_external_grammar_rule(X, Y)]),
	fail.

