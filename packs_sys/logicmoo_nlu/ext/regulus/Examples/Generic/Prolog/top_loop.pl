
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').
:- use_module('$REGULUS/PrologLib/utilities').
 
top_loop(Port, Package, NuanceParameters, StartMessage, InitialState, MainPred) :-
	absolute_file_name(Package, AbsPackage),
	regulus_sockettalk_debug,
	regulus_sockettalk_init(Port, AbsPackage, NuanceParameters),
	regulus_sockettalk_say_tts(StartMessage),
	!,
	loop(MainPred, InitialState).

loop(Pred, InState) :-
	format('~NPRESS RETURN TO RECOGNISE, "EXIT" TO END >> ', []),
	read_line(Line),
	(   Line = "EXIT" ->
	    regulus_sockettalk_exit_server ;

	    regulus_sockettalk_recognise('.MAIN', Recognised),
	    format('~N~nRecognised: ~w~n~n', [Recognised]),
	    Call =.. [Pred, Recognised, InState, OutState],
	    (   call(Call) ->
		true ;
		format('~NSomething went wrong.~n', []),
		InState = OutState
	    ),
	    !,
	    loop(Pred, OutState)
	).

top_loop_multi_package(Port, Packages, NuanceParameters, StartMessage, InitialState, MainPred,TopLevelGrammars) :-
	absolute_file_name_list(Packages, AbsPackages),
	regulus_sockettalk_debug,
	regulus_sockettalk_init_multi_package(Port, AbsPackages, NuanceParameters),
	regulus_sockettalk_say_tts(StartMessage),
	!,
	loop_multi_package(MainPred, InitialState, TopLevelGrammars).

loop_multi_package(Pred, InState, TopLevelGrammars) :-
	format('~NPRESS RETURN TO RECOGNISE, "EXIT" TO END >> ', []),
	read_line(Line),
	(   Line = "EXIT" ->
	    regulus_sockettalk_exit_server ;

	    recognise_with_all_packages(TopLevelGrammars, [Grammar,Recognised]),
	    format('~N~nRecognised: ~w~n~n', [Recognised]),
	    Call =.. [Pred, Recognised, Grammar, InState, OutState],
	    (   call(Call) ->
		true ;
		format('~NSomething went wrong.~n', []),
		InState = OutState
	    ),
	    !,
	    loop_multi_package(Pred, OutState, TopLevelGrammars)
	).
 
recognise_with_all_packages(TopLevelGrammars, BestGrammarAndResultPair):-
	recognise_with_all_packages1(TopLevelGrammars, GrammarAndResultPairs),
	(   pick_result_with_best_confidence_score(GrammarAndResultPairs, BestGrammarAndResultPair) ->
	    true
	;
	    otherwise ->
	    GrammarAndResultPairs = [BestGrammarAndResultPair | _]
	).

recognise_with_all_packages1([TopLevelGrammar1 | OtherTopLevelGrammars], [[TopLevelGrammar1,Result1]|OtherResults]):-
	regulus_sockettalk_recognise(TopLevelGrammar1, Result1),
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile),
	recognize_from_file_with_other_grammars(Wavfile, OtherTopLevelGrammars, OtherResults).

recognize_from_file_with_other_grammars(_Wavfile, [], []).
recognize_from_file_with_other_grammars(Wavfile, [TopLevelGrammar | OtherTopLevelGrammars], [[TopLevelGrammar, Result] | OtherResults]):-
	regulus_sockettalk_recognise_file(Wavfile, TopLevelGrammar, Result),
	recognize_from_file_with_other_grammars(Wavfile, OtherTopLevelGrammars, OtherResults).

pick_result_with_best_confidence_score([[Grammar,Result]], [Grammar,Result]).
pick_result_with_best_confidence_score([[Grammar1,Result1],[_Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair):-
	Result1 = recognition_succeeded(Confidence1, _Words1, _LF1),
	Result2 = recognition_succeeded(Confidence2, _Words2, _LF2),
	Confidence1 >= Confidence2,
	pick_result_with_best_confidence_score([[Grammar1,Result1]|GrammarAndResultPairs], BestGrammarAndResultPair).

pick_result_with_best_confidence_score([[_Grammar1,Result1],[Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair):-
	Result1 = recognition_succeeded(Confidence1, _Words1, _LF1),
	Result2 = recognition_succeeded(Confidence2, _Words2, _LF2),
	Confidence2 > Confidence1,
	pick_result_with_best_confidence_score([[Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair).

pick_result_with_best_confidence_score([[Grammar1,Result1],[_Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair):-
	Result1 = recognition_succeeded(_Confidence1, _Words1, _LF1),
	\+ Result2 = recognition_succeeded(_Confidence2, _Words2, _LF2),
	pick_result_with_best_confidence_score([[Grammar1,Result1]|GrammarAndResultPairs], BestGrammarAndResultPair).

pick_result_with_best_confidence_score([[_Grammar1,Result1],[Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair):-
	\+ Result1 = recognition_succeeded(_Confidence1, _Words1, _LF1),
	Result2  = recognition_succeeded(_Confidence2, _Words2, _LF2),
	pick_result_with_best_confidence_score([[Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair).

pick_result_with_best_confidence_score([[_Grammar1,Result1],[_Grammar2,Result2]|GrammarAndResultPairs], BestGrammarAndResultPair):-
	\+ Result1 = recognition_succeeded(_Confidence1, _Words1, _LF1),
	\+ Result2  = recognition_succeeded(_Confidence2, _Words2, _LF2),
	pick_result_with_best_confidence_score(GrammarAndResultPairs, BestGrammarAndResultPair).

absolute_file_name_list([], []).
absolute_file_name_list([F | R], [F1 | R1]) :-
	safe_absolute_file_name(F, F1),
	absolute_file_name_list(R, R1).
