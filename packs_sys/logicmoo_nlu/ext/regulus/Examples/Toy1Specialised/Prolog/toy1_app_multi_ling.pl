 
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1Specialised/Prolog/input_manager_multi_ling').
:- use_module('$REGULUS/Examples/Toy1Specialised/Prolog/dialogue_manager_multi_ling').
:- use_module('$REGULUS/Examples/Toy1Specialised/Prolog/output_manager_multi_ling').
:- use_module('$REGULUS/Examples/Toy1Specialised/Prolog/dialogue_state').

:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

%======================================================================

toy1_app(Port) :-
	RecognitionPackages = ['$REGULUS/Examples/Toy1Specialised/Generated/toy1_specialised_eng_recogniser','$REGULUS/Examples/Toy1Specialised/Generated/french_recogniser'],
	initial_dialogue_state(InitialState),
	top_loop_multi_package(Port,
			       RecognitionPackages,
			       'rm.Port=1234 audio.OutputVolume=200',
			       "Ready to start",
			       InitialState,
			       process_recogniser_output,
			       ['.MAIN__english','.MAIN']).

grammar_to_language('.MAIN__english', english).
grammar_to_language('.MAIN', french).


%======================================================================		

process_recogniser_output(RecogniserOutput, Grammar, InState, OutState) :-
	format('~N~n              Old state: ~w', [InState]),

	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=LF]),
	format('~N~n  Words from recogniser: "~w" (confidence: ~d)~n', [Words, Confidence]),
	format('~N                     LF: ~w~n', [LF]),
	grammar_to_language(Grammar, Language),
	set_in_state(language, InState, Language, NewInState),
	regulus_sockettalk_set_parameter('tts.ResourceName', Language),
	regulus_sockettalk_get_parameter('tts.ResourceName', SetLanguage),
	(   Language = english ->
	    regulus_sockettalk_set_parameter('client.TTSAddresses', 'localhost:32323')
	;
	    Language = french ->
	    regulus_sockettalk_set_parameter('client.TTSAddresses', 'localhost:32324')
	),
	format('~NTTS language set to "~w"~n', [SetLanguage]),
	
	lf_to_dialogue_move(LF, NewInState, DialogueMove),
	format('~N          Dialogue move: ', []), portray_clause(DialogueMove),
	
	update_dialogue_state(DialogueMove, NewInState, AbstractAction, OutState),
	format('~N        Abstract action: ~w', [AbstractAction]),
	
	abstract_action_to_action(AbstractAction, Action),
	convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	format('~N        Concrete action: ~w', [PrintFormForAction]),
	
	perform_action(Action),

	format('~N~n              New state: ~w~n', [OutState]),
	!.
process_recogniser_output(RecogniserOutput, _Grammar, InState, OutState) :-
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	Action = say("Sorry, something went wrong"),
	perform_action(Action),
	InState = OutState,
	!.

%======================================================================

perform_action(Action) :-
	(   ( Action = say_string(String), is_prolog_string(String) ) ->
	    String1 = String ;
	    String1 = "Sorry, something went wrong"
	),
	regulus_sockettalk_say_tts(String1),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.

	