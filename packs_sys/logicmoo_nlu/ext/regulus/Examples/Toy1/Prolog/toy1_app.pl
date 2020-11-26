
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/input_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/output_manager').

:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

%======================================================================

runtime_entry(start) :-
	toy1_app(1975).

%======================================================================

toy1_app(Port) :-
	RecognitionPackage = '$REGULUS/Examples/Toy1/Generated/recogniser',
	initial_dialogue_state(InitialState),
	top_loop(Port,
		 RecognitionPackage,
		 'audio.OutputVolume=200',
		 "Ready to start",
		 InitialState,
		 process_recogniser_output).
		       
%======================================================================		

process_recogniser_output(RecogniserOutput, InState, OutState) :-
	format('~N~n              Old state: ~w', [InState]),

	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=LF]),
	format('~N~n  Words from recogniser: "~w" (confidence: ~d)~n', [Words, Confidence]),
	format('~N                     LF: ~w~n', [LF]),
	
	lf_to_dialogue_move(LF, InState, DialogueMove),
	format('~N          Dialogue move: ', []), portray_clause(DialogueMove),
	
	update_dialogue_state(DialogueMove, InState, AbstractAction, OutState),
	format('~N        Abstract action: ~w', [AbstractAction]),
	
	abstract_action_to_action(AbstractAction, Action),
	convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	format('~N        Concrete action: ~w', [PrintFormForAction]),
	
	perform_action(Action),

	format('~N~n              New state: ~w~n', [OutState]),
	!.
process_recogniser_output(RecogniserOutput, InState, OutState) :-
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	Action = tts("Sorry, something went wrong"),
	perform_action(Action),
	InState = OutState,
	!.

%======================================================================

perform_action(Action) :-
	(   ( Action = tts(String), is_prolog_string(String) ) ->
	    String1 = String ;
	    String1 = "Sorry, something went wrong"
	),
	regulus_sockettalk_say_tts(String1),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.





	