
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/input_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/output_manager').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Generic/Prolog/open_mic_client.pl').

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

% OpenMicServerPort = 4547

toy1_app :-
	toy1_app(4547).

%======================================================================

toy1_app(OpenMicServerPort) :-

	start_open_mic_server(OpenMicServerPort),

	regulus_sockettalk_say_tts("Waiting for open mic input"),
	initial_dialogue_state(InitialState),
	top_loop_asynchronous(InitialState),

	shut_down_open_mic_server.
		       
%======================================================================

top_loop_asynchronous(InState) :-
	wait_for_message(Message),
	process_received_message(Message, Action, InState, OutState),
	perform_action(Action),	
	!,
	top_loop_asynchronous(OutState).

%======================================================================		

wait_for_message(Message) :-
	get_message(Message),
	!.
wait_for_message(Message) :-
	sleep(0.1),
	!,
	wait_for_message(Message).

get_message(Message) :-
	check_for_open_mic_recognition_result(RecognitionResult),
	Message = RecognitionResult,
	!.

%======================================================================		

process_received_message(RecogniserOutput, Action, InState, OutState) :-
	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=LF]),
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile),
	format('~N~n--- Current wavfile: ~w~n~n', [Wavfile]),
	
	format('~N~n              Old state: ~w', [InState]),

	format('~N~n  Words from recogniser: "~w" (confidence: ~d)~n', [Words, Confidence]),
	format('~N                     LF: ~w~n', [LF]),
	
	lf_to_dialogue_move(LF, InState, DialogueMove),
	format('~N          Dialogue move: ', []), portray_clause(DialogueMove),
	
	update_dialogue_state(DialogueMove, InState, AbstractAction, OutState),
	format('~N        Abstract action: ~w', [AbstractAction]),
	
	abstract_action_to_action(AbstractAction, Action),
	convert_strings_to_quoted_atoms(Action, PrintFormForAction),
	format('~N        Concrete action: ~w', [PrintFormForAction]),
	
	format('~N~n              New state: ~w~n', [OutState]),
	!.
process_received_message(RecogniserOutput, no_action, InState, OutState) :-
	(   RecogniserOutput = recognition_succeeded(_Confidence, _Words, [value=_LF]) ;
	    RecogniserOutput = recognition_failed(_Reason)
	),
	format('~N~nUnable to process recogniser output: ~w~n', [RecogniserOutput]),
	InState = OutState,
	!.
process_received_message(OtherMessage, Action, InState, OutState) :-
	process_non_recognition_message(OtherMessage, Action, InState, OutState),
	!.

process_non_recognition_message(OtherMessage, Action, InState, OutState) :-
	format('~NWould process non-recognition message: ~q~n', [OtherMessage]),
	InState = OutState,
	% Placeholder action - would be something non-trivial in real app.
	Action = action_resulting_from_non_rec_message(OtherMessage),
	!.

%======================================================================

perform_action(action_resulting_from_non_rec_message(_Message)) :-
	!.
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
