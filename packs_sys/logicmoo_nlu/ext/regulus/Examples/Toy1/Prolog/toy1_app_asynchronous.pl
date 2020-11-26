
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/input_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/output_manager').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module('$REGULUS/Examples/Generic/Prolog/open_mic_client.pl').
:- use_module('$REGULUS/Examples/Generic/Prolog/backend_server_client.pl').

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

% OpenMicServerPort = 8060
% RegserverPort = 8061
% BackendServerPort = 8062

toy1_app :-
	toy1_app(7020, 7022).

%======================================================================

toy1_app(OpenMicServerPort, BackendServerPort) :-

	start_open_mic_server(OpenMicServerPort),
	connect_to_backend_server_over_socket(BackendServerPort),
	
	initial_dialogue_state(InitialState),
	top_loop_asynchronous(InitialState),

	shut_down_open_mic_server,
	close_backend_server_socket.
		       
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
get_message(Message) :-
	check_for_reply_from_backend_server(ReplyTerm),
	Message = ReplyTerm,
	!.

%======================================================================		

process_received_message(RecogniserOutput, Action, InState, OutState) :-
	RecogniserOutput = recognition_succeeded(Confidence, Words, [value=LF]),
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

/*
perform_action(Action) :-
	format('~N--- Would perform action: ~q~n', [Action]),
	(   Action = action_resulting_from_non_rec_message(_Message) ->
	    true ;
	    send_request_to_backend_server(action(dummy_action))
	),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.
*/

perform_action(action_resulting_from_non_rec_message(_Message)) :-
	!.
perform_action(Action) :-
	(   ( Action = tts(String), is_prolog_string(String) ) ->
	    String1 = String ;
	    String1 = "Sorry, something went wrong"
	),
	regulus_sockettalk_say_tts(String1),
	send_request_to_backend_server(action(dummy_action)),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.
