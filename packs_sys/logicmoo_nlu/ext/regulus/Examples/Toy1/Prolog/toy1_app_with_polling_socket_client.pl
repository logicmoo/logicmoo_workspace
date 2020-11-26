
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%======================================================================

:- use_module('$REGULUS/Examples/Toy1/Prolog/input_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/dialogue_manager').
:- use_module('$REGULUS/Examples/Toy1/Prolog/output_manager').

:- ensure_loaded('$REGULUS/Examples/Generic/Prolog/top_loop.pl').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%======================================================================

toy1_app(RegserverPort, BackendServerPort) :-
	RecognitionPackage = '$REGULUS/Examples/Toy1/Generated/recogniser',

	connect_to_server_over_socket(BackendServerPort),
	
	initial_dialogue_state(InitialState),
	top_loop(RegserverPort,
		 RecognitionPackage,
		 'audio.OutputVolume=200',
		 "Ready to start",
		 InitialState,
		 process_recogniser_output),

	close_server_socket.
		       
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
	send_request_to_server(action(Action), ReplyTerm),
	format('~N~n--- Received reply from backend server: ~q~n', [ReplyTerm]),
	!.
perform_action(Action) :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.

%======================================================================

connect_to_server_over_socket(Port) :-
	current_host(Host),
	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	store_backend_server_socket_and_stream(Socket, Stream),
	format('~N~n--- Connected to backend server on port ~d~n', [Port]),
	!.
connect_to_server_over_socket(Port) :-
	format('~N~n*** Error: Unable to connect to backend server on port ~d~n', [Port]),
	!.

close_server_socket :-
	get_backend_server_socket_and_stream(Socket, _Stream),
	format('~N~n--- Closing down connection to backend server...~n', []),
	send_request_to_server(client_shutdown, _ReplyTerm),
	socket_close(Socket),
	format('~N~n--- Closed down connection to backend server.~n', []).

send_request_to_server(Action, ReplyTerm) :-
	get_backend_server_socket_and_stream(_Socket, Stream),
	format('~N~n--- Sending message to backend server: ~w~n', [Action]),
	format(Stream, '~q.~n', [Action]),
	flush_output(Stream),
	(   Action = client_shutdown ->
	    true ;
	    get_reply_from_stream(Stream, ReplyTerm)
	),
	!.
send_request_to_server(Action) :-
	format('~N~n*** Error: Couldn\'t send message to backend server: ~w~n', [Action]),
	fail.

:- dynamic backend_server_socket_and_stream/2.

store_backend_server_socket_and_stream(Socket, Stream) :-
	retractall(backend_server_socket_and_stream(_, _)),
	assertz(backend_server_stream(Socket, Stream)),
	!.

get_backend_server_socket_and_stream(Socket, Stream) :-
	backend_server_stream(Socket, Stream),
	!.
get_backend_server_stream(_Socket, _Stream) :-
	format('~N~n*** Error: No backend socket and stream defined~n', []),
	fail.

%======================================================================

get_reply_from_stream(Stream, ReplyTerm) :-
	poll_for_waiting_reply(Stream),
	read(Stream, ReplyTerm),
	!.
get_reply_from_stream(Stream, ReplyTerm) :-
	format('~N~nCouldn\'t find anything on socket - waited one second and trying again~n', []),
	!,
	get_reply_from_stream(Stream, ReplyTerm).

%======================================================================

poll_for_waiting_reply(Stream) :-	
	socket_select([], [], 1:0, [Stream], StreamsWithInput),
	StreamsWithInput \== [],
	!.




	