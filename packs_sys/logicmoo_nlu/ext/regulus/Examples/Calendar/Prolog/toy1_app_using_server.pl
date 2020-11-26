
%======================================================================

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/Prolog/recognition').

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusSpeechServer/Prolog/regulus_sockettalk').

:- use_module(library(sockets)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(system)).
:- use_module(library(lists)).

%======================================================================

% Nuance parameters

beamwidth(1600).
nbest_n(6).

%======================================================================

:- dynamic debug_mode/0.

set_debug_mode :-
	retractall(debug_mode),
	assertz(debug_mode).
set_nodebug_mode :-
	retractall(debug_mode).

call_if_debug_mode(Goal) :-
	debug_mode,
	!,
	call(Goal).
call_if_debug_mode(_Goal).

call_if_not_debug_mode(Goal) :-
	\+ debug_mode,
	!,
	call(Goal).
call_if_not_debug_mode(_Goal).

%======================================================================

toy1_app(RegserverPort, DialogueServerPort, RecognitionPackage) :-
	call_if_debug_mode(regulus_sockettalk_debug),
	connect_to_server_over_socket(DialogueServerPort),
	%send_request_to_server(action(execute_regulus_command("ECHO_ON")), _Response1),
	%send_request_to_server(action(execute_regulus_command("SET_NOTIONAL_SPEAKER manny")), _Response2),

	get_timestamped_record_dir(RecordDirectory),
	absolute_file_name(RecordDirectory, AbsRecordDirectory),
	nbest_n(NBestN),
	beamwidth(Beamwidth),
	
	format_to_atom('client.TTSAddresses=localhost audio.OutputVolume=200 rec.ConfidenceRejectionThreshold=0 rec.Pruning=~d rec.DoNBest=TRUE rec.NumNBest=~d client.RecordDirectory=~w',
		       [Beamwidth, NBestN, RecordDirectory],
		       RegserverParams),

	on_exception(Exception,
		     top_loop(RegserverPort,
			      RecognitionPackage,
			      RegserverParams,
			      "Ready to start",
			      [record_directory=AbsRecordDirectory],
			      process_recogniser_output),
		     handle_exception_in_top_loop(Exception)),

	close_server_socket.

handle_exception_in_top_loop(Exception) :-
	format('*** Exception in top-loop: ~w~n', [Exception]),
	close_server_socket.
		       
%======================================================================

top_loop(Port, Package, NuanceParameters, StartMessage, InitialState, MainPred) :-
	safe_absolute_file_name(Package, AbsPackage),
	regulus_sockettalk_init(Port, AbsPackage, NuanceParameters),
	regulus_sockettalk_say_tts(StartMessage),
	!,
	loop(MainPred, InitialState).

loop(Pred, InState) :-
	format('~N==========================================================~n~n', []),
	format('~NPRESS [RETURN] TO RECOGNISE, "REVERT" FOR PREVIOUS CONTEXT, "RESTART" TO ERASE CONTEXT, "EXIT" TO END >> ', []),
	read_line(Line),
	split_string_into_words(Line, Words),
	(   Words = ['EXIT'] ->
	    regulus_sockettalk_exit_server
	;
	    Words = ['RESTART'] ->
	    return_to_initial_context,
	    !,
	    loop(Pred, InState)
	;
	    Words = ['REVERT'] ->
	    revert_to_previous_context,
	    !,
	    loop(Pred, InState)
	;
	    Words = [] ->
	    regulus_sockettalk_recognise('.MAIN', Recognised),
	    call_if_debug_mode(format('~N~nReceived from recogniser: ~n~n', [])),
	    call_if_debug_mode(prettyprint(Recognised)),
	    nl, nl,
	    Call =.. [Pred, Recognised, InState, OutState],
	    (   call(Call) ->
		true ;
		format('~NSomething went wrong.~n', []),
		InState = OutState
	    ),
	    !,
	    loop(Pred, OutState)
	;
	    format('~N~nUnknown command: "~s"~n', [Line]),
	    !,
	    loop(Pred, OutState)
	).

%======================================================================

return_to_initial_context :-
	format('~N~nAttempting to return to initial context... ', []),
	(   send_request_to_server(action(return_to_initial_context), Response) ->
	    format('~w~n~n', [Response])
	;
	    otherwise ->
	    format('~w~n~n', [error])
	),
	!.

revert_to_previous_context :-
	format('~N~nAttempting to revert to previous context... ', []),
	(   send_request_to_server(action(revert_discourse_context), Response) ->
	    format('~w~n~n', [Response])
	;
	    otherwise ->
	    format('~w~n~n', [error])
	),
	!.

process_recogniser_output(RecogniserOutput, State, State) :-
	RecogniserOutput = recognition_succeeded(NBestList),
	is_list(NBestList),
	format_nbest_list_for_dialogue_server(NBestList, NBestList1),
	call_if_not_debug_mode(print_nbest_list(NBestList1)),
	send_request_to_server(action(process_nbest_list(NBestList1)), Response),
	process_dm_server_response(Response),
	do_help_processing(State, RecogniserOutput),
	!.
process_recogniser_output(RecogniserOutput, State, State) :-
	RecogniserOutput = recognition_succeeded(_Confidence, Words, [value=_LF]),
	call_if_not_debug_mode(format('~NRecognised: "~w"~n', [Words])),
	send_request_to_server(action(process_rec_string(Words)), Response),
	process_dm_server_response(Response),
	do_help_processing(State, RecogniserOutput),
	nl,
	!.
process_recogniser_output(RecogniserOutput, State, State) :-
	call_if_debug_mode(format('~N~nUnable to process recogniser output:~n', [])),
	call_if_debug_mode(prettyprint(RecogniserOutput)),
	call_if_debug_mode(nl),
	
	call_if_not_debug_mode(format('~N~nSorry, something went wrong~n', [])),
	!.

%======================================================================

process_dm_server_response(Response) :-
	(   member(selected=SelectedSent, Response) ->
	    format('~N~nSelected: "~w"~n', [SelectedSent])
	;
	    true
	),
	(   member(paraphrase=Paraphrase, Response) ->
	    format('~N~nUnderstood as: "~w"~n', [Paraphrase])
	;
	    true
	),
	(   member(action=Action, Response) ->
	    perform_action(Action, DisplayFormForAction),
	    format('~N~nResponse: "~w"~n', [DisplayFormForAction])
	;
	    otherwise ->
	    format('~N*** Error: no action in dialogue server response: ~w~n', [Response])
	),
	!.
process_dm_server_response(Response) :-
	format('~N*** Error: bad call: ~w~n',
	       [process_dm_server_response(Response)]),
	fail.

%======================================================================

% Nbest list is list of elements of form rec_result(Confidence, Words, [value=LF])

format_nbest_list_for_dialogue_server([], []) :-
	!.
format_nbest_list_for_dialogue_server([F | R], [F1 | R1]) :-
	format_nbest_list_element_for_dialogue_server(F, F1),
	!,
	format_nbest_list_for_dialogue_server(R, R1).
format_nbest_list_for_dialogue_server(NBestList, NBestList1) :-
	format('~N*** Error: bad call: ~w~n', [format_nbest_list_for_dialogue_server(NBestList, NBestList1)]),
	fail.

format_nbest_list_element_for_dialogue_server(rec_result(Confidence, Words, _NLResult),
					      nbest_hyp(Confidence, Words)) :-
	!.
format_nbest_list_element_for_dialogue_server(F, F1) :-
	format('~N*** Error: bad call: ~w~n',
	       [format_nbest_list_element_for_dialogue_server(F, F1)]),
	fail.

%======================================================================

print_nbest_list(NBestList) :-
	format('~NRecognised:~n~n', []),
	print_nbest_list1(NBestList),
	!.
print_nbest_list(NBestList) :-
	format('~N*** Error: bad call: ~w~n',
	       [print_nbest_list(NBestList)]),
	fail.

print_nbest_list1([]).
print_nbest_list1([nbest_hyp(_Confidence, Words) | R]) :-
	format('~N~w~n', [Words]),
	!,
	print_nbest_list1(R).

%======================================================================

perform_action(tts(Arg), Atom) :-
	(   is_prolog_string(Arg) ->
	    atom_codes(Atom, Arg)
	;
	    atom(Arg) ->
	    Atom = Arg,
	    atom_codes(Arg, String)
	;
	    Atom = 'Sorry, something went wrong',
	    String = "Sorry, something went wrong"
	),
	regulus_sockettalk_say_tts(String).
perform_action(wavfile_tts(Atom, WavfileAtom0), Atom) :-
	atom(WavfileAtom0),
	postprocess_wavfile_output_atom(WavfileAtom0, WavfileAtom),
	regulus_sockettalk_say_list_atom(WavfileAtom),
	!.
perform_action(Action, 'Sorry, something went wrong') :-
	format('~N~nUnable to perform action: ~w~n', [Action]),
	!.

%======================================================================

do_help_processing(_State, _GLMRecogniserOutput) :-
	get_most_recent_recorded_wavfile(Wavfile),
	call_if_debug_mode(format('~N~n--- Current wavfile: ~w~n~n', [Wavfile])),
	absolute_file_name(Wavfile, AbsWavfile),
	regulus_sockettalk_recognise_file(AbsWavfile, '.MAIN_SLM', RecogniserOutput),
	get_top_hyp(RecogniserOutput, Words),
	send_request_to_server(action(get_help_examples(Words, 5)), HelpResponse),
	display_help_response(Words, HelpResponse),
	!.
do_help_processing(State, RecogniserOutput) :-
	format('~N*** Warning: bad call: ~w~n~n',
	       [do_help_processing(State, RecogniserOutput)]).
	
get_most_recent_recorded_wavfile(Wavfile) :-
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile).

get_top_hyp(RecogniserOutput, Words) :-
	RecogniserOutput = recognition_succeeded(_Confidence, Words, _NLResult),
	atomic(Words),
	!.
get_top_hyp(RecogniserOutput, Words) :-
	RecogniserOutput = recognition_succeeded(NBestList),
	NBestList = [rec_result(_Confidence, Words, _NLResult) | _],
	atomic(Words),
	!.

display_help_response(Words, help(Atom)) :-
	atom_codes(Atom, String),
	format('~N~nHELP EXAMPLES FOR STRING "~w":~n~n~s~n~n', [Words, String]),
	!.
display_help_response(Words, Response) :-
	format('~N~n*** BAD HELP RESPONSE FOR STRING "~w":~n~n~w~n~n', [Words, Response]),
	!.
	
%======================================================================

connect_to_server_over_socket(Port) :-
	current_host(Host),
	%socket('AF_INET', Socket),
	safe_socket('AF_INET', Socket),
	%socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	safe_socket_connect(Socket, Host, Port, Stream),
	store_dialogue_server_socket_and_stream(Socket, Stream),
	format('~N~n--- Connected to dialogue server on port ~d~n', [Port]),
	!.
connect_to_server_over_socket(Port) :-
	format('~N~n*** Error: Unable to connect to dialogue server on port ~d~n', [Port]),
	!.

close_server_socket :-
	get_dialogue_server_socket_and_stream(_Socket, Stream),
	format('~N~n--- Closing down connection to dialogue server...~n', []),
	(   get_confirmation('Keep server running (y/n)? ', []) ->
	    Message = disconnect
	;
	    Message = client_shutdown
	),
	send_request_to_server(Message, _ReplyTerm),
	!,
	%socket_close(Socket),
	%safe_socket_close(Socket),
	close(Stream),
	format('~N~n--- Closed down connection to dialogue server.~n', []).

send_request_to_server(Action, ReplyTerm) :-
	get_dialogue_server_socket_and_stream(_Socket, Stream),
	call_if_debug_mode(format('~N~n--- Sending message to dialogue server: ~w~n', [Action])),
	format(Stream, '~q.~n', [Action]),
	flush_output(Stream),
	read(Stream, ReplyTerm),
	call_if_debug_mode(format('~N~n--- Received reply from dialogue server: ~w~n', [ReplyTerm])),
	!.
send_request_to_server(Action) :-
	format('~N~n*** Error: Couldn\'t send message to dialogue server: ~w~n', [Action]),
	fail.

:- dynamic dialogue_server_socket_and_stream/2.

store_dialogue_server_socket_and_stream(Socket, Stream) :-
	retractall(dialogue_server_socket_and_stream(_, _)),
	assertz(dialogue_server_stream(Socket, Stream)),
	!.

get_dialogue_server_socket_and_stream(Socket, Stream) :-
	dialogue_server_stream(Socket, Stream),
	!.
get_dialogue_server_stream(_Socket, _Stream) :-
	format('~N~n*** Error: No dialogue socket and stream defined~n', []),
	fail.

%---------------------------------------------------------------

get_timestamped_record_dir(RecordDirectory) :-
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format_to_atom('../corpora/speech/~w', [Timestamp], RecordDirectory),
	absolute_file_name(RecordDirectory, AbsRecordDirectory),
	format('~N--- Making directory ~w~n', [AbsRecordDirectory]),
	make_directory(AbsRecordDirectory).

get_confirmation(FormatAtom, Args) :-
	format(FormatAtom, Args),
	read_line(user, Chars),
	split_string_into_words(Chars, Words),
	(   Words = [y] ->
	    true ;

	    Words = [n] ->
	    fail ;

	    format('~NPlease answer \'y\' or \'n\'~n', []),
	    !,
	    get_confirmation(FormatAtom, Args)
	).
 
