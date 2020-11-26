
/*

Dialogue server. The call

  server(<PortNumber>)

starts a dialogue server that accepts requests on the port <PortNumber>.

The following requests are supported. The format of messages is formally
defined in $REGULUS/Prolog/check_dialogue_server_messages.pl:

 Request: client(ClientHost).
Response: accept(ClientHost).

 Request: get_message_mode.
Response: message_mode({prolog,xml,json,pure_json}) or error 

Find out what the current message mode is.
IMPORTANT: THIS MESSAGE AND ITS RESPONSE ARE ALWAYS FORMATTED IN PROLOG,
SINCE THE WHOLE POINT IS THAT THE CLIENT DOESN'T KNOW WHAT THE RIGHT FORMAT IS

 Request: restart_server(CFGFile, CommandsAsAtoms, InitParameter).
Response: ok or error

Reload resources from CFGFile, using CommandsAsAtoms, which should be a list of Regulus commands
expressed as atoms. Finally reinitialise the dialogue context using InitParameter, and remove
all stored contexts.

 Request: restart_server(RestartTag).
Response: ok or error

If there is a declaration of the form

user:restart_server_decl(RestartTag, L1, L2, CFGFile, CommandsAsAtoms, InitParameter)

then as restart_server(CFGFile, CommandsAsAtoms, InitParameter). Also
stores RestartTag so that it can be accessed by the message last_restart_server_tag.

 Request: last_restart_server_tag
Response: restart_tag(Tag) or error

Returns the most recent tag used in a restart_server(RestartTag), or error if there is none

 Request: restart_recognition_processes
Response: ok or error

Restart the Regserver

 Request: action(xml_messages).
Response: Format future messages in both directions in XML form. Each message will
          be of the form

	  xml_message(<XMLString>).

          where <XMLString> is an XML encoding of the corresponding Prolog message
	  produced using the predicate prolog_xml/2 in $REGULUS/PrologLib/prolog_xml.pl.
          The XML can be converted back into Prolog if necessary using the same predicate.

 Request: action(json_messages).
Response: Format future messages in both directions in JSON form. Each message will
          be of the form

	  json_message(<JSONString>).

          where <JSONString> is an JSON encoding of the corresponding Prolog message
	  produced using the predicate prolog_json/2 in $REGULUS/PrologLib/prolog_json_compact.pl.
          The JSON can be converted back into Prolog if necessary using the same predicate.

 Request: action(pure_json_messages).
Response: Format future messages in both directions in JSON form. Each message will
          be of the form

	  <JSONString>

          where <JSONString> is an JSON encoding of the corresponding Prolog message
	  produced using the predicate prolog_json/2 in $REGULUS/PrologLib/prolog_json_compact.pl.
          The JSON can be converted back into Prolog if necessary using the same predicate.

 Request: action(prolog_messages).
Response: Format future messages in both directions in Prolog form (default). 

 Request: action(concrete_actions).
Response: Pass concrete actions to the client (default). 

 Request: action(abstract_actions).
Response: Pass abstract actions to the client, so that the client can do its own output management. 

 Request: action(process_rec_string('<RecognitionString>')).
          e.g. action(process_rec_string('what meetings are there next week')).
Response: [action=tts('<TTSString>')].
          e.g. [action=tts('meeting on July 9 2007')].
          error. [Something went wrong]

 Request: action(robust_process_string('<RecognitionString>')).
          e.g. action(robust_process_string('like um what meetings are there you know next week')).
Response: [action=tts('<TTSString>')].
          e.g. [action=tts('meeting on July 9 2007')].
          error. [Something went wrong]

 Request: action(process_nbest_list([nbest_hyp(<Confidence1>, <Words1>), nbest_hyp(<Confidence2>, <Words2>), ...])).
          e.g. action(process_nbest_list([nbest_hyp(55, 'when is next meeting'), nbest_hyp(43, 'what is next meeting')])).
Response: [selected=<ChosenWords>,action=tts('<TTSString>')].
          e.g. [selected='when is next meeting',action=tts('meeting on July 9 2007')].
          error. [Something went wrong]

 Request: action(recognise_and_dialogue_process).
 Request: action(recognise_and_dialogue_process_from_wavfile(Wavfile)).
Response: Performs recognition according to current parameters, then performs dialogue processing
          as with action(process_rec_string(...)) or action(process_nbest_list(...)) above.
          Requires recognition to be loaded in the server (LOAD_RECOGNITION command)

Request: action(slm_recognise_and_dialogue_process).
Request: action(slm_recognise_and_dialogue_process_from_wavfile(Wavfile)).

Response: Performs recognition according to current parameters, then performs dialogue
          processing with the input form slm_result(TopHyp) where TopHyp is an
	  atom representing the top/only recognition hypothesis

 Request: action(get_most_recent_recorded_wavfile).
Response: <Wavfile> or error.
          Requires recognition to be loaded in the server (LOAD_RECOGNITION command)

 Request: action(get_current_recognition_grammar).
Response: <Grammar> or error.

Request:  action(play_wavfile(<Wavfile>))
Response: Passes play-wavfile request to Regserver
          Requires recognition to be loaded in the server (LOAD_RECOGNITION command)

Request:  action(tts(<Atom>))
Response: Passes tts request to Regserver
          Requires recognition to be loaded in the server (LOAD_RECOGNITION command)

 Request: action(process_xml_message('<XMLStringRepresentingNBestList>')).	  
Response: as with action(process_nbest_list(...)) above

 Request: action(get_help_examples(<Words>, <NExamples>)).
          e.g. action(get_help_examples('what be next meeting geneva', 2)).
Response: help('<HelpString>').
          <HelpString> is a string of <NExamples> help responses separated by newlines.
          e.g. help('when is the next meeting\nwhen is the next meeting in geneva').
          error. [Something went wrong]

Request:  action(return_to_initial_context).
Response: ok. [Successful]
          error. [Something went wrong]

Request:  action(set_notional_time(Timestamp)).
Response: ok. [Successful]
          error. [Something went wrong]

Request:  action(log(Message)).
Response: ok. [Successful]
          error. [Something went wrong]

	  Add a timestamped message to the logfile

 Request: disconnect.
Response: disconnect. [Server disconnects but stays up and running]

 Request: client_shutdown.
Response: thread_shutdown. [Server disconnects and closes down]

Note the periods after the requests and responses.

Socket code adapted from code at http://dlp.cs.vu.nl/~ctv/dlpinfo/srcs/tcp/sicsock.pl.html

*/

%----------------------------------------------------------------------------------

:- ['$REGULUS/Prolog/load'].

:- use_module(generate).
:- use_module(regulus_utilities).
:- use_module(parse_xml_rec_result).
:- use_module(prolog_logging).
:- use_module(check_dialogue_server_messages).

:- use_module('$REGULUS/PrologLib/utilities').
%:- use_module('$REGULUS/PrologLib/prolog_xml').
:- use_module('$REGULUS/PrologLib/prolog_xml_compact').
:- use_module('$REGULUS/PrologLib/prolog_json_compact').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
:- use_module(library(sockets)).

%----------------------------------------------------------------------------------

% Print all error messages to stdout or whatever is doing its
% job. This is necessary for remote compilation and loading.

user:message_hook(Severity, _Message, Lines):-
	telling(MyStream),
	print_message_lines(MyStream, Severity, Lines).

%----------------------------------------------------------------------------------

:- dynamic dialogue_server_debug_level/1.

valid_debug_level(DebugLevel) :-
	integer(DebugLevel),
	1 =< DebugLevel, DebugLevel =< 5,
	!.

set_dialogue_server_debug_level(DebugLevel) :-
	retractall(dialogue_server_debug_level(_)),
	assertz(dialogue_server_debug_level(DebugLevel)),
	!.

get_dialogue_server_debug_level(DebugLevel) :-
	dialogue_server_debug_level(DebugLevel),
	!.
get_dialogue_server_debug_level(DebugLevel) :-
	DebugLevel = 5.

call_if_debug_level_at_least(Threshold, Goal) :-
	get_dialogue_server_debug_level(DebugLevel),
	DebugLevel >= Threshold,
	!,
	call(Goal).
call_if_debug_level_at_least(_Threshold, _Goal).

call_if_debug_level_at_least_or_else(Threshold, Goal1, Goal2) :-
	get_dialogue_server_debug_level(DebugLevel),
	(   DebugLevel >= Threshold ->
	    call(Goal1)
	;
	    call(Goal2)
	).

print_timing_info_if_bug_level_at_least(Threshold, Goal, Operation) :-
	absolute_timed_call(Goal, TimeInSeconds),
	update_total_processing_times(Operation, TimeInSeconds),
	get_average_processing_time_for_events_of_type(Operation, AverageTimeInSeconds),
	
	safe_format_to_chars('~2f', [TimeInSeconds], TimeString),
	safe_format_to_chars('~2f', [AverageTimeInSeconds], AverageTimeString),
	log_dialogue_string_message('    TIMING', Operation, TimeString),
	log_dialogue_string_message('AV. TIMING', Operation, AverageTimeString),

	get_dialogue_server_debug_level(DebugLevel),
	(   DebugLevel >= Threshold ->
	    format('~N---     Time for ~w: ~2f secs~n', [Operation, TimeInSeconds]),
	    format('~N--- Av. time for ~w: ~2f secs~n', [Operation, AverageTimeInSeconds])
	;
	    otherwise ->
	    true
	).

%----------------------------------------------------------------------------------

:- dynamic number_of_timed_events_of_type/2.
:- dynamic total_processing_time_for_events_of_type/2.

init_dialogue_server_timing_info :-
	retractall(number_of_timed_events_of_type(_, _)),
	retractall(total_processing_time_for_events_of_type(_, _)).

get_number_of_timed_events_of_type(Type, N) :-
	number_of_timed_events_of_type(Type, N),
	!.
get_number_of_timed_events_of_type(_Type, 0).

get_total_processing_time_for_events_of_type(Type, Total) :-
	total_processing_time_for_events_of_type(Type, Total),
	!.
get_total_processing_time_for_events_of_type(_Type, 0).

get_average_processing_time_for_events_of_type(Type, Average) :-
	get_number_of_timed_events_of_type(Type, N),
	(   N = 0 ->
	    Average is 0.0
	;
	    otherwise ->
	    get_total_processing_time_for_events_of_type(Type, Total),
	    Average is Total / N
	),
	!.
get_average_processing_time_for_events_of_type(Type, Average) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_average_processing_time_for_events_of_type(Type, Average)]),
	fail.

update_total_processing_times(Type, TimeTaken) :-
	get_number_of_timed_events_of_type(Type, OldN),
	get_total_processing_time_for_events_of_type(Type, OldTotal),

	NewN is OldN + 1,
	NewTotal is OldTotal + TimeTaken,

	retractall(number_of_timed_events_of_type(Type, _)),
	retractall(total_processing_time_for_events_of_type(Type, _)),

	assertz(number_of_timed_events_of_type(Type, NewN)),
	assertz(total_processing_time_for_events_of_type(Type, NewTotal)),
	!.
update_total_processing_times(Type, TimeTaken) :-
	format('~N*** Error: bad call: ~w~n',
	       [update_total_processing_times(Type, TimeTaken)]),
	fail.

%----------------------------------------------------------------------------------

server(Port, CFGFile) :-
	server(Port, CFGFile, ["NUANCE_PARSER", "LOAD_DIALOGUE", "LOAD_HELP"], '$REGULUS/logfiles', []).

server(Port, CFGFile, InitCommands, LogfileDir) :-
	server(Port, CFGFile, InitCommands, LogfileDir, '*no_init_parameter*', []).

server(Port, CFGFile, InitCommands, LogfileDir, InitParameter) :-
	server(Port, CFGFile, InitCommands, LogfileDir, InitParameter, []).

server(Port, CFGFile, InitCommands, LogfileDir, InitParameter, OpenStreamParameters) :-
	%trace,
	%spy(recognise_and_dialogue_process),
	set_current_config_file(CFGFile),
	start_new_dialogue_session_logfile(LogfileDir),
	init_server(CFGFile, InitCommands),
	!,
	server1(Port, InitParameter, OpenStreamParameters).

%----------------------------------------------------------------------------------

init_server(CFGFile, InitCommands) :-
	regulus_batch(CFGFile, InitCommands),
	init_dialogue_server_timing_info.

%----------------------------------------------------------------------------------

server1(Port, InitParameter, OpenStreamParameters) :-
	safe_socket_server_open(Port, Socket),
	format('~N~nDialogue server ready to accept connections on port ~d.~n~n', [Port]),
	flush_output(user),
	log_dialogue_event(dialogue_server_ready),
	!,
	server2(Socket, parameter(InitParameter), OpenStreamParameters).

server2(Socket, ParameterOrState, OpenStreamParameters) :-
	(   OpenStreamParameters = [] ->
	    safe_socket_server_accept(Socket, _Client, Stream)
	;
	    otherwise ->
	    safe_socket_server_accept(Socket, _Client, Stream, OpenStreamParameters)
	),
	format('~N~nDialogue server connected.~n~n', []),
	log_dialogue_event(dialogue_server_connected),
	%trace,
	(   ParameterOrState = parameter(InitParameter) ->
	    init_dm(InitParameter, InitialState)
	;
	    ParameterOrState = state(InitialState) ->
	    true
	;
	    format('~N*** Error: bad second arg "~q" in call to server2~n', [ParameterOrState]),
	    fail
	),
	server_loop(Stream, InitialState, [], EndOrRestart),
	close(Stream),
	log_dialogue_event(dialogue_server_disconnect),
	!,
	(   EndOrRestart = end ->
	    format('Exit server~n', []),
	    log_dialogue_event(dialogue_server_exit)
	;
	    set_message_mode(prolog),
	    server2(Socket, state(InitialState), OpenStreamParameters)
	).

%----------------------------------------------------------------------------------

server_from_file(File, CFGFile, InitCommands, LogfileDir, Logfile, InitParameter) :-
	init_server_from_file(CFGFile, InitCommands, InitParameter, InitialState),
	server_from_file_no_init(File, LogfileDir, Logfile, InitialState).

%----------------------------------------------------------------------------------

init_server_from_file(CFGFile, InitCommands, InitParameter, InitialState) :-
	set_current_config_file(CFGFile),
	init_server(CFGFile, InitCommands),
	init_dm(InitParameter, InitialState).
	
%----------------------------------------------------------------------------------

server_from_file_no_init(File, LogfileDir, Logfile, InitialState) :-
	start_new_dialogue_session_logfile(LogfileDir, Logfile),
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, read, Stream),
	format('~N~nDialogue server reading input from file ~w.~n~n', [AbsFile]),
	flush_output(user),
	server_from_file_loop(Stream, InitialState, []),
	close(Stream).

%----------------------------------------------------------------------------------

init_dm(InitParameter, InitialState) :-
	(   InitParameter = '*no_init_parameter*' -> 
	    initial_dialogue_state(InitialState)
	;
	    otherwise ->
	    initial_dialogue_state(InitParameter, InitialState)
	),
	init_saved_dialogue_state,
	save_dialogue_state(default, InitialState).

%----------------------------------------------------------------------------------

server_loop(Stream, InState, InPreviousStates, EndOrRestart) :-
	%print_long_string,

	(   get_message_mode(pure_json) ->
	    read_lines_until_non_empty_line(Stream, ClientRequest0)
	;
	    otherwise ->
	    read(Stream, ClientRequest0)
	),

	process_server_loop_item(Stream, ClientRequest0, ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates),
	!,
	(   ClientRequest == client_shutdown ->
	    EndOrRestart = end
	;
	    ClientRequest == disconnect ->
	    EndOrRestart = restart
	;
	    server_loop(Stream, NextState, NextPreviousStates, EndOrRestart)
	),
	!.

process_server_loop_item(Stream, ClientRequest0, ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates) :-
	timed_call(process_server_loop_item_main(Stream, ClientRequest0, ClientRequest,
						 InState, InPreviousStates, NextState, NextPreviousStates),
		   TimeTaken),
	call_if_debug_level_at_least(2, format('~N~nItem processed, ~3f secs~n', [TimeTaken])).	
	
process_server_loop_item_main(Stream, ClientRequest0, ClientRequest,
			      InState, InPreviousStates, OutState, OutPreviousStates) :-
	call_if_debug_level_at_least_or_else(2,
					     format('~N~n----------------------------------------------~n', []),
					     (format('.', []), flush_output(user))
					    ),	

	print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest),

	(   server_input(ClientRequest, InState, InPreviousStates, OutState, OutPreviousStates, ServerReply0) ->
	    true
	;
	    format('~N~n*** Error: unable to process message: ~q~n', [ClientRequest]),
	    OutState = InState,
	    OutPreviousStates = InPreviousStates,
	    ServerReply0 = error
	),

	(   no_reply_required_for_request(ClientRequest) ->
	    log_no_reply_required(ServerReply0)
	;
	    otherwise ->
	    print_log_and_possibly_encode_client_response(ServerReply0, ServerReply),
	    server_reply(ServerReply, Stream)
	),
	
	flush_output(user),
	!.

%----------------------------------------------------------------------------------

server_from_file_loop(Stream, InState, InPreviousStates) :-
	read(Stream, ClientRequest0),
	call_if_debug_level_at_least_or_else(2,
					     format('~N~n----------------------------------------------~n', []),
					     call_if_debug_level_at_least(1,
									  (format('.', []), flush_output(user)))
					    ),	

	print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest),
	
	server_input(ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates, ServerReply0),

	print_log_and_possibly_encode_client_response(ServerReply0, _ServerReply),

	flush_output(user),
	!,
	(   member(ClientRequest, [end_of_file, disconnect, client_shutdown]) ->
	    true
	
	;   otherwise ->
	    server_from_file_loop(Stream, NextState, NextPreviousStates)
	).

%----------------------------------------------------------------------------------

print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest) :-
	(   ClientRequest0 = get_message_mode
	;
	    is_list_of_non_negative_integers(ClientRequest0),
	    is_substring("get_message_mode.", ClientRequest0)
	),
	%trace,
	ClientRequest = get_message_mode,
	log_dialogue_event(received(ClientRequest)),
	!.
print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest) :-
	ClientRequest0 = xml_message(XMLClientRequestString),
	current_datime_formatted_as_HMS(Datime),
	(   safe_prolog_xml(ClientRequest, XMLClientRequestString) ->
	    call_if_debug_level_at_least(2, format('~N~nServer received (~w):~n~s~n~ninterpreted as:~n~n~w~n', [Datime, XMLClientRequestString, ClientRequest]))
	;
	    ClientRequest = bad_xml,
	    call_if_debug_level_at_least(2, format('~N~nServer received (~w): "~s"~nbut could not interpret~n',
						   [Datime, XMLClientRequestString]))
	),
	call_if_debug_level_at_least(2, check_client_message(ClientRequest)),
	%atom_codes(XMLClientRequestAtom, XMLClientRequestString),
	%log_dialogue_event(received_xml(XMLClientRequestAtom)),
	log_dialogue_string_message('RECEIVED XML', xml_message, XMLClientRequestString),
	log_dialogue_event(received(ClientRequest)),
	!.
print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest) :-
	(   ClientRequest0 = json_message(JSONClientRequestString)
	;
	    get_message_mode(pure_json),
	    ClientRequest0 = JSONClientRequestString,
	    is_list_of_non_negative_integers(JSONClientRequestString)
	),
	current_datime_formatted_as_HMS(Datime),
	(   safe_prolog_json(ClientRequest, JSONClientRequestString) ->
	    call_if_debug_level_at_least(2, format('~N~nServer received (~w):~n~s~n~ninterpreted as:~n~n~w~n', [Datime, JSONClientRequestString, ClientRequest]))
	;
	    ClientRequest = bad_json,
	    call_if_debug_level_at_least(2, format('~N~nServer received (~w): "~s"~nbut could not interpret it~n',
						   [Datime, JSONClientRequestString]))
	),
	call_if_debug_level_at_least(2, check_client_message(ClientRequest)),
	%atom_codes(JSONClientRequestAtom, JSONClientRequestString),
	%log_dialogue_event(received_json(JSONClientRequestAtom)),
	log_dialogue_string_message('RECEIVED JSON', json_message, JSONClientRequestString),
	log_dialogue_event(received(ClientRequest)),
	!.
print_log_and_possibly_decode_client_request(ClientRequest0, ClientRequest) :-
	ClientRequest0 = ClientRequest,
	current_datime_formatted_as_HMS(Datime),
	call_if_debug_level_at_least(2, format('~N~nServer received (~w): ~w~n', [Datime, ClientRequest])),
	log_dialogue_event(received(ClientRequest)),
	call_if_debug_level_at_least(2, check_client_message(ClientRequest)),
	!.

% The response to the "get_message_mode" message is always sent as Prolog.
print_log_and_possibly_encode_client_response(ServerReply0, ServerReply) :-
	ServerReply0 = message_mode(_Mode),
	log_dialogue_event(response(ServerReply0)),
	ServerReply = ServerReply0,
	!.
print_log_and_possibly_encode_client_response(ServerReply0, ServerReply) :-
	(   postprocessing_is_defined ->
	    true
	;
	    otherwise ->
	    call_if_debug_level_at_least(2, check_server_message(ServerReply0))
	),
	current_datime_formatted_as_HMS(Datime),
	log_dialogue_event(response(ServerReply0)),
	(   get_message_mode(xml) ->
	    (   safe_prolog_xml(ServerReply0, XMLServerReplyString) ->
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n~nencoded as~n~s~n', [Datime, ServerReply0, XMLServerReplyString]))
	    ;
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n but failed to encoded as XML~n', [Datime, ServerReply0])),
		XMLServerReplyString = "unable to encode as XML"
	    ),
	    ServerReply = xml_message(XMLServerReplyString)
	    %atom_codes(XMLServerReplyAtom, XMLServerReplyString),
	    %log_dialogue_event(xml_response(XMLServerReplyAtom))
	;
	    get_message_mode(json) ->
	    (   safe_prolog_json(ServerReply0, JSONServerReplyString) ->
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n~nencoded as~n~s~n', [Datime, ServerReply0, JSONServerReplyString]))
	    ;
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n but failed to encoded as JSON~n', [Datime, ServerReply0])),
		JSONServerReplyString = "unable to encode as JSON"
	    ),
	    ServerReply = json_message(JSONServerReplyString)
	    %atom_codes(JSONServerReplyAtom, JSONServerReplyString),
	    %log_dialogue_event(json_response(JSONServerReplyAtom))
	;
	    get_message_mode(pure_json) ->
	    (   safe_prolog_json(ServerReply0, JSONServerReplyString) ->
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n~nencoded as~n~s~n', [Datime, ServerReply0, JSONServerReplyString]))
	    ;
		call_if_debug_level_at_least(2, format('  ~nServer response (~w): ~w~n but failed to encoded as JSON~n', [Datime, ServerReply0])),
		JSONServerReplyString = "unable to encode as JSON"
	    ),
	    ServerReply = JSONServerReplyString
	;
	    otherwise ->
	    ServerReply0 = ServerReply
	),
	!.

%----------------------------------------------------------------------------------

log_no_reply_required(ServerReply) :-
	log_dialogue_event(unsent_response(ServerReply)),
	!.
	
%----------------------------------------------------------------------------------

no_reply_required_for_request(action_with_no_reply(_Request)).
no_reply_required_for_request(action_for_user_with_no_reply(_Id, _Request)).
no_reply_required_for_request(action_for_session_with_no_reply(_Id, _Request)).

%----------------------------------------------------------------------------------

server_input(ClientRequest, InState, InPreviousStates, InState, InPreviousStates, ServerReply) :-
	ClientRequest = client(ClientHost),
	!,
	ServerReply = accept(ClientHost).
server_input(ClientRequest, InState, InPreviousStates, InState, InPreviousStates, ServerReply) :-
	ClientRequest = get_message_mode,
	!,
	get_message_mode(Mode),
	ServerReply = message_mode(Mode).
server_input(ClientRequest, InState, InPreviousStates, NewState, NewPreviousStates, ServerReply) :-
	ClientRequest = restart_server(RestartTag),
	!,
	(   \+ current_predicate(user:restart_server_decl/6) ->
	    format('~N*** Error: restart_server_decl/6 is not defined, can\'t process ~w~n', [restart_server(RestartTag)]),
	    NewState = InState, 	    
	    NewPreviousStates = InPreviousStates,
	    Response = error
	;
	    user:restart_server_decl(RestartTag, _L1, _L2, CFGFile, CommandsAsAtoms, InitParameter) ->
	    ClientRequest1 = restart_server(CFGFile, CommandsAsAtoms, InitParameter),
	    format('~N--- Trying to restart server with CFG file = ~w, commands = ~w and init parameter = ~w',
		   [CFGFile, CommandsAsAtoms, InitParameter]),
	    server_input(ClientRequest1, InState, InPreviousStates, NewState, NewPreviousStates, ServerReply),
	    set_last_server_restart_tag(RestartTag)
	;
	    otherwise ->
	    format('~N*** Error: no restart_server_decl for ~w~n', [RestartTag]),
	    NewState = InState, 	    
	    NewPreviousStates = InPreviousStates,
	    Response = error
	).
server_input(ClientRequest, InState, InPreviousStates, NewState, NewPreviousStates, ServerReply) :-
	ClientRequest = last_restart_server_tag,
	!,
	(   get_last_server_restart_tag(RestartTag) ->
	    ServerReply = restart_tag(RestartTag)
	;
	    otherwise ->
	    ServerReply = error
	),
	NewState = InState,
	NewPreviousStates = InPreviousStates.
server_input(ClientRequest, InState, InPreviousStates, NewState, NewPreviousStates, ServerReply) :-
	(   ClientRequest = restart_server(CFGFile, CommandsAsAtoms, InitParameter)
	;
	    ClientRequest = restart_server(CFGFile, CommandsAsAtoms),
	    InitParameter = '*no_init_parameter*'
	),
	!,
	(   handle_restart_server(CFGFile, CommandsAsAtoms, InitParameter, NewState, NewPreviousStates, Response) ->
	    ServerReply = Response
	;
	    otherwise ->
	    NewState = InState, 	    
	    NewPreviousStates = InPreviousStates,
	    Response = error
	).
server_input(action_with_no_reply(Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	!,
	server_input(action(Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply).
server_input(action_for_user_with_no_reply(Id, Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	!,
	server_input(action_for_user(Id, Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply).
server_input(action_for_session_with_no_reply(Id, Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	!,
	server_input(action_for_session(Id, Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply).
server_input(ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	ClientRequest = action(Request),
	!,
	handle_action_request(Request, InState, InPreviousStates, NextState, NextPreviousStates, Response),
	ServerReply = Response.
server_input(ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	ClientRequest = action_for_user(Id, Request),
	!,
	server_input(action_for_session(Id, Request), InState, InPreviousStates, NextState, NextPreviousStates, ServerReply).
server_input(ClientRequest, InState, InPreviousStates, NextState, NextPreviousStates, ServerReply) :-
	ClientRequest = action_for_session(Id, Request),
	!,
	%trace,
	handle_action_request(set_session(Id), InState, InPreviousStates, NextState1, NextPreviousStates1, ok),
	handle_action_request(Request, NextState1, NextPreviousStates1, NextState, NextPreviousStates, Response),
	ServerReply = Response.
server_input(ClientRequest, InState, InPreviousStates, InState, InPreviousStates, ServerReply) :-
	( ClientRequest = end_of_file ; ClientRequest = disconnect ),
	!,
	ServerReply = disconnect.
server_input(ClientRequest, InState, InPreviousStates, InState, InPreviousStates, ServerReply) :-
	ClientRequest = client_shutdown,
	!,
	ServerReply = thread_shutdown.
server_input(ClientRequest, InState, InPreviousStates, InState, InPreviousStates, ServerReply) :-
	ServerReply = unknown_request(ClientRequest),
	call_if_debug_level_at_least(2, format('Unknown client request: ~w~n', [ClientRequest])).

%----------------------------------------------------------------------------------

server_reply(client_shutdown, _ServerOut) :-
	call_if_debug_level_at_least(1, format('Server: end of file.~n', [])),
	!.
% The response to the "get_message_mode" message is always sent as Prolog.
server_reply(ServerReply, ServerOut) :-
	ServerReply = message_mode(_Mode),
	format(ServerOut, '~q.~n', [ServerReply]),
	flush_output(ServerOut),
	!.
server_reply(ServerReply, ServerOut) :-
	(   ( ServerReply = xml_message(String), is_list_of_non_negative_integers(String) ) ->
	    quote_layout_chars_in_string(String, String1),
	    format(ServerOut, 'xml_message("~s").~n', [String1]),
	    log_dialogue_string_message('SENT XML', xml_message, String1)
	;
	    ( ServerReply = json_message(String), is_list_of_non_negative_integers(String) ) ->
	    quote_layout_chars_in_string(String, String1),
	    format(ServerOut, 'json_message("~s").~n', [String1]),
	    log_dialogue_string_message('SENT JSON', json_message, String1)
	;
	    ( get_message_mode(pure_json), ServerReply = String, is_list_of_non_negative_integers(String) ) ->
	    format(ServerOut, '~s~n', [String]),
	    log_dialogue_string_message('SENT JSON', json_message, String)
	;
	    otherwise ->
	    format(ServerOut, '~q.~n', [ServerReply])
	),
	flush_output(ServerOut).

%----------------------------------------------------------------------------------

handle_restart_server(CFGFile, CommandsAsAtoms, InitParameter, NewState, NewPreviousStates, Response) :-
	on_exception(
		     Exception,
		     handle_restart_server1(CFGFile, CommandsAsAtoms, InitParameter, NewState, NewPreviousStates, Response),
		     handle_exception_in_restart_server(Exception, CFGFile, CommandsAsAtoms, InitParameter)
		    ),
	!.
handle_restart_server(CFGFile, CommandsAsAtoms, InitParameter, _NewState, _NewPreviousStates, _Response) :-
	call_if_debug_level_at_least(2, format('~N~n*** Unable to restart server due to internal error ***~n~n', [])),
	call_if_debug_level_at_least(2, format('~N~n*** CFGFile = ~w, commands = ~w, init parameter = ~w ***~n~n',
					       [CFGFile, CommandsAsAtoms, InitParameter])),
	!,
	fail.

handle_exception_in_restart_server(Exception, CFGFile, CommandsAsAtoms, InitParameter) :-
	call_if_debug_level_at_least(2, format('~N~n*** Unable to restart server due to exception ***~n~n', [])),
	call_if_debug_level_at_least(2, format('~N~n*** CFGFile = ~w, commands = ~w, init parameter = ~w ***~n~n',
					       [CFGFile, CommandsAsAtoms, InitParameter])),
	print_message(error, Exception),
	fail.

handle_restart_server1(CFGFile, CommandsAsAtoms, InitParameter, InitialState, InitialPreviousStates, Response) :-
	(   atom(CFGFile) ->
	    true
	;
	    format('~N*** Error: CFG file "~w" in restart message is not an atom~n', [CFGFile]),
	    fail
	),
	unpack_list_of_command_atoms(CommandsAsAtoms, Commands),
	set_current_config_file(CFGFile),
	init_server(CFGFile, Commands),
	init_dm(InitParameter, InitialState),
	InitialPreviousStates = [],
	Response = ok.

unpack_list_of_command_atoms([], []).
unpack_list_of_command_atoms([F | R], [F1 | R1]) :-
	unpack_command_atom(F, F1),
	!,
	unpack_list_of_command_atoms(R, R1).

unpack_command_atom(Atom, _String) :-
	\+ atom(Atom),
	!,
	format('~N*** Error: command "~w" in restart message is not an atom~n', [Atom]),
	fail.
unpack_command_atom(Atom, String) :-
	atom(Atom),
	atom_codes(Atom, String),
	!.

%----------------------------------------------------------------------------------

handle_action_request(Request, InState, InPreviousStates, NextState, NextPreviousStates, Response) :-
	on_exception(
	Exception, 
	handle_action_request1(Request, InState, InPreviousStates, NextState, NextPreviousStates, Response),
	handle_exception_in_action_request(Exception, InState, InPreviousStates, NextState, NextPreviousStates, Response)
    ),
	!.
handle_action_request(_Request, InState, InPreviousStates, InState, InPreviousStates, error) :-
	call_if_debug_level_at_least(2, format('~N~n*** Processing aborted due to internal error ***~n~n', [])),
	!.

handle_exception_in_action_request(Exception, InState, InPreviousStates, NextState, NextPreviousStates, Response) :-
	Response = error,
	InState = NextState,
	InPreviousStates = NextPreviousStates,
	call_if_debug_level_at_least(2, format('~N~n*** Processing aborted due to internal exception ***~n~n', [])),
	print_message(error, Exception).

%----------------------------------------------------------------------------------

handle_action_request1(ClientRequest, InState, InPreviousStates, OutState, OutPreviousStates, Action) :-
	ClientRequest =.. [action_sequence | ClientRequests],
	%trace,
	handle_action_request1_list(ClientRequests, InState, InPreviousStates, OutState, OutPreviousStates, Actions),
	Action =.. [action_sequence | Actions],
	!.
handle_action_request1(log(_AnyStructure), InState, InPreviousStates, OutState, OutPreviousStates, Action) :-
	OutState = InState,
	OutPreviousStates = InPreviousStates,
	Action = ok,
	!.
handle_action_request1(set_session(Id), InState, InPreviousStates, OutState, OutPreviousStates, Action) :-
	get_current_session(Id),
	OutState = InState,
	OutPreviousStates = InPreviousStates,
	Action = ok,
	!.
% Discard the set of previous states when we switch to a new user - it'll be too expensive to store them.
handle_action_request1(set_session(NewId), InState, _InPreviousStates, OutState, OutPreviousStates, Action) :-
	get_current_session(OldId),
	save_dialogue_state(OldId, InState),
	
	get_dialogue_state(NewId, OutState),
	set_current_session(NewId),
	OutPreviousStates = [],
	Action = ok,
	!.
handle_action_request1(process_xml_message(XMLAtom), InState, InPreviousStates, OutState, OutPreviousStates, Action) :-
	atom_codes(XMLAtom, String),
	(   using_strcat_semantics ->
	    parse_xml_rec_result(String, keep_sem_strcat, NBestList)
	;
	    otherwise ->
	    parse_xml_rec_result(String, discard_sem, NBestList)
	),
	handle_action_request1(process_nbest_list(NBestList), InState, InPreviousStates, OutState, OutPreviousStates, Action),
	!.
handle_action_request1(get_current_recognition_grammar, InState, InPreviousStates, InState, InPreviousStates, Action) :-
	(   safe_get_recognition_grammar_from_state(InState, Grammar) ->
	    Action = grammar(Grammar)
	;
	    otherwise ->
	    Action = error
	),
	!.
handle_action_request1(process_nbest_list(_Wavfile, NBestList), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	process_nbest_recogniser_output(NBestList, InState, OutState, Action),
	!.
handle_action_request1(process_nbest_list(NBestList), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	process_nbest_recogniser_output(NBestList, InState, OutState, Action),
	!.
handle_action_request1(process_rec_string(_Wavfile, Words), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	process_recogniser_output(Words, InState, OutState, Action),
	!.
handle_action_request1(process_rec_string(Words), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	process_recogniser_output(Words, InState, OutState, Action),
	!.
handle_action_request1(robust_process_string(Words), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	robust_process_string(Words, InState, OutState, Action),
	!.
handle_action_request1(restart_recognition_processes, InState, InPreviousStates, InState, InPreviousStates, Action) :-
	try_to_restart_recognition_processes_in_state(Action, InState),
	!.
handle_action_request1(recompile_system, InState, InPreviousStates, InState, InPreviousStates, Action) :-
	try_to_recompile_system(InState, Action),
	!.
handle_action_request1(reload_system, InState, InPreviousStates, InState, InPreviousStates, Action) :-
	try_to_reload_system(InState, Action),
	!.
handle_action_request1(get_debug_level, InState, InPreviousStates, InState, InPreviousStates, Action) :-
	try_to_get_debug_level(Action),
	!.
handle_action_request1(set_debug_level(Level), InState, InPreviousStates, InState, InPreviousStates, Action) :-
	try_to_set_debug_level(Level, Action),
	!.
handle_action_request1(recognise_and_dialogue_process, InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	recognise_and_process_recogniser_one_best_or_nbest_output(live, default, InState, OutState, Action),
	!.
handle_action_request1(recognise_and_dialogue_process_from_wavfile(Wavfile), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	recognise_and_process_recogniser_one_best_or_nbest_output(wavfile(Wavfile), default, InState, OutState, Action),
	!.
handle_action_request1(slm_recognise_and_dialogue_process, InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	recognise_and_process_recogniser_one_best_or_nbest_output(live, slm, InState, OutState, Action),
	!.
handle_action_request1(slm_recognise_and_dialogue_process_from_wavfile(Wavfile), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	recognise_and_process_recogniser_one_best_or_nbest_output(wavfile(Wavfile), slm, InState, OutState, Action),
	!.
handle_action_request1(get_most_recent_recorded_wavfile, InState, InPreviousStates, OutState, InPreviousStates, Action) :-
	get_most_recent_recorded_wavfile(Action),
	InState = OutState,
	!.
handle_action_request1(play_wavfile(Wavfile), InState, InPreviousStates, OutState, InPreviousStates, Action) :-
	play_wavfile(Wavfile),
	InState = OutState,
	Action = ok,
	!.
handle_action_request1(tts(Atom), InState, InPreviousStates, OutState, InPreviousStates, Action) :-
	perform_tts(Atom),
	InState = OutState,
	Action = ok,
	!.
handle_action_request1(process_non_speech_input(AtomOrTerm), InState, InPreviousStates, OutState, [InState | InPreviousStates], Action) :-
	process_non_speech_input(AtomOrTerm, InState, OutState, Action),
	!.
handle_action_request1(get_help_examples(Words, NExamples), InState, InPreviousStates, InState, InPreviousStates, Response) :-
	process_help_request(Words, NExamples, Response),
	!.
handle_action_request1(return_to_initial_context, InState, _InPreviousStates, OutState, [], Response) :-
	process_return_to_initial_dialogue(InState, OutState, Response),
	!.
handle_action_request1(revert_discourse_context, _InState, [LastState | PreviousStates], LastState, PreviousStates, ok) :-
	!.
handle_action_request1(revert_discourse_context, _InState, [], _NextState, _NextPreviousStates, _Response) :-
	!,
	format('~N*** Error: no previous state to revert to~n', []),
	fail.
handle_action_request1(set_notional_time(Timestamp), InState, InPreviousStates, InState, InPreviousStates, ok) :-
	atom_codes(Timestamp, TimeChars),
	(   parse_datime(TimeChars, Datime) ->
	    
	    set_notional_time(Datime) ;
	    
	    format('~NUnable to parse time "~w". Format = YYYY-MM-DD_HH-MM-SS, e.g. 2006-12-31_23-59-59~n', [Timestamp]),
	    fail
	),
	!.
handle_action_request1(execute_regulus_command(CommandString), InState, InPreviousStates, InState, InPreviousStates, ok) :-
	get_current_config_file(ConfigFile),
	regulus_batch(ConfigFile, [CommandString]),
	!.
handle_action_request1(xml_messages, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_message_mode(xml),
	!.
handle_action_request1(json_messages, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_message_mode(json),
	!.
handle_action_request1(pure_json_messages, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_message_mode(pure_json),
	!.
handle_action_request1(prolog_messages, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_message_mode(prolog),
	!.
handle_action_request1(concrete_actions, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_action_mode(concrete),
	!.
handle_action_request1(abstract_actions, InState, InPreviousStates, InState, InPreviousStates, ok) :-
	set_action_mode(abstract),
	!.

%----------------------------------------------------------------------------------

handle_action_request1_list([], InState, InPreviousStates, OutState, OutPreviousStates, Action) :-
	InState = OutState,
	InPreviousStates = OutPreviousStates,
	Action = [],
	!.
handle_action_request1_list([F | R], InState, InPreviousStates, OutState, OutPreviousStates, [F1 | R1]) :-
	handle_action_request1(F, InState, InPreviousStates, NextState, NextPreviousStates, F1),
	!,
	handle_action_request1_list(R, NextState, NextPreviousStates, OutState, OutPreviousStates, R1).

%----------------------------------------------------------------------------------

process_non_speech_input(AtomOrTerm, InState, OutState, Response) :-
	(   ( atom(AtomOrTerm), atom_codes(AtomOrTerm, Chars), interpret_string_as_raw_lf_input(Chars, LF) ) ->
	    true
	;
	    otherwise ->
	    LF = AtomOrTerm
	),
	print_timing_info_if_bug_level_at_least(5,
						dialogue_process_lf(LF, InState, OutState, Record),
						'dialogue processing (non-recognition)'),
	get_concrete_or_abstract_action_from_record(Record, Action),
	Response = [action=Action],
	call_if_debug_level_at_least(3, print_dialogue_processing_record(user, Record)),
	!.
process_non_speech_input(RecogniserOutput, InState, OutState, FinalResponse) :-
	call_if_debug_level_at_least(2, format('~N~nUnable to process non-speech input: ~w~n~n', [RecogniserOutput])),
	Response = [action=tts('Sorry, something went wrong')],
	maybe_postprocess_response(Response, FinalResponse),
	InState = OutState,
	!.

process_recogniser_output(Atom, InState, OutState, FinalResponse) :-
	dialogue_process_utterance(Atom, InState, OutState, Record),
	get_concrete_or_abstract_action_from_record(Record, Action),
	call_if_debug_level_at_least(3, print_dialogue_processing_record(user, Record)),
	recognition_result_atom_to_selected_recognition_result_alist(Atom, Selected),
	(   member(paraphrase=[Paraphrase, _Judgement], Record) ->
	    append(Selected, [action=Action, paraphrase=Paraphrase], Response)
	;
	    otherwise ->
	    append(Selected, [action=Action], Response)
	),
	maybe_postprocess_response(Response, FinalResponse),
	!.
process_recogniser_output(RecogniserOutput, InState, OutState, FinalResponse) :-
	call_if_debug_level_at_least(2, format('~N~nUnable to process recogniser output: ~w~n~n', [RecogniserOutput])),
	Response = [action=tts('Sorry, something went wrong')],
	maybe_postprocess_response(Response, FinalResponse),
	InState = OutState,
	!.

robust_process_string(Atom, InState, OutState, FinalResponse) :-
	get_help_matches_with_lf(Atom, 1, Matches, Traces),
	Matches = [[MatchedSent, Move]],
	Traces = [RobustMatchScore-Details],
	\+ help_trace_shows_trivial_matching(RobustMatchScore-Details),
	dialogue_process_sent_and_dialogue_move(MatchedSent, Move, InState, OutState, Record),
	get_concrete_or_abstract_action_from_record(Record, Action),
	call_if_debug_level_at_least(3, print_dialogue_processing_record(user, Record)),
	recognition_result_atom_to_selected_recognition_result_alist(MatchedSent, Selected),
	(   member(paraphrase=[Paraphrase, _Judgement], Record) ->
	    append(Selected, [action=Action, paraphrase=Paraphrase], Response)
	;
	    otherwise ->
	    append(Selected, [action=Action], Response)
	),
	maybe_postprocess_response(Response, FinalResponse),
	!.
robust_process_string(Atom, InState, OutState, FinalResponse) :-
	call_if_debug_level_at_least(2, format('~N~nUnable to do robust processing on: ~w~n~n', [Atom])),
	Response = [action=tts('Sorry, something went wrong')],
	maybe_postprocess_response(Response, FinalResponse),
	InState = OutState,
	!.

process_nbest_recogniser_output(NBestList, InState, OutState, FinalResponse) :-
	dialogue_process_nbest_list(NBestList, InState, OutState, Record, RankOfChosenElement),
	safe_nth(RankOfChosenElement, NBestList, nbest_hyp(_, SelectedSentOrSentAndLF)),
	possibly_extract_sent_from_sent_and_lf(SelectedSentOrSentAndLF, SelectedSent),
	get_concrete_or_abstract_action_from_record(Record, Action),
	call_if_debug_level_at_least(3, print_dialogue_processing_record(user, Record)),
	recognition_result_atom_to_selected_recognition_result_alist(SelectedSent, Selected),
	(   member(paraphrase=[Paraphrase, _Judgement], Record) ->
	    append(Selected, [action=Action, paraphrase=Paraphrase], Response)
	;
	    otherwise ->
	    append(Selected, [action=Action], Response)
	),
	maybe_postprocess_response(Response, FinalResponse),
	!.
process_nbest_recogniser_output(NBestList, InState, OutState, FinalResponse) :-
	call_if_debug_level_at_least(2, format('~N~nUnable to process N-best recogniser output:~n~n', [])),
	call_if_debug_level_at_least(2, prettyprint(NBestList)),
	call_if_debug_level_at_least(2, format('~n~n', [])),
	Response = [action=tts('Sorry, something went wrong')],
	maybe_postprocess_response(Response, FinalResponse),
	InState = OutState,
	!.

recognition_result_atom_to_selected_recognition_result_alist(Atom, Selected) :-
	(
%	  user:atom_to_original_script_and_gloss(Atom, OriginalScriptAtom, GlossAtom) ->
%	    (   fix_recognition_orthography_on_atom(Atom, CleanedUpAtom) ->
%		Selected = [selected=CleanedUpAtom, selected_original_script=OriginalScriptAtom, selected_gloss=GlossAtom]
%	    ;
%		otherwise ->
%		Selected = [selected=Atom, selected_original_script=OriginalScriptAtom, selected_gloss=GlossAtom]
%	    )
%	;
	    user:atom_to_original_script(Atom, OriginalScriptAtom) ->
	    Selected = [selected=OriginalScriptAtom]
	;
	    fix_recognition_orthography_on_atom(Atom, CleanedUpAtom) ->
	    Selected = [selected=CleanedUpAtom]
	;
	    otherwise ->
	    Selected = [selected=Atom]
	).

postprocessing_is_defined :-
	current_predicate(_Package:postprocess_dialogue_server_response/2),
	!.

maybe_postprocess_response(Response, FinalResponse) :-
	postprocessing_is_defined,
	!,
	(   user:postprocess_dialogue_server_response(Response, FinalResponse) ->
	    call_if_debug_level_at_least(5, format('~N~nPostprocessed output: ~w~n', [FinalResponse]))
	;
	    otherwise ->
	    format('~N*** Error: bad call: ~w~n', [user:postprocess_dialogue_server_response(Response, FinalResponse)]),
	    fail
	),
	!.
maybe_postprocess_response(Response, Response).

possibly_extract_sent_from_sent_and_lf(Sent, Sent) :-
	atom(Sent),
	!.
possibly_extract_sent_from_sent_and_lf(string_and_lf(RecStringAtom, _LF), RecStringAtom) :-
	!.
possibly_extract_sent_from_sent_and_lf(X, Y) :-
	format('~N*** Error: bad call: ~w~n~n',
	       [possibly_extract_sent_from_sent_and_lf(X, Y)]),
	fail.

get_most_recent_recorded_wavfile(_Action) :-
	\+ recognition_is_loaded,
	!,
	call_if_debug_level_at_least(2, format('~N~nRecognition resources are not loaded: unable to get most recent recorded wavfile~n~n', [])),
	fail.
get_most_recent_recorded_wavfile(AbsWavfile) :-
	regulus_sockettalk_get_parameter('client.FilenameRecorded', Wavfile),
	absolute_file_name(Wavfile, AbsWavfile),
	!.
get_most_recent_recorded_wavfile(AbsWavfile) :-
	format('~N*** Warning: bad call: ~w~n~n',
	       [get_most_recent_recorded_wavfile(AbsWavfile)]).

play_wavfile(_Play) :-
	\+ recognition_is_loaded,
	!,
	call_if_debug_level_at_least(2, format('~N~nRecognition resources are not loaded: unable to play wavfile~n~n', [])),
	fail.
play_wavfile(Wavfile) :-
	recognition_is_loaded,
	safe_absolute_file_name(Wavfile, AbsWavfile),
	regulus_sockettalk_say_list_atom(AbsWavfile),
	!.
play_wavfile(Wavfile) :-
	format('~N*** Warning: bad call: ~w~n~n',
	       [play_wavfile(Wavfile)]),
	fail.

perform_tts(_AtomOrString) :-
	( \+ recognition_is_loaded ; \+ tts_is_loaded ),
	!,
	call_if_debug_level_at_least(2, format('~N~nTTS resources are not loaded: unable to do TTS~n~n', [])),
	fail.
perform_tts(AtomOrString) :-
	(   atomic(AtomOrString) ->
	    atom_codes(AtomOrString, String)
	;
	    is_prolog_string(AtomOrString) ->
	    AtomOrString = String
	),
	recognition_is_loaded,
	tts_is_loaded,
	regulus_sockettalk_say_tts(String),
	!.
perform_tts(AtomOrString) :-
	format('~N*** Warning: bad call: ~w~n~n',
	       [perform_tts(AtomOrString)]),
	fail.

%---------------------------------------------------------------

try_to_get_debug_level(Action) :-
	get_dialogue_server_debug_level(DebugLevel) ->
	Action = debug_level(DebugLevel),
	!.	

try_to_set_debug_level(Level, Action) :-
	(   valid_debug_level(Level) ->
	    set_dialogue_server_debug_level(Level),
	    Action = ok
	;
	    otherwise ->
	    format('~N*** Warning: invalid debug level: ~w~n~n',
		   [Level]),
	    Action = error
	).
	

%---------------------------------------------------------------

try_to_restart_recognition_processes_in_state(Action, State) :-
	safe_check_user_is_administrator_in_state(State),
	restart_recognition_resources(Action),
	!.
try_to_restart_recognition_processes_in_state(Action, _State) :-
	Action = error.

try_to_recompile_system(InState, Action) :-
	user:regulus_config(recompile_tag, Tag),
	current_predicate(user:recompile_system/2),
	!,
	safe_call_saving_output((  set_current_dialogue_state(InState),
				   user:recompile_system(Tag, CompileStatus)
				),
				ExecuteStatus,
				OutputAtomList),
	(   ( ExecuteStatus = ok, CompileStatus = ok ) ->
	    Status = ok
	;
	    otherwise ->
	    Status = error
	),
	Action = recompiled(Status, OutputAtomList),
	!.
try_to_recompile_system(_InState, Action) :-
	Action = recompiled(action_undefined, []),
	!.

try_to_reload_system(InState, Action) :-
	user:regulus_config(reload_tag, Tag),
	current_predicate(user:reload_system/1),
	!,
	safe_call_saving_output((  set_current_dialogue_state(InState),
				   user:reload_system(Tag)
				),
				Status,
				OutputAtomList),
	Action = reloaded(Status, OutputAtomList),
	!.
try_to_reload_system(_InState, Action) :-
	Action = reloaded(action_undefined, []),
	!.

%---------------------------------------------------------------

recognise_and_process_recogniser_one_best_or_nbest_output(RecSource, InState, OutState, Action) :-
	recognise_and_process_recogniser_one_best_or_nbest_output(RecSource, default, InState, OutState, Action).

recognise_and_process_recogniser_one_best_or_nbest_output(_RecSource, _RecType, _InState, _OutState, _Action) :-
	\+ recognition_is_loaded,
	!,
	call_if_debug_level_at_least(2, format('~N~nRecognition resources are not loaded: unable to perform recognition~n~n', [])),
	fail.
recognise_and_process_recogniser_one_best_or_nbest_output(RecSource, RecType, InState, OutState, Action) :-
	(   RecSource = live ->
	    (   RecType = slm ->
		print_timing_info_if_bug_level_at_least(5, recognise_slm_as_defined_by_config_file(Recognised), recognition)
	    ;
		safe_get_recognition_grammar_from_state(InState, Grammar) ->
		print_timing_info_if_bug_level_at_least(5, recognise_as_defined_by_config_file(Grammar, Recognised), recognition)
	    ;
		otherwise ->
		print_timing_info_if_bug_level_at_least(5, recognise_as_defined_by_config_file(Recognised), recognition)
	    )
	;
	    RecSource = wavfile(Wavfile) ->
	    (   RecType = slm ->
		print_timing_info_if_bug_level_at_least(5, recognise_slm_from_wavfile_as_defined_by_config_file(Wavfile, Recognised), recognition)
	    ;
		safe_get_recognition_grammar_from_state(InState, Grammar) ->
		print_timing_info_if_bug_level_at_least(5, recognise_from_wavfile_as_defined_by_config_file(Grammar, Wavfile, Recognised), recognition)
	    ;
		otherwise ->
		print_timing_info_if_bug_level_at_least(5, recognise_from_wavfile_as_defined_by_config_file(Wavfile, Recognised), recognition)
	    )
	;
	    otherwise ->
	    call_if_debug_level_at_least(2, format('~N*** Error: unknown recognition source ~w in recognise_and_process_recogniser_one_best_or_nbest_output/4~n', [RecSource])),
	    fail
	),
	postprocess_recognition_result(Recognised, RecType, Recognised1),
	call_if_debug_level_at_least(2, format('~N~nProcessing recognition result:~n~n', [])),
	call_if_debug_level_at_least(2, prettyprint(Recognised1)),
	call_if_debug_level_at_least(2, format('~N~n', [])),
	(   Recognised1 = dummy ->
	    Action = [action=dummy],
	    InState = OutState
	;
	    RecType = slm ->
	    print_timing_info_if_bug_level_at_least(5,
						    dialogue_process_lf(Recognised1, InState, OutState, Record),
						    'dialogue processing (SLM recognition)'),
	    get_concrete_or_abstract_action_from_record(Record, Action0),
	    Action = [action=Action0],
	    call_if_debug_level_at_least(3, print_dialogue_processing_record(user, Record))
	;
	    otherwise ->
	    print_timing_info_if_bug_level_at_least(5,
						    recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, Action),
						    'dialogue processing (recognition)')
	    ),	    
	!.
recognise_and_process_recogniser_one_best_or_nbest_output(_RecSource, _RecType, InState, OutState, FinalResponse) :-
	call_if_debug_level_at_least(2, format('~N~nInternal error in recognition processing~n~n', [])),
	Response = [action=tts('Sorry, something went wrong')],
	maybe_postprocess_response(Response, FinalResponse),
	InState = OutState,
	!.

% Treat recognition failure as the text input 'nothing_recognised'
%recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, FinalResponse) :-
%	Recognised1 = recognition_failed,
%	Response = [action=tts('I couldn\'t recognise anything at all')],
%	maybe_postprocess_response(Response, FinalResponse),
%	InState = OutState,
%	!.
recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, Action) :-
	atom(Recognised1),
	process_recogniser_output(Recognised1, InState, OutState, Action),
	!.
recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, Action) :-
	is_list(Recognised1),
	process_nbest_recogniser_output(Recognised1, InState, OutState, Action),
	!.
recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, Action) :-
	format('~N*** Error: bad call: ~w~n',
	       [recognise_and_process_recogniser_one_best_or_nbest_output1(Recognised1, InState, OutState, Action)]),
	fail.

safe_get_recognition_grammar_from_state(State, Grammar) :-
	current_predicate(user:get_recognition_grammar_from_state/2),
	user:get_recognition_grammar_from_state(State, Grammar),
	atom(Grammar),
	!.

safe_check_user_is_administrator_in_state(State) :-
	current_predicate(user:check_user_is_administrator_in_state/1),
	user:check_user_is_administrator_in_state(State),
	!.

% Help response is of the form help(<String>)
% where <String> consists of the help responses joined together with newline characters.
process_help_request(Sent, NExamples, Response) :-
	get_help_matches(Sent, NExamples, Matches),
	package_matches_into_string(Matches, ""-MatchesString),
	atom_codes(MatchesAtom, MatchesString),
	Response = help(MatchesAtom).

process_return_to_initial_dialogue(_InState, OutState, Response) :-
	initial_dialogue_state(OutState),
	Response = ok,
	!.
process_return_to_initial_dialogue(InState, OutState, Response) :-
	OutState = InState,
	Response = error,
	!.

%===========================================================

postprocess_recognition_result(Rec, RecType, Rec2) :-
	postprocess_recognition_result1(Rec, Rec1),
	postprocess_recognition_result2(Rec1, RecType, Rec2),
	!.
postprocess_recognition_result(Rec, RecType, Rec1) :-
	format('~N*** Error: bad call: ~w~n', [postprocess_recognition_result(Rec, RecType, Rec1)]),
	fail.

postprocess_recognition_result1(dummy, dummy) :-
	!.
postprocess_recognition_result1(recognition_failed(_), nothing_recognised) :-
	!.
postprocess_recognition_result1(recognition_succeeded(_Conf, Atom, _LF), Atom) :-
	atom(Atom),
	!.
postprocess_recognition_result1(recognition_succeeded(List), List1) :-
	is_list(List),
	format_nbest_list_for_dialogue_server(List, List1),
	!.

postprocess_recognition_result2(dummy, _Type, dummy) :-
	!.
postprocess_recognition_result2(Rec, default, Rec) :-
	!.
postprocess_recognition_result2(SingleHyp, slm, slm_result(SingleHyp)) :-
	atom(SingleHyp).
postprocess_recognition_result2(List, slm, slm_result(TopHyp)) :-
	is_list(List),
	List = [nbest_hyp(_Confidence, TopHyp) | _Rest],
	!.

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

%===========================================================

package_matches_into_string([], String-String) :-
	!.
package_matches_into_string([Match], StringIn-StringOut) :-
	atom_codes(Match, String),
	append(StringIn, String, StringOut),
	!.
package_matches_into_string([F | R], StringIn-StringOut) :-
	atom_codes(F, String),
	append_list([StringIn, String, "\n"], StringNext),
	!,
	package_matches_into_string(R, StringNext-StringOut).

%===========================================================

start_new_dialogue_session_logfile(LogfileDir) :-
	start_new_dialogue_session_logfile(LogfileDir, _Logfile).

start_new_dialogue_session_logfile(LogfileDir, Logfile) :-
	start_new_prolog_format_logfile(LogfileDir, dialogue_event, Logfile).

log_dialogue_string_message(Mode, Wrapper, String) :-
	log_string_event(Mode, Wrapper, String).
log_dialogue_event(Term) :-
	log_event_in_prolog_format(Term, dialogue_event).

%===========================================================

:- dynamic message_mode/1.

set_message_mode(Mode) :-
	valid_message_mode(Mode),
	retractall(message_mode(_)),
	assertz(message_mode(Mode)),
	!.
set_message_mode(Mode) :-
	format('~N*** Error: bad call: ~w~n',
	       [set_message_mode(Mode)]),
	fail.

get_message_mode(Mode) :-
	(   message_mode(Mode0) ->
	    Mode = Mode0
	;
	    Mode = prolog
	).

valid_message_mode(xml).
valid_message_mode(json).
valid_message_mode(pure_json).
valid_message_mode(prolog).

%===========================================================

:- dynamic saved_dialogue_state/2.
:- dynamic current_sessionid/1.

init_saved_dialogue_state :-
	retractall(saved_dialogue_state(_, _)),
	retractall(current_sessionid(_)),
	format('--- Initialised saved dialogue state table~n', []).

save_dialogue_state(SessionId, State) :-
	retractall(saved_dialogue_state(SessionId, _)),
	assertz(saved_dialogue_state(SessionId, State)),
	!.

get_dialogue_state(SessionId, State) :-
	saved_dialogue_state(SessionId, State),
	!.
get_dialogue_state(_SessionId, State) :-
	saved_dialogue_state(default, State),
	!.
get_dialogue_state(SessionId, State) :-
	format('~N*** Error: bad call: ~w~n', [get_dialogue_state(SessionId, State)]),
	fail.

get_current_session(SessionId) :-
	current_sessionid(SessionId),
	!.
get_current_session(SessionId) :-
	SessionId = default_session.

set_current_session(SessionId) :-
	retractall(current_sessionid(_)),
	assertz(current_sessionid(SessionId)),
	!.
set_current_session(SessionId) :-
	format('~N*** Error: bad call: ~w~n', [set_current_session(SessionId)]),
	fail.

%===========================================================

:- dynamic action_mode/1.

set_action_mode(Mode) :-
	valid_action_mode(Mode),
	retractall(action_mode(_)),
	assertz(action_mode(Mode)),
	!.
set_action_mode(Mode) :-
	format('~N*** Error: bad call: ~w~n',
	       [set_action_mode(Mode)]),
	fail.

get_action_mode(Mode) :-
	(   action_mode(Mode0) ->
	    Mode = Mode0
	;
	    Mode = concrete
	).

valid_action_mode(concrete).
valid_action_mode(abstract).

%----------------------------------------------------------------------------------

get_concrete_or_abstract_action_from_record(Record, Action) :-
	get_action_mode(concrete),
	member(action=[Action, _Judgement], Record),
	!.
get_concrete_or_abstract_action_from_record(Record, Action) :-
	get_action_mode(abstract),
	member(abstract_action_and_out_state=[Action, _OutState, _Judgement], Record),
	!.
%----------------------------------------------------------------------------------

:- dynamic last_server_restart_tag/1.

set_last_server_restart_tag(RestartTag) :-
	retractall(last_server_restart_tag(_)),
	assertz(last_server_restart_tag(RestartTag)),
	!.

get_last_server_restart_tag(RestartTag) :-
	last_server_restart_tag(RestartTag),
	!.

%----------------------------------------------------------------------------------

print_long_string :-
	append_n_copies(25, ['"a"'], X),
	append_n_copies(100, [X], Y),
	format('~N', []),
	prettyprint(Y),
	format('~N', []).

%

