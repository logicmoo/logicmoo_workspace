:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(check_dialogue_server_messages,
	[check_client_message/1,
	 check_server_message/1]
    ).

/*

Defines predicates for checking well-formedness of Prolog-format
messages sent to and from dialogue server.

check_client_message/1 checks well-formedness of message sent from client.

check_server_message/1 checks well-formedness of reply message sent
from server.

Usually, another file will be loaded which defines the following predicates

client_action:client_message/1, used to define well-formedness of messages sent from the client.
client_action:client_action/1, used to define well-formedness of actions sent to client.

An example can be found in $CALLSLT/Int/Prolog/client_action.pl

*/

%======================================================================

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%===========================================================

check_client_message(ClientRequest) :-
	on_exception(
	Exception, 
	client_message(ClientRequest),
	(   format('~N~n*** Warning: processing aborted in client_message/1 due to internal exception ***~n~n', []),
	    print_message(error, Exception)
	)
    ),
	!.
check_client_message(ClientRequest) :-
	format('~N*** Warning: ill-formed client request: ~q~n~n', [ClientRequest]).

check_server_message(ServerReply) :-
	on_exception(
	Exception, 
	server_message(ServerReply),
	(   format('~N~n*** Warning: processing aborted in server_message/1 due to internal exception ***~n~n', []),
	    print_message(error, Exception)
	)
		    ),
	!.
check_server_message(ServerReply) :-
	format('~N*** Warning: ill-formed server reply: ~q~n~n', [ServerReply]).

%----------------------------------------------------------------------------------

% Messages for setting up and closing down server/client connection
client_message(client(_ClientHost)).
client_message(end_of_file).
client_message(disconnect).
client_message(client_shutdown).

% Re-initialise the dialogue server.

client_message(restart_server(Tag)) :-
	atom(Tag).

client_message(restart_server(CFGFile, CommandsAsAtoms, InitParameter)) :-
	cfg_file(CFGFile),
	command_list(CommandsAsAtoms),
	init_parameter(InitParameter).

% Get last reinitialisation tag
client_message(last_restart_server_tag) :-
	!.

% Get current message mode
% Possible responses: message_mode({prolog,xml,json,pure_json}), error
client_message(get_message_mode).

% Normal message. Responses will be application-dependent.
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
client_message(action(Action)) :-
	action(Action).
client_message(action_with_no_reply(Action)) :-
	action(Action).

client_message(action_for_user(UserId, Action)) :-
	userid(UserId),
	action(Action).
client_message(action_for_session(SessionId, Action)) :-
	sessionid(SessionId),
	action(Action).
client_message(action_for_user_with_no_reply(UserId, Action)) :-
	userid(UserId),
	action(Action).
client_message(action_for_session_with_no_reply(SessionId, Action)) :-
	sessionid(SessionId),
	action(Action).

action(ActionSequence) :-
	compound(ActionSequence),
	ActionSequence =.. [action_sequence | List],
	action_list(List).

% Use XML-format messages.
% Possible responses: ok, error
action(xml_messages).

% Use JSON-format messages.
% Possible responses: ok, error
action(json_messages).
action(pure_json_messages).

% Use Prolog-format messages.
% Possible responses: ok, error
action(prolog_messages).

% Pass concrete actions from server to client.
% Possible responses: ok, error
action(concrete_actions).

% Pass abstract actions from server to client.
% Possible responses: ok, error
action(abstract_actions).

% Restart rec processes
% Possible responses: error, restarted_regserver_on_port(RegserverPort), restarted_all_recognition_resources_on_port(RegserverPort)
action(restart_recognition_processes).

% Get/set debug level
action(get_debug_level).

action(set_debug_level(Level)) :-
	integer(Level).

% Get grammar
action(get_current_recognition_grammar).

% Recompile app
% Possible responses: error, recompiled(Status, OutputAtomList)
action(recompile_system).

% Reload app
% Possible responses: error, reloaded(Status, OutputAtomList)
action(reload_system).

% Process an XML-formatted N-best recognition result.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_xml_message(XMLAtom)) :-
	atom(XMLAtom).

% Process a Prolog-formatted N-best recognition result with wavfile.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_nbest_list(_Wavfile, NBestList)) :-
	nbest_list(NBestList).

% Process a Prolog-formatted N-best recognition result without wavfile.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_nbest_list(NBestList)) :-
	nbest_list(NBestList).

% Process a Prolog-formatted 1-best recognition result with wavfile.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_rec_string(_Wavfile, Words)) :-
	atom(Words).

% Process a Prolog-formatted 1-best recognition result without wavfile.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_rec_string(Words)) :-
	atom(Words).

% Robust process a Prolog-formatted 1-best recognition result without wavfile.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(robust_process_string(Words)) :-
	atom(Words).

% Perform recognition on server side, and process recognition result.
% Possible responses: Rec response, tts(ErrorAtom)
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(recognise_and_dialogue_process).
action(recognise_and_dialogue_process_from_wavfile(Wavfile)) :-
	atom(Wavfile).

% Return the most recent recorded wavfile.
% Possible responses: WavfileAtom, error
action(get_most_recent_recorded_wavfile).

% Play the specified wavfile.
% Possible responses: ok, error
action(play_wavfile(Wavfile)) :-
	atom(Wavfile).

% Do TTS on string encoded as Prolog atom.
% Possible responses: ok, error
action(tts(Atom)) :-
	atom(Atom).

% Process a non-speech input (old style). The atom should be of the form 'LF: <PrologTerm>'
action(process_non_speech_input(Atom)) :-
	atom(Atom).

% Process a non-speech input (new style). The message should be defined by client_action:client_non_speech_input/1
% Responses will be application-dependent.
% For CALLSLT, see $CALLSLT/Int/Prolog/client_actions.pl
action(process_non_speech_input(Message)) :-
	(   current_predicate(client_action:client_non_speech_input/1) ->
	    client_action:client_non_speech_input(Message)
	;
	    otherwise ->
	    true
	).

% Get <NExamples> help examples for input <Words>
% Possible responses: N-best list, error
action(get_help_examples(Words, NExamples)) :-
	atom(Words),
	number(NExamples).

% Return the dialogue state to the initial state.
% Possible responses: ok, error
action(return_to_initial_context).

% Revert the dialogue state to the preceding one.
% Possible responses: ok, error
action(revert_discourse_context).

% Set the notional time
action(set_notional_time(Timestamp)) :-
	atom(Timestamp).

% Execute a top-level Regulus command, formatted as a string.
% Possible responses: ok, error
action(execute_regulus_command(CommandString)) :-
	is_prolog_string(CommandString).

% Add message to logfiles
% Possible responses: ok, error
action(log(_ArbitraryStructure)).

action_list([]).
action_list([F | R]) :-
	action(F),
	!,
	action_list(R).

% An N-best list is list of elements of the form
%
%    nbest_hyp(Confidence, Words)

nbest_list([]).
nbest_list([F | R]) :-
	nbest_list_element(F),
	!,
	nbest_list(R).

nbest_list_element(nbest_hyp(Confidence, Words)) :-
	number(Confidence),
	atom(Words).

%----------------------------------------------------------------------------------

server_message(ActionSequence) :-
	ActionSequence =.. [action_sequence | List],
	server_message_list(List).

% Messages for setting up and closing down server/client connection
server_message(accept(_ClientHost)).
server_message(disconnect).
server_message(thread_shutdown).

% Command succeeded.
server_message(ok).

% Command failed.
server_message(error).

% Response to last_restart_server_tag
server_message(message_mode(MessageMode)) :-
	atom(MessageMode),
	message_mode(MessageMode).
	
% Response to last_restart_server_tag
server_message(restart_tag(RestartTag)) :-
	atom(RestartTag).

% Responses to restart_recognition_processes
server_message(restarted_regserver_on_port(RegserverPort)) :-
	number(RegserverPort).
server_message(restarted_all_recognition_resources_on_port(RegserverPort)) :-
	number(RegserverPort).

% Response to get_current_recognition_grammar
server_message(grammar(Grammar)) :-
	atomic(Grammar).

% Response to get_debug_level
server_message(debug_level(Level)) :-
	integer(Level).
server_message(debug_level(failed)).

% Response to recompile_system
server_message(recompiled(Status, OutputAtomList)) :-
	recompile_or_reload_status(Status),
	output_trace(OutputAtomList).

% Response to reload_system
server_message(reloaded(Status, OutputAtomList)) :-
	recompile_or_reload_status(Status),
	output_trace(OutputAtomList).

% Response to recognition request or non-speech input
server_message(RecResponse) :-
	rec_response(RecResponse).

% Response to request for last recorded wavfile
server_message(Wavfile) :-
	atom(Wavfile).

% Response to request for help info
% Help response is of the form help(<String>)
% where <String> consists of the help responses joined together with newline characters.
server_message(help(HelpResponse)) :-
	atom(HelpResponse).

server_message_list([]).
server_message_list([F | R]) :-
	server_message(F),
	!,
	server_message_list(R).

recompile_or_reload_status(ok).
recompile_or_reload_status(error).
recompile_or_reload_status(action_undefined).

output_trace([]).
output_trace([F | R]) :-
	atom(F),
	!,
	output_trace(R).

% Rec response is a list of components of one of the forms
%
%   selected=Atom
%   selected_original_script=Atom
%   selected_gloss=Atom
%   action=Action 
%   paraphrase=Paraphrase
%
% Action is defined by the application-specific predicate client_action:client_action/1

% Alternately, if we have post-processing, the whole response can be defined by client_action:client_action/1

rec_response(Action) :-
	current_predicate(client_action:client_action/1),
	client_action:client_action(Action).
rec_response([]).
rec_response([F | R]) :-
	rec_response_component(F),
	rec_response(R).

rec_response_component(selected=Atom) :-
	atom(Atom).
rec_response_component(selected_original_script=Atom) :-
	atom(Atom).
rec_response_component(selected_gloss=Atom) :-
	atom(Atom).
rec_response_component(action=Action) :-
	client_action(Action).
rec_response_component(paraphrase=Paraphrase) :-
	atom(Paraphrase).
rec_response_component(robust_match_score=RobustMatchScore) :-
	number(RobustMatchScore).

client_action(ok).
client_action(error).
client_action(tts(Atom)) :-
	atom(Atom).
client_action(Action) :-
	(   current_predicate(client_action:client_action/1) ->
	    client_action:client_action(Action)
	;
	    otherwise ->
	    true
	).

message_mode(prolog).
message_mode(xml).
message_mode(json).
message_mode(pure_json).

userid(UserId) :-
	atomic(UserId).

sessionid(SessionId) :-
	atomic(SessionId).

cfg_file(CFGFile) :-
	atom(CFGFile).

command_list([]).
command_list([F | R]) :-
	command(F),
	command_list(R).

command(Command) :-
	atom(Command).

% Any Prolog term is OK for now.
init_parameter(_InitParameter).
	
