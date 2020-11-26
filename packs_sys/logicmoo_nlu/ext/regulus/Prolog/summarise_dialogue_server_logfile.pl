:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(summarise_dialogue_server_logfile,
	[test_summarise_dialogue/1,
	 summarise_dialogue_server_logfile/2,
	 summarise_dialogue_server_logfile/3
	]).

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(system)) ),
	      ( :- use_module(library(system3), [datime/1, datime/2] ) ) ).

%---------------------------------------------------------------

test_summarise_dialogue(french) :-
	summarise_dialogue_server_logfile('$CALLSLT/Eval/SlowProcessing/logfiles/dialogue_server_log_2011-10-10_19-41-23.pl',
					  %'UTF-16LE',
					  '$CALLSLT/Eval/SlowProcessing/logfiles/french_summarised.pl').

test_summarise_dialogue(german) :-
	summarise_dialogue_server_logfile('$CALLSLT/Eval/SlowProcessing/logfiles/dialogue_server_log_2011-10-12_18-24-08.pl',
					  %'UTF-16LE',
					  '$CALLSLT/Eval/SlowProcessing/logfiles/german_summarised.pl').

test_summarise_dialogue(japanese) :-
	summarise_dialogue_server_logfile('$CALLSLT/Eval/SlowProcessing/logfiles/dialogue_server_log_japanese.pl',
					  %'UTF-16LE',
					  '$CALLSLT/Eval/SlowProcessing/logfiles/japanese_summarised.pl').

%---------------------------------------------------------------
 
summarise_dialogue_server_logfile(InFile, OutFile) :-
	summarise_dialogue_server_logfile(InFile, default_encoding, OutFile).

summarise_dialogue_server_logfile(InFile, Encoding, OutFile) :-
	init_readable_session_id_table,
	safe_prolog_file_to_list_printing_statistics(InFile, InList, Encoding),
	summarise_dialogue_server_list(InList, OutList, no_time, waiting_for_input),
	safe_list_to_prolog_file_printing_statistics(OutList, OutFile).

summarise_dialogue_server_list([], [], _TimeIn, _StateIn) :-
	!.
summarise_dialogue_server_list([F | R], [F1 | R1], TimeIn, StateIn) :-
	summarise_dialogue_server_item(F, F1, TimeIn-TimeNext, StateIn-StateNext),
	!,
	summarise_dialogue_server_list(R, R1, TimeNext, StateNext).

summarise_dialogue_server_item(dialogue_event(TimeOut, received(Action)),
			       no_message_received_for(TimeDiff),
			       TimeIn-TimeOut,
			       waiting_for_input-waiting_for_output(TimeOut, SummarisedAction)) :-
	time_stamp_diff(TimeIn, TimeOut, TimeDiff),
	try_to_summarise_action(Action, SummarisedAction),
	!.
summarise_dialogue_server_item(dialogue_event(TimeOut, received(Action)),
			       action_and_no_logged_response(start_time:TimeIn, PreviousSummarisedAction, response_time:TimeDiff),
			       TimeIn-TimeOut,
			       waiting_for_output(TimeIn, PreviousSummarisedAction)-waiting_for_output(TimeOut, SummarisedAction)) :-
	time_stamp_diff(TimeIn, TimeOut, TimeDiff),
	try_to_summarise_action(Action, SummarisedAction),
	!.
summarise_dialogue_server_item(dialogue_event(TimeOut, response(_Response)),
			       action_and_response(start_time:TimeIn, SummarisedAction, response_time:TimeDiff),
			       TimeIn-TimeOut,
			       waiting_for_output(TimeIn, SummarisedAction)-waiting_for_input) :-
	time_stamp_diff(TimeIn, TimeOut, TimeDiff),
	!.
summarise_dialogue_server_item(F, F1, Time, State) :-
	format('~N*** Error: bad call: ~w~n', [summarise_dialogue_server_item(F, F1, Time, State)]),
	fail.

time_stamp_diff(no_time, TimeOut, TimeOut) :-
	!.
time_stamp_diff(TimeIn, TimeOut, TimeDiff) :-
	time_stamp_to_number(TimeIn, TimeInN),
	time_stamp_to_number(TimeOut, TimeOutN),
	TimeDiffN is TimeOutN - TimeInN,
	time_stamp_diff_readable(TimeDiffN, TimeDiff),
	!.
time_stamp_diff(TimeIn, TimeOut, TimeDiff) :-
	format('~N*** Error: bad call: ~w~n', [time_stamp_diff(TimeIn, TimeOut, TimeDiff)]),
	fail.

time_stamp_to_number(TimeStamp, N) :-
	atom(TimeStamp),
	atom_codes(TimeStamp, Str),
	parse_datime(Str, Datime),
	datime(N, Datime),
	!.

time_stamp_diff_readable(TimeDiffN, TimeDiff) :-
	number(TimeDiffN),
	TimeDiffN < 60,
	!,
	format_to_atom('~d seconds', [TimeDiffN], TimeDiff).
time_stamp_diff_readable(TimeDiffN, TimeDiff) :-
	number(TimeDiffN),
	Mins is TimeDiffN / 60,
	!,
	format_to_atom('~1f minutes', [Mins], TimeDiff).
time_stamp_diff_readable(Other, Other).


try_to_summarise_action(Action, SummarisedAction) :-
	summarise_action(Action, SummarisedAction),
	!.
try_to_summarise_action(_Action, SummarisedAction) :-
	SummarisedAction = action.

:- dynamic readable_session_id/2.

init_readable_session_id_table :-
	retractall(readable_session_id(_, _)),
	!.

get_readable_session_id(SessionId, SessionIdReadable) :-
	readable_session_id(SessionId, SessionIdReadable),
	!.
get_readable_session_id(SessionId, SessionIdReadable) :-
	get_readable_session_id1(SessionId, SessionIdReadable, 1).

get_readable_session_id1(SessionId, SessionIdReadable, I) :-
	format_to_atom('session_~d', [I], SessionIdReadable),
	\+ readable_session_id(_AnySessionId, SessionIdReadable),
	assertz(readable_session_id(SessionId, SessionIdReadable)),
	!.
get_readable_session_id1(SessionId, SessionIdReadable, I) :-
	I1 is I + 1,
	!,
	get_readable_session_id1(SessionId, SessionIdReadable, I1).

%---------------------------------------------------------------

summarise_action(action_for_session(SessionId, Action),
		 action_for_session(SessionIdReadable, SummarisedAction)) :-
	!,
	get_readable_session_id(SessionId, SessionIdReadable),
	summarise_action(Action, SummarisedAction).
summarise_action(process_non_speech_input(Action), SummarisedAction) :-
	!,
	summarise_action(Action, SummarisedAction).
summarise_action(action_sequence(A, _B), SummarisedAction) :-
	!,
	summarise_action(A, SummarisedAction).
summarise_action(action_sequence(A, _B, _C), SummarisedAction) :-
	!,
	summarise_action(A, SummarisedAction).
summarise_action(recognise_and_dialogue_process_from_wavfile(_), recognise_and_dialogue_process_from_wavfile) :-
	!.
summarise_action(X, X).
