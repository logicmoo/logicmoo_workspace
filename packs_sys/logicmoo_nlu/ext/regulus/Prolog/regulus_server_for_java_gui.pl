%%% Adapted from "evaluate.pl" example in PrologBeans directory

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(prologbeans)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).
:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

start_regulus_server_for_java_gui:-
	prolog_flag(argv, Args),
	(
	  ( Args = [PortAtom], atom_to_int(PortAtom, Port) ) ->
	  format(user_error, '~N~nInitiating Regulus server: port = ~d~n~n', [Port]),
	  flush_output(user_error)
	;
	  otherwise ->
	  format(user_error, "~n Usage: sicstus -l regulus_server_for_java_gui.pl <Port>~n", []),
	  halt
	),
	start_regulus_server_for_java_gui(Port).

%% Load Regulus, mark as using Java GUI,
%% register acceptable queries and start the server 
start_regulus_server_for_java_gui(Port) :-

	['$REGULUS/Prolog/load'],

	assertz(using_java_gui),

	register_query(get_config_file_alist(AList),
		       get_config_file_alist_for_java_gui(AList)),

	register_query(start_regulus(CFGFileString, OutChars),
		       load_regulus_config_file_string(CFGFileString, OutChars)),

	register_query(end_regulus_session,
		       handle_regulus_exit),

	register_query(get_regulus_command_status(PosPackage, NegPackage),
		       get_regulus_commands_available_for_java_gui(PosPackage, NegPackage)),
		       
	register_query(start_regulus_stepper(OutChars),
		       init_stepper_loop_with_output_to_string(OutChars)),

	register_query(execute_regulus_command(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorString),
		       process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorString)),

	register_query(remote_execute_regulus_command(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorString),
		       remote_process_regulus_loop_item_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, OutPromptChars, Status, ErrorString)),

	register_query(execute_stepper_command(InChars, CommentChars, AnswerChars, OutChars, Status, ChangeInfo, OutPromptChars, ErrorString),
		       process_stepper_command_for_java_gui(InChars, CommentChars, AnswerChars, OutChars, Status, ChangeInfo, OutPromptChars, ErrorString)),

	register_query(perform_translation(InChars, AnswerTerm),
		       parse_transfer_and_generate_for_java_gui(InChars, AnswerTerm)),

	register_query(remote_perform_translation(InChars, AnswerTerm),
		       remote_parse_transfer_and_generate_for_java_gui(InChars, AnswerTerm)),
	
	register_query(perform_dialogue_processing(InChars, AnswerTerm),
		       dialogue_process_utterance_for_java_gui(InChars, AnswerTerm)),

	register_query(read_translation_output_file(IdChars, AnswerTerm),
		       read_translation_output_file_for_java_gui(IdChars, AnswerTerm)),

	register_query(read_speech_translation_output_file(IdChars, AnswerTerm),
		       read_speech_translation_output_file_for_java_gui(IdChars, AnswerTerm)),

	register_query(store_judged_translation_output_file(IdChars, JudgedFileString, Result),
		       store_judged_translation_output_file_for_java_gui(IdChars, JudgedFileString, Result)),

	register_query(store_judged_speech_translation_output_file(IdChars, JudgedFileString, Result),
		       store_judged_speech_translation_output_file_for_java_gui(IdChars, JudgedFileString, Result)),

	register_query(get_stepper_summary(Comment, SummaryTerm),
		       get_stepper_summary_for_java_gui(Comment, SummaryTerm)),

	register_query(show_item(Comment, ItemId, PackagedItem),
		       show_item_for_java_gui(Comment, ItemId, PackagedItem)),

	register_query(show_item(Comment, ItemId, NodeID, PackagedItem),
		       show_item_for_java_gui(Comment, ItemId, NodeID, PackagedItem)),

	register_query(show_rule_for_item(Comment, ItemID, NodeID, RuleString),
		       get_rule_string_for_item_for_java_gui(Comment, ItemID, NodeID, RuleString)),

	register_query(check_regulus_status(Comment, Type, YesNo),
		       check_regulus_status_for_java_gui(Type, YesNo)),

	register_query(get_regulus_file(KeyString, File, InputOrOutput),
		       get_regulus_file_for_java_gui(KeyString, File, InputOrOutput)),

	register_query(get_progress_file_info(CommandString, InfoTerm),
		       get_progress_file_info_for_java_gui(CommandString, InfoTerm)),

	register_query(delete_file(FileString, Status),
		       delete_file_with_status_for_java_gui(FileString, Status)),

	register_query(do_speech_recognition(RecognisedString),
		       recognise_for_gui_as_defined_by_config_file(RecognisedString)),

	register_query(do_speech_recognition_from_wavfile(WavfileString, RecognisedString),
		       recognise_for_gui_from_wavfile_as_defined_by_config_file(WavfileString, RecognisedString)),

	register_query(get_most_recent_recorded_wavfiles(N, WavfilesItem),
		       recorded_wavfile_list_for_gui(N, WavfilesItem)),

	format(user_error, '~N~nRegulus loaded in server~n', []),
	flush_output(user_error),

	log_event('CONNECT_TO_GUI', []),

	start([port(Port)]).

% This will be called if we build a run-time system
user:runtime_entry(start) :-
	start_regulus_server_for_java_gui.
 