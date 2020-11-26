
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(translate_bidirectional,
	  [is_in_bidirectional_mode/0,
	   set_bidirectional_mode/0,
	   unset_bidirectional_mode/0,

	   start_bidirectional_mode/0,
	   exit_bidirectional_mode/0,
	   exit_bidirectional_mode_if_necessary/0,

	   process_regulus_loop_item_string_remote/2,
	   set_remote_discourse_context_from_target_representation/3,
	   
	   pre_process_string_wrt_bidirectional_processing/3]).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/analysis_constraints').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

:- use_module('$REGULUS/PrologLib/utilities').
:- use_module('$REGULUS/RegulusLanguageServer/Prolog/remote_regulus').

%----------------------------------------------------------------------

port_for_remote_regulus(4327).

%----------------------------------------------------------------------

:- dynamic bidirectional_mode/0.

is_in_bidirectional_mode :-
	bidirectional_mode,
	!.

set_bidirectional_mode :-
	bidirectional_mode,
	!.
set_bidirectional_mode :-
	assertz(bidirectional_mode).

unset_bidirectional_mode :-
	retractall(bidirectional_mode).

%----------------------------------------------------------------------

:- dynamic remote_cfg_file/1.

get_remote_cfg_file(CFGFile) :-
	remote_cfg_file(CFGFile),
	!.

set_remote_regulus_config_file_from_local_config_info(AbsCFGFile) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(answer_config_file, CFGFile),
	safe_absolute_file_name(CFGFile, AbsCFGFile),
	file_exists(AbsCFGFile),
	set_remote_cfg_file(AbsCFGFile),
	!.
set_remote_regulus_config_file_from_local_config_info(_CFGFile) :-
	\+ current_predicate(user:regulus_config/2),
	!,
	format2error('~N*** Error: no config file loaded~n', []),
	fail.
set_remote_regulus_config_file_from_local_config_info :-
	\+ user:regulus_config(answer_config_file, _CFGFile),
	!,
	format2error('~N*** Error: need to define answer_config_file in loaded config file~n', []),
	fail.
set_remote_regulus_config_file_from_local_config_info :-
	user:regulus_config(answer_config_file, CFGFile),
	safe_absolute_file_name(CFGFile, AbsCFGFile),
	format2error('~N*** Error: can\'t find ~w~n', [AbsCFGFile]),
	fail,
	!.

set_remote_cfg_file(CFGFile) :-
	retractall(remote_cfg_file(_)),
	assertz(remote_cfg_file(CFGFile)),
	!.

%----------------------------------------------------------------------

/*

  - Starts remote Regulus language server using config file defined
    by answer_config_file.
  - Calls set_bidirectional_mode

*/

start_bidirectional_mode :-
	is_in_bidirectional_mode,
	format('~NAlready in bidirectional mode, nothing to do~n', []),
	!.
start_bidirectional_mode :-
	set_remote_regulus_config_file_from_local_config_info(CFGFile),
	port_for_remote_regulus(Port),
	format('~NSTARTING REGULUS LANGUAGE SERVER ON PORT ~d~n~n', [Port]),
	remote_regulus_init(Port),
	remote_regulus_call(user:regulus_batch(CFGFile, [])),
	set_bidirectional_mode,
	!.
start_bidirectional_mode :-
	format2error('~N*** Error: unable to start bidirectional mode~n', []),
	fail.
	
%----------------------------------------------------------------------

/*

  - Closes down the remote server
  - Calls unset_bidirectional_mode

*/

exit_bidirectional_mode_if_necessary :-
	is_in_bidirectional_mode,
	!,
	exit_bidirectional_mode.
exit_bidirectional_mode_if_necessary.

exit_bidirectional_mode :-
	\+ is_in_bidirectional_mode,
	format('~NNot in bidirectional mode, nothing to do~n', []),
	!.
exit_bidirectional_mode :-
	format('~NCLOSING DOWN REGULUS LANGUAGE SERVER~n~n', []),
	remote_regulus_exit_server,
	unset_bidirectional_mode,
	!.
exit_bidirectional_mode :-
	format2error('~NError: unable to exit bidirectional mode~n', []),
	fail.

%----------------------------------------------------------------------

/*

  - Executes Regulus command InString remotely, returning all text output as OutString

*/

process_regulus_loop_item_string_remote(InString, OutString) :-
	is_prolog_string(InString),
	remote_regulus_call(user:process_regulus_loop_item_with_output_to_string(InString, OutString)),
	!.
process_regulus_loop_item_string_remote(InString, OutString) :-
	(   is_prolog_string(InString) ->
	    format_to_chars('~N*** Error: remote regulus command "~s" failed~n',
			    [InString],
			    OutString) ;
	    format_to_chars('~N*** Error: argument to remote regulus call, ~w, not a string~n',
			    [InString],
			    OutString)
	).

%----------------------------------------------------------------------

set_remote_discourse_context_from_target_representation(TargetAtom, TargetRepresentation, OutputString) :-
	remote_regulus_call(user:set_current_discourse_context_from_target_representation(TargetAtom, TargetRepresentation, OutputString)),
	!.		    

%----------------------------------------------------------------------

pre_process_string_wrt_bidirectional_processing(String, String1, LocalOrRemote) :-
	on_exception(
	Exception, 
	pre_process_string_wrt_bidirectional_processing1(String, String1, LocalOrRemote),
	( handle_pre_process_string_wrt_bidirectional_processing_error(Exception) )
    ),
	!.
pre_process_string_wrt_bidirectional_processing(_String, _String1, _LocalOrRemote) :-
	format2error('~N*** Error in pre_process_string_wrt_bidirectional_processing/3~n', []),
	fail.

pre_process_string_wrt_bidirectional_processing1(String, String1, LocalOrRemote) :-
	\+ is_in_bidirectional_mode,
	String1 = String,
	LocalOrRemote = local,
	!.
pre_process_string_wrt_bidirectional_processing1(String, String1, LocalOrRemote) :-
	split_string_into_words(String, Words),
	Words = [FirstWord | RestWords],
	(   FirstWord = 'Q:' ->
	    LocalOrRemote = local
	;

	    FirstWord = 'A:' ->
	    LocalOrRemote = remote
	;

	    format2error('~N*** Error: in bidirectional mode, all commands must be prefaced by "Q:" or "A:"~n', []),
	    fail
	),
	join_with_spaces(RestWords, RestAtom),
	atom_codes(RestAtom, String1),
	!.

handle_pre_process_string_wrt_bidirectional_processing_error(Exception) :-
	format2error('~N*** Error in pre_process_string_wrt_bidirectional_processing/3: ~w~n', [Exception]),
	fail.

