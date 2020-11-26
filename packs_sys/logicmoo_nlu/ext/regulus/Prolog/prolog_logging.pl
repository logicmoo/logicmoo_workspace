
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(prolog_logging,
	  [
	   start_new_prolog_format_logfile/2,
	   start_new_prolog_format_logfile/3,
	   log_event_in_prolog_format/2,
	   log_string_event/3
	  ]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%===========================================================

start_new_prolog_format_logfile(LogfileDir, RecordPred) :-
	start_new_prolog_format_logfile(LogfileDir, RecordPred, _File).

start_new_prolog_format_logfile(LogfileDir, RecordPred, File) :-
	check_record_pred_is_ok(RecordPred),
	get_new_logfile_name(LogfileDir, File),
	set_current_logfile(File),
	format('~N~nStarted logging on ~w~n', [File]),
	% Do a normal 'write' first in case it's a Unicode file -
	% otherwise we don't get the right header.
	open_regulus_file(File, write, S),
	format(S, '~n', []),
	close(S),
	log_event_in_prolog_format(start_session, RecordPred),
	!.
start_new_prolog_format_logfile(LogfileDir, RecordPred, File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [start_new_prolog_format_logfile(LogfileDir, RecordPred, File)]),
	fail.

%-----------------------------------------------------------

log_event_in_prolog_format(Term, RecordPred) :-
	check_record_pred_is_ok(RecordPred),
	(   get_current_logfile(File) ->
	    log_event_to_logfile(Term, RecordPred, File)
	;
	    true
	).

log_event_to_logfile(Term, RecordPred, File) :-
	open_regulus_file(File, append, S),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format(S, '~N~n', []),
	Record =.. [RecordPred, Timestamp, Term],
	prettyprintq_to_stream(S, Record, 0, 150),
	format(S, '.~n', []),
	close(S),
	!.
log_event_to_logfile(Operation, RecordPred, Term) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [log_event_to_logfile(Operation, RecordPred, Term)]),
	fail.

%-----------------------------------------------------------

log_string_event(Comment, Wrapper, String) :-
	check_record_pred_is_ok(Wrapper),
	(   get_current_logfile(File) ->
	    log_string_event_to_logfile(String, Comment, Wrapper, File)
	;
	    true
	).

log_string_event_to_logfile(String, Comment, Wrapper, File) :-
	open_regulus_file(File, append, S),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format(S, '~N% ~w, ~w (Timestamp: ~w)~n', [Comment, Wrapper, Timestamp]),
	format(S, '~N% "~s"~n', [String]),
	close(S),
	!.
log_string_event_to_logfile(String, Comment, Wrapper, File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [log_string_event_to_logfile(String, Comment, Wrapper, File)]),
	fail.
	
%-----------------------------------------------------------

get_new_logfile_name(LogfileDir, AbsFile) :-
	get_logfile_dir(LogfileDir, Dir),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format_to_atom('~w/dialogue_server_log_~w.pl', [Dir, Timestamp], File),
	absolute_file_name(File, AbsFile),
	!.
get_new_logfile_name(AbsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [get_new_logfile_name(AbsFile)]),
	fail.

get_logfile_dir(LogfileDir, Dir) :-
	absolute_file_name(LogfileDir, Dir),
	(   safe_directory_exists(Dir) ->
	    true
	;
	    otherwise ->
	    make_directory(Dir)
	),
	!.
get_logfile_dir(Dir) :-
	format2error('~N*** Error: bad call: ~w~n', [get_logfile_dir(Dir)]),
	fail.

%-----------------------------------------------------------

:- dynamic current_logfile/1.

set_current_logfile(File) :-
	retractall(current_logfile(_)),
	assertz(current_logfile(File)).

get_current_logfile(File) :-
	current_logfile(File).

%-----------------------------------------------------------

check_record_pred_is_ok(RecordPred) :-
	(   atom(RecordPred) ->
	    true
	;
	    format2error('~N*** Error: bad second argument "~w" to start_new_prolog_format_logfile/2 or log_event_in_prolog_format/2. Must be an atom.~n', [RecordPred]),
	    fail
	).
