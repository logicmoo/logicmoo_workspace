
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(logging,
	  [
	   log_event/2,
	   log_event/3,
	   finish_logfile/0,
	   read_logfile/2
	  ]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(xml)).

%-----------------------------------------------------------

%log_start_new_session(CFGFile) :-
%	get_new_logfile_name(File),
%	set_current_logfile(File),
%	log_event('START_SESSION', [CFGFile]).

%-----------------------------------------------------------

log_event(Operation, Args) :-
	log_event(Operation, "*null_comment*", Args).

log_event(Operation, CommentChars, Args) :-
	(   get_current_logfile(File) ->
	    true
	;
	    get_new_logfile_name(File),
	    write_logfile_header(File),
	    set_current_logfile(File)
	),
	log_event_to_logfile(Operation, CommentChars, Args, File).

finish_logfile :-
	(   get_current_logfile(File) ->
	    write_logfile_footer(File),
	    unset_current_logfile
	;
	    format2error('~N*** Error: request to finish logfile but no current logfile defined~n', []),
	    fail
	).

log_event_to_logfile(Operation, CommentChars, Args, File) :-
	open_regulus_file(File, append, S),
	write_log_term(S, Operation, CommentChars, Args),
	close(S),
	!.
log_event_to_logfile(Operation, CommentChars, Args, File) :-
	format2error('~N*** Error: bad call: ~w~n', [log_event_to_logfile(Operation, CommentChars, Args, File)]),
	fail.

%-----------------------------------------------------------

write_logfile_header(File) :-
	open_regulus_file(File, append, S),
	format(S, "~N<?xml version='1.0' encoding='ISO-8859-1'?>~n", []),
	format(S, "~N<logfile>~n", []),
	close(S),
	!.
write_logfile_header(File) :-
	format2error('~N*** Error: bad call: ~w~n', [write_logfile_header(File)]),
	fail.

write_logfile_footer(File) :-
	open_regulus_file(File, append, S),
	format(S, "~N</logfile>~n", []),
	close(S),
	!.
write_logfile_footer(File) :-
	format2error('~N*** Error: bad call: ~w~n', [write_logfile_footer(File)]),
	fail.

%-----------------------------------------------------------

read_logfile(File, XMLTerm) :-
	absolute_file_name(File, AbsFile),
	read_file_to_string(AbsFile, String),
	xml_parse(String, XMLTerm).

%-----------------------------------------------------------

write_log_term(S, Operation, CommentChars, Args) :-
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	comment_atom(CommentChars, CommentAtom),
	write_log_term1(S, Timestamp, CommentAtom, Operation, Args).

comment_atom("*null_comment*", '') :-
	!.
comment_atom(CommentChars, CommentAtom) :-
	format_to_atom('comment="~s" ', [CommentChars], CommentAtom).

write_log_term1(S, Timestamp, CommentAtom, Operation, []) :-
	format(S, '~N~n<EVENT operation="~w" ~wtimestamp="~w"/>~n', [Operation, CommentAtom, Timestamp]),
	!.
write_log_term1(S, Timestamp, CommentAtom, Operation, Args) :-
	format(S, '~N~n<EVENT operation="~w" ~wtimestamp="~w">~n', [Operation, CommentAtom, Timestamp]),
	write_log_term_args(Args, 1, S),
	format(S, '~N</EVENT>~n', []),
	!.

write_log_term_args([], _I, _S).
write_log_term_args([F | R], I, S) :-
	write_log_term_arg(F, I, S),
	I1 is I + 1,
	!,
	write_log_term_args(R, I1, S).

write_log_term_arg(Arg, I, S) :-
	make_arg_into_atom(Arg, Atom),
	format(S, '~N   <ARG~d>~w</ARG~d>~n', [I, Atom, I]),
	!.
write_log_term_arg(Arg, I, S) :-
	format2error('~N*** Error: bad call: ~w~n', [write_log_term_arg(Arg, I, S)]),
	fail.

%-----------------------------------------------------------

make_arg_into_atom(Arg, Atom) :-
	atomic(Arg),
	!,
	Arg = Atom.
make_arg_into_atom(Arg, Atom) :-
	is_list(Arg),
	make_arg_into_atom_list(Arg, AtomList),
	join_with_spaces(AtomList, Atom).

make_arg_into_atom_list([], []).
make_arg_into_atom_list([F | R], [F1 | R1]) :-
	make_arg_into_atom(F, F1),
	make_arg_into_atom_list(R, R1).

%-----------------------------------------------------------

get_new_logfile_name(AbsFile) :-
	get_logfile_dir(Dir),
	datime(Datime),
	datime_to_timestamp(Datime, Timestamp),
	format_to_atom('~w/log_~w.xml', [Dir, Timestamp], File),
	absolute_file_name(File, AbsFile),
	!.
get_new_logfile_name(AbsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [get_new_logfile_name(AbsFile)]),
	fail.

get_logfile_dir(Dir) :-
	absolute_file_name('$REGULUS/logfiles', Dir),
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

unset_current_logfile :-
	retractall(current_logfile(_)).

