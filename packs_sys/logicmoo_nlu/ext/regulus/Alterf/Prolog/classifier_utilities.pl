
% classifier_utilities.pl

%---------------------------------------------------------------

:- module(classifier_utilities,
	  [is_alist/1,
           check_alists_are_compatible/1,
	   open_alist/3,
	   close_alist/1,
	   read_alist/2,
	   is_end_of_file_alist/1,
           make_tmp_file/2,
	   get_default_confidence_threshold/1,
	   set_default_confidence_threshold/1]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(terms)).

%------------------------------------------------------------------------------------

:- dynamic default_confidence_threshold/1.

%------------------------------------------------------------------------------------

is_alist(X) :-
	is_list(X),
	is_alist1(X).

is_alist1([]).
is_alist1([F | R]) :-
	subsumes_chk(_-_, F),
	!,
	is_alist1(R).

%------------------------------------------------------------------------------------

check_alists_are_compatible([]) :-
	!.
check_alists_are_compatible([F | R]) :-
	is_alist(F),
	get_keys_from_alist(F, Keys),
	no_doubles(Keys),
	check_alists_are_compatible1(Keys, R),
	!.
check_alists_are_compatible(Other) :-
	format('~N*** Error: bad call: ~w~n', [check_alists_are_compatible(Other)]),
	fail.

check_alists_are_compatible1(_Keys, []) :-
	!.
check_alists_are_compatible1(Keys, [F | R]) :-
	get_keys_from_alist(F, Keys1),
	permutation(Keys, Keys1),
	!,
	check_alists_are_compatible1(Keys, R).

%------------------------------------------------------------------------------------

% open_alist(+FileAlist, +Mode, -StreamAlist)

open_alist([], _Mode, []) :-
	!.
open_alist([Key-File | Files], Mode, [Key-Stream | Streams]) :-
	open(File, Mode, Stream),
	!,
	open_alist(Files, Mode, Streams).
open_alist(FileAlist, Mode, StreamAlist) :-
	format('~N*** Error: bad call: ~w~n', [open_alist(FileAlist, Mode, StreamAlist)]),
	fail.

%------------------------------------------------------------------------------------

% close_alist(+StreamAlist)

close_alist([]) :-
	!.
close_alist([_Key-Stream | Streams]) :-
	close(Stream),
	!,
	close_alist(Streams).
close_alist(Other) :-
	format('~N*** Error: bad call: ~w~n', [close_alist(Other)]),
	fail.

%------------------------------------------------------------------------------------

% read_alist(+StreamAlist, -TermAlist)

read_alist([], []) :-
	!.
read_alist([Key-Stream | Streams], [Key-Term | Terms]) :-
	read(Stream, Term),
	read_alist(Streams, Terms),
	!.
read_alist(StreamAlist, TermAlist) :-
	format('~N*** Error: bad call: ~w~n', [read_alist(StreamAlist, TermAlist)]),
	fail.

%------------------------------------------------------------------------------------

is_end_of_file_alist([]) :-
	!.
is_end_of_file_alist([_Key-Term | R]) :-
	Term == end_of_file,
	!,
	is_end_of_file_alist(R).

%------------------------------------------------------------------------------------

make_tmp_file(File0, File) :-
	absolute_file_name(alterf_generated_files(File0), File).

%--------------------------------------------------------------------------------------------

get_default_confidence_threshold(Value) :-
	default_confidence_threshold(Value1),
	!,
	Value = Value1.
get_default_confidence_threshold(Value) :-
	Value = 45.

set_default_confidence_threshold(Value) :-
	retractall(default_confidence_threshold(_)),
	assertz(default_confidence_threshold(Value)).

%--------------------------------------------------------------------------------------------

get_keys_from_alist([], []).
get_keys_from_alist([Key-_ | R], [Key | R1]) :-
	get_keys_from_alist(R, R1).
