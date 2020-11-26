
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%------------------------------------------------------------------------------------

search_for_patterns(Alist) :-
	convert_atoms_to_strings_in_alist(Alist, Alist1),
	search_for_patterns1(Alist1).

search_for_patterns1(AlistIn) :-
	read_line(Line),
	format('~N~s~n', [Line]),
	flush_output(user),
	(   (  member(Pattern-Touchfile, AlistIn), is_substring(Pattern, Line) ) ->
	    write_touchfile(Touchfile),
	    delete(AlistIn, Pattern-Touchfile, AlistNext)
	;
	    otherwise ->
	    AlistNext = AlistIn
	),
	!,
	(   AlistNext = [] ->
	    pass_through_lines
	;
	    otherwise ->
	    search_for_patterns1(AlistNext)
	).

convert_atoms_to_strings_in_alist([], []) :-
	!.
convert_atoms_to_strings_in_alist([Atom-File | R], [String-File | R1]) :-
	atom_codes(Atom, String),
	!,
	convert_atoms_to_strings_in_alist(R, R1).
convert_atoms_to_strings_in_alist(Alist, AlistIn) :-
	format('~N*** Bad call: ~w~n',
	       [convert_atoms_to_strings_in_alist(Alist, AlistIn)]),
	fail.

pass_through_lines :-
	read_line(Line),
	format('~N~s~n', [Line]),
	flush_output(user),
	!,
	pass_through_lines.

write_touchfile(File) :-
	safe_absolute_file_name(File, AbsFile),
	open(AbsFile, write, S),
	format(S, 'touchfile', []),
	close(S).

