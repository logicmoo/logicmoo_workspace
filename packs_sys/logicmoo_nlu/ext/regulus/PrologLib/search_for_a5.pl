
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%------------------------------------------------------------------------------------

search_for_string_in_input(Pattern, Touchfile) :-
	read_line(Line),
	format('~N~s~n', [Line]),
	flush_output(user),
	(   is_substring(Pattern, Line) ->
	    write_touchfile(Touchfile),
	    !,
	    pass_through_lines
	;
	    otherwise ->
	    search_for_string_in_input(Pattern, Touchfile)
	).

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

:- search_for_string_in_input("a: 5", '$CALLSLT/tmp/found_a5.txt').
