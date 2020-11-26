
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(html_diff,
	  [html_diff/4,
	   html_diff_from_command_line/1,
	   
	   test_html_diff/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

% Testing

test_html_diff(1) :-
	html_diff('$ACCEPT/MT/Homophones/Data/multiple_small.txt',
		  '$ACCEPT/MT/Homophones/Data/unique_small.txt',
		  'UTF-8',
		  '$ACCEPT/MT/Homophones/Data/unique_multiple_diff.html').

test_html_diff(2) :-
	html_diff('$ACCEPT/MT/Homophones/Data/devtest_b_unique.txt',
		  '$ACCEPT/MT/Homophones/Data/devtest_b_multiple.txt',
		  'UTF-8',
		  '$ACCEPT/MT/Homophones/Data/unique_multiple_diff.html').

test_html_diff(3) :-
	html_diff('$ACCEPT/MT/Homophones/Data/devtest_b_raw.fr',
		  '$ACCEPT/MT/Homophones/Data/devtest_b_unique.txt',
		  'UTF-8',
		  '$ACCEPT/MT/Homophones/Data/raw_unique_diff.html').
		
%---------------------------------------------------------------

html_diff_from_command_line([File1, File2, Encoding, Out]) :-
	!,
	html_diff(File1, File2, Encoding, Out).
html_diff_from_command_line(_Args) :-
	format('~N~nUsage: sicstus -l run_html_diff.pl -a <File1> <File2> <Encoding> <OutFile> ~2n', []).

%---------------------------------------------------------------
	
html_diff(File1, File2, Encoding, Out) :-
	absolute_file_name(File1, AbsFile1),
	absolute_file_name(File2, AbsFile2),
	absolute_file_name(Out, AbsOut),
	
	read_file_to_atom_list(AbsFile1, Encoding, List1),
	read_file_to_atom_list(AbsFile2, Encoding, List2),
	
	length(List1, N1),
	length(List2, N2),

	(   N1 = N2 ->
	    N = N1,
	    format('~N--- Read input files (~d lines)~n', [N])

	;
	    format('~N*** Error: files are different lengths: ~d, ~d~n', [N1, N2]),
	    fail
	),
	
	html_diff_list(List1, List2, OutList, 0-NDiffs),
	list_to_color_coded_html_file(OutList, AbsOut),
	format('~N--- Written output file (~d differences) ~w~n', [NDiffs, AbsOut]),
	!.

html_diff_list([], [], [], N-N).
html_diff_list([F | R], [F1 | R1], [F2, F3 | R2], In-Out) :-
	html_diff_line(F, F1, F2, F3, In-Next),
	!,
	html_diff_list(R, R1, R2, Next-Out).

html_diff_line(F, F1, F2, F3, In-Out) :-
	(   F = F1 ->
	    F2 = F,
	    F3 = F,
	    Out = In
	;
	    otherwise ->
	    colour_diff_items(F, F1, F2, F3, In-Out)
	),
	!.
html_diff_line(F, F1, F2, F3, In-Out) :-
	format('~N*** Error: bad call: ~w~n', [html_diff_line(F, F1, F2, F3, In-Out)]),
	fail.

colour_diff_items(F, F1, F2, F3, In-Out) :-
	insertions_deletions_substitutions_and_matches(F, F1, Total, _I, _D, _S, Matches),
	colour_diff_items1(Matches, F2, F3),
	Out is In + Total,
	!.
colour_diff_items(F, F1, F2, F3, In-Out) :-
	format('~N*** Error: bad call: ~w~n', [colour_diff_items(F, F1, F2, F3, In-Out)]),
	fail.

colour_diff_items1([], [], []).
colour_diff_items1([same(X) | R1], [X | R2], [X | R3]) :-
	!,
	colour_diff_items1(R1, R2, R3).
colour_diff_items1([sub(X, Y) | R1], [color(substitute, X) | R2], [color(substitute, Y) | R3]) :-
	!,
	colour_diff_items1(R1, R2, R3).
colour_diff_items1([del(X) | R1], R2, [color(delete, X) | R3]) :-
	!,
	colour_diff_items1(R1, R2, R3).
colour_diff_items1([ins(X) | R1], [color(insert, X) | R2], R3) :-
	!,
	colour_diff_items1(R1, R2, R3).

%---------------------------------------------------------------

list_to_color_coded_html_file(Lines, File) :-
	safe_absolute_file_name(File, AbsFile),
	open(File, write, S, [encoding('UTF-8'), encoding_signature(true)]),
	print_html_table_opening(S),
	print_html_table1(Lines, S),
	print_html_table_closing(S),
	close(S),
	length(Lines, N),
	format('~N--- Written HTML reviewing table (~d lines) ~w~n', [N, AbsFile]),
	!.

print_html_table_opening(S) :-
	format(S, '~N<html>~n', []),
	format(S, '~N<body>~n', []),
	format(S, '~N<table  border="1">~n', []),
	!.

print_html_table_closing(S) :-
	format(S, '~N</table>~n', []),
	format(S, '~N</body>~n', []),
	format(S, '~N</html>~n', []),
	!.

print_html_table1([], _S).
print_html_table1([F | R], S) :-
	print_html_line([F], S),
	!,
	print_html_table1(R, S).

print_html_line(Line, S) :-
	print_html_line_opening(S),
	print_html_line1(Line, S),
	print_html_line_closing(S),
	!.
print_html_line(Line, S) :-
	format('~N*** Error: bad call: ~w~n', [print_html_line(Line, S)]),
	fail.

print_html_line_opening(S) :-
	format(S, '~N<tr>~n', []),
	!.

print_html_line_closing(S) :-
	format(S, '~N</tr>~n', []),
	!.

print_html_line1([], _S).
print_html_line1([F | R], S) :-
	print_html_line_element(F, S),
	!,
	print_html_line1(R, S).

print_html_line_element(Elt, S) :-
	format(S, '~N<td>', []),
	print_html_line_element1(Elt, S),
	format(S, '</td>~n', []),
	!.

print_html_line_element1([], _S) :-
	!.
print_html_line_element1([F | R], S) :-
	print_html_line_element2(F, S),
	format(S, ' ', []),
	!,
	print_html_line_element1(R, S).
print_html_line_element1(Other, S) :-
	print_html_line_element2(Other, S).

print_html_line_element2(color(ColorID, Text), S) :-
	color_id_to_html_color(ColorID, HTMLColor),
	format(S, '<FONT COLOR="~w">~w</FONT>', [HTMLColor, Text]),
	!.
print_html_line_element2(Other, S) :-
	format(S, '~w', [Other]),
	!.

color_id_to_html_color(delete, '#ff0033').       %red
%color_id_to_html_color(insert, '#00ff33').       %green
color_id_to_html_color(insert, '#ff0033').       %red
%color_id_to_html_color(substitute, '#0000cc').   %blue
color_id_to_html_color(substitute, '#ff0033').   %red
