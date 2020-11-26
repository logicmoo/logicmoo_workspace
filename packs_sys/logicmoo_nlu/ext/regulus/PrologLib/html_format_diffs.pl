
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

%---------------------------------------------------------------

:- module(html_format_diffs,
	  [html_format_diffs/2,
	   html_format_diffs/5,
	   test_html_format_diffs/1
	   ]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

test_html_format_diffs(make_test_data) :-
	Str1a = "Bonjour, j'ai un problème avec Norton car il ne se lance pas.",
	Str1b = "Bonjour. J'ai un problème avec Norton. Il ne se lance pas.",
	Str2a = "j ai internet explorer et depuis une mise a jour, je n'ai plus la barre d'outil norton...",
	Str2b = "J'ai Internet Explorer. Depuis une mise a jour, je n'ai plus la barre d'outil norton.",
	Str3a = "Salut",
	Str3b = "Salut",

	safe_absolute_file_name('$ACCEPT/MT/Comparison/test_fr.csv', AbsFile),
	
	open(AbsFile, write, S, [encoding('UTF-8')]),
	
	format(S, '~s\t~s~n', [Str1a, Str1b]),
	format(S, '~s\t~s~n', [Str2a, Str2b]),
	format(S, '~s\t~s~n', [Str3a, Str3b]),

	close(S),
	format('~N--- Written file ~w~n', [AbsFile]).

test_html_format_diffs(go) :-
	html_format_diffs('$ACCEPT/MT/Comparison/test_fr_en.csv',
			  '$ACCEPT/MT/Comparison/test_fre_en.html',
			  'UTF-8', 0'", 0',). %"'

%---------------------------------------------------------------
	
html_format_diffs(InFile, OutFile) :-
	html_format_diffs(InFile, OutFile, default_encoding, 0'", 0',).  %"'

html_format_diffs(InFile, OutFile, Encoding, DelimiterChar, SeparatorChar) :-
	absolute_file_name(InFile, AbsInFile),
	csv_file_to_list_of_lists(AbsInFile, Encoding, DelimiterChar, SeparatorChar, InList),
	length(InList, N),
	format('~N--- Read CSV input table (~d lines) ~w~n', [N, AbsInFile]),
	html_format_diffs_list(InList, OutList),
	html_column_headers(Headers),
	list_to_color_coded_html_file([Headers | OutList], OutFile),
	!.

html_column_headers(['RawSource','PreEditedSource','TranslRaw','TranslPreEdited','Identical']).

html_format_diffs_list([], []).
html_format_diffs_list([F | R], [F1 | R1]) :-
	html_format_diff_line(F, F1),
	!,
	html_format_diffs_list(R, R1).

html_format_diff_line(Line) :-
	is_list(Line),
	length(Line, Len),
	Len \== 4,
	!,
	format('~N*** Error: bad line "~w" does not contain four elements~n', [Line]),
	fail.
html_format_diff_line([Source1, Source2, Target1, Target2],
		      [Source1, Source2, AnnotatedTarget1, AnnotatedTarget2, SameP]) :-
	split_atom_into_words(Target1, Target1Words),
	split_atom_into_words(Target2, Target2Words),
	(   Target1Words = Target2Words ->
	    [AnnotatedTarget1, AnnotatedTarget2, SameP] = [Target1,Target2, same]
	;
	    otherwise ->
	    colour_diff_items(Target1, Target2, AnnotatedTarget1, AnnotatedTarget2),
	    SameP = different
	),
	!.
html_format_diff_line(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [html_format_diff_line(F, F1)]),
	fail.

colour_diff_items(Target1, Target2, AnnotatedTarget1, AnnotatedTarget2) :-
	insertions_deletions_substitutions_and_matches(Target1, Target2, _Total, _I, _D, _S, Matches),
	colour_diff_items1(Matches, AnnotatedTarget1, AnnotatedTarget2),
	!.
colour_diff_items(Target1, Target2, AnnotatedTarget1, AnnotatedTarget2) :-
	format('~N*** Error: bad call: ~w~n', [colour_diff_items(Target1, Target2, AnnotatedTarget1, AnnotatedTarget2)]),
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
	open(File, write, S),
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
	print_html_line(F, S),
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
color_id_to_html_color(insert, '#00ff33').       %green
color_id_to_html_color(substitute, '#0000cc').   %blue
