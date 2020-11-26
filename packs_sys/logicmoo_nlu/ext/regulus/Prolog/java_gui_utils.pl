:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(java_gui_utils,
	[package_processing_alist_for_java_gui/2,
	 prettyprint_to_string_for_gui/3,
	 replace_characters_in_string_for_gui/2,
	 truncate_comment_lines_in_string_for_java_gui/2,
	 wrap_lines_in_string_for_java_gui/2
	]
	 ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------

% Length of lines for prettyprinted material sent to GUI
line_length_for_gui(100).

% How many comment lines to keep when formatting rules
max_number_of_comment_lines_to_keep_for_gui(20).

%----------------------------------------------------------------------

package_processing_alist_for_java_gui([], []).
package_processing_alist_for_java_gui([F | R], [F1 | R1]) :-
	package_processing_alist_element_for_java_gui(F, F1),
	!,
	package_processing_alist_for_java_gui(R, R1).

package_processing_alist_element_for_java_gui(Key=Value, Key1=Value1) :-
	format_to_chars('~w', [Key], Key1),
	package_processing_alist_element_for_java_gui1(Value, Value1),
	!.
package_processing_alist_element_for_java_gui(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [package_processing_alist_element_for_java_gui(X, Y)]),
	fail.

package_processing_alist_element_for_java_gui1(X, String1) :-
	var(X),
	format_to_chars('~w', [X], String1),
	!.
package_processing_alist_element_for_java_gui1([], "*empty_string*") :-
	!.
package_processing_alist_element_for_java_gui1(X, String1) :-
	atom(X),
	format_to_chars('~w', [X], String),
	replace_characters_in_string_for_gui(String, String1),
	!.
package_processing_alist_element_for_java_gui1(String, String1) :-
	is_prolog_string(String),
	replace_characters_in_string_for_gui(String, String1),
	!.
package_processing_alist_element_for_java_gui1(Term, String) :-
	line_length_for_gui(LineLength),
	prettyprint_to_string_for_gui(Term, String, LineLength),
	!.

%----------------------------------------------------------------------

prettyprint_to_string_for_gui(Term, String, LineLength) :-
	with_output_to_chars(prettyprint(Term, 0, LineLength), String0),
	replace_characters_in_string_for_gui(String0, String),
	!.
prettyprint_to_string_for_gui(Term, String, LineLength) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [prettyprint_to_string_for_gui(Term, String, LineLength)]),
	fail.

%----------------------------------------------------------------------

% Newlines and accented characters in strings seem to cause problems for PrologBeans

replace_characters_in_string_for_gui([], []).
replace_characters_in_string_for_gui([F | R], [0'!, Char1, Char2, 0'! | R1]) :-
	dangerous_character_for_prolog_beans(F, [Char1, Char2]),
	!,
	replace_characters_in_string_for_gui(R, R1).
replace_characters_in_string_for_gui([F | R], [F | R1]) :-
	!,
	replace_characters_in_string_for_gui(R, R1).

% Choose replacement characters that don't mean anything to regex
dangerous_character_for_prolog_beans(0'\n, "NL").

dangerous_character_for_prolog_beans(0'á, "a1").
dangerous_character_for_prolog_beans(0'â, "a2").
dangerous_character_for_prolog_beans(0'à, "a3").
dangerous_character_for_prolog_beans(0'ä, "a4").
dangerous_character_for_prolog_beans(0'å, "a5").

dangerous_character_for_prolog_beans(0'ç, "c1").

dangerous_character_for_prolog_beans(0'é, "e1").
dangerous_character_for_prolog_beans(0'ê, "e2").
dangerous_character_for_prolog_beans(0'è, "e3").
dangerous_character_for_prolog_beans(0'ë, "e4").
dangerous_character_for_prolog_beans(0'æ, "e6").

dangerous_character_for_prolog_beans(0'í, "i1").
dangerous_character_for_prolog_beans(0'î, "i2").
dangerous_character_for_prolog_beans(0'ì, "i3").
dangerous_character_for_prolog_beans(0'ï, "i4").

dangerous_character_for_prolog_beans(0'ñ, "n1").

dangerous_character_for_prolog_beans(0'ó, "o1").
dangerous_character_for_prolog_beans(0'ô, "o2").
dangerous_character_for_prolog_beans(0'ò, "o3").
dangerous_character_for_prolog_beans(0'ö, "o4").

dangerous_character_for_prolog_beans(0'ú, "u1").
dangerous_character_for_prolog_beans(0'û, "u2").
dangerous_character_for_prolog_beans(0'ù, "u3").
dangerous_character_for_prolog_beans(0'ü, "u4").

%----------------------------------------------------------------------

truncate_comment_lines_in_string_for_java_gui(StringIn, StringOut) :-
	split_string_into_strings(StringIn, 0'\n, LinesIn),
	max_number_of_comment_lines_to_keep_for_gui(MaxComments),
	truncate_comment_lines_in_string_for_java_gui1(LinesIn, 0, MaxComments, LinesOut),
	append_strings_with_char(LinesOut,  0'\n, StringOut),
	!.
truncate_comment_lines_in_string_for_java_gui(StringIn, StringOut) :-
	format2error('~N*** Error: bad call: ~w',
		     [truncate_comment_lines_in_string_for_java_gui(StringIn, StringOut)]),
	fail.

truncate_comment_lines_in_string_for_java_gui1([], _NComments, _MaxComments, []).
truncate_comment_lines_in_string_for_java_gui1([F | R], NComments, MaxComments, Result) :-
	is_comment_line(F),
	NComments1 is NComments + 1,
	(   NComments1 =< MaxComments ->
	    Result = [F | R1]
	;
	    otherwise ->
	    Result = R1
	),
	!,
	truncate_comment_lines_in_string_for_java_gui1(R, NComments1, MaxComments, R1).
truncate_comment_lines_in_string_for_java_gui1([F | R], NComments, MaxComments, [F | R1]) :-
	!,
	truncate_comment_lines_in_string_for_java_gui1(R, NComments, MaxComments, R1).

is_comment_line(Chars) :-
	first_non_whitespace_in_string_is_comment_char(Chars).

first_non_whitespace_in_string_is_comment_char([]).
first_non_whitespace_in_string_is_comment_char([0'% | _R]) :-
	!.
first_non_whitespace_in_string_is_comment_char([F | R]) :-
	whitespace_char(F),
	!,
	first_non_whitespace_in_string_is_comment_char(R).

%----------------------------------------------------------------------

wrap_lines_in_string_for_java_gui(InString, OutString) :-
	line_length_for_gui(MaxLineLength),
	MaxLineLength1 is MaxLineLength - 1,
	wrap_lines_in_string_for_java_gui(InString, MaxLineLength1, 0, OutString).

wrap_lines_in_string_for_java_gui([], _MaxLineLength, _NCharsOnCurrentLine, []) :-
	!.
wrap_lines_in_string_for_java_gui([0'\n | R], MaxLineLength, _NCharsOnCurrentLine, [0'\n | R1]) :-
	!,
	wrap_lines_in_string_for_java_gui(R, MaxLineLength, 0, R1).
wrap_lines_in_string_for_java_gui([F | R], MaxLineLength, NCharsOnCurrentLine, [F, 0'\\, 0'\n | R1]) :-
	NCharsOnCurrentLine >= MaxLineLength,
	!,
	wrap_lines_in_string_for_java_gui(R, MaxLineLength, 0, R1).
wrap_lines_in_string_for_java_gui([0'\t | R], MaxLineLength, NCharsOnCurrentLine, [0'\t | R1]) :-
	NCharsOnCurrentLine1 is NCharsOnCurrentLine + 8 - (NCharsOnCurrentLine mod 8),
	!,
	wrap_lines_in_string_for_java_gui(R, MaxLineLength, NCharsOnCurrentLine1, R1).
wrap_lines_in_string_for_java_gui([F | R], MaxLineLength, NCharsOnCurrentLine, [F | R1]) :-
	NCharsOnCurrentLine1 is NCharsOnCurrentLine + 1,
	!,
	wrap_lines_in_string_for_java_gui(R, MaxLineLength, NCharsOnCurrentLine1, R1).

	
	
