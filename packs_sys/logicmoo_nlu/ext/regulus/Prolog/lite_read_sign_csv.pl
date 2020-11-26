:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(lite_read_sign_csv,
	[read_sign_csv/2,
	 read_sign_csv/3,
	 
	 test_read_sign_csv/1]
    ).

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%======================================================================

test_read_sign_csv(1) :-
	read_sign_csv('$MED_SLT2/Fre/corpora/trainslate1.csv',
		      '$MED_SLT2/Fre/corpora/trainslate1_converted.csv').

%======================================================================

read_sign_csv(CSVFile, TextFile) :-
	read_sign_csv(CSVFile, default_encoding, TextFile).

read_sign_csv(CSVFile, Encoding, TextFile) :-
	safe_absolute_file_name(CSVFile, AbsCSVFile),
	safe_absolute_file_name(TextFile, AbsTextFile),
	
	csv_file_to_list_of_lists(AbsCSVFile, Encoding, 0'", 0',, InList), %"'
	length(InList, NIn),
	format('~N--- Read CSV file (~d lines) ~w~n', [NIn, AbsCSVFile]),
	
	sign_csv_list_to_sign_text_list(InList, 1, OutList),
	length(OutList, NOut),
	write_atom_list_to_file(OutList, AbsTextFile),
	format('~N--- Written text Lite file (~d lines) ~w~n', [NOut, AbsTextFile]),
	!.
read_sign_csv(CSVFile, Encoding, _TextFile) :-
	format('~N*** Error: unable to convert CSV file ~w to Lite text form using encoding ~w~n',
	       [CSVFile, Encoding]),
	fail.

sign_csv_list_to_sign_text_list([], _N, []).
sign_csv_list_to_sign_text_list([F | R], I, [F1 | R1]) :-
	sign_csv_list_line_to_sign_text_line(F, I, F1),
	I1 is I + 1,
	!,
	sign_csv_list_to_sign_text_list(R, I1, R1).

sign_csv_list_line_to_sign_text_line(Line, _I, Atom) :-
	maybe_add_quotes_in_apply_template(Line, Line1),
	join_with_spaces(Line1, Atom),
	!.
sign_csv_list_line_to_sign_text_line(Line, I, _Atom) :-
	format('~N*** Error: unable to convert line ~d, ~w~n', [I, Line]),
	fail.

maybe_add_quotes_in_apply_template([ApplyTemplateField, Name | R], [ApplyTemplateField, Name | R1]) :-
	is_apply_template_field(ApplyTemplateField),
	add_quotes_in_apply_template_args(R, R1),
	!.
maybe_add_quotes_in_apply_template(Line, Line).

add_quotes_in_apply_template_args([], []).
add_quotes_in_apply_template_args([F | R], [F1 | R1]) :-
	add_quotes_in_apply_template_arg(F, F1),
	!,
	add_quotes_in_apply_template_args(R, R1).

is_apply_template_field(ApplyTemplateField) :-
	atomic(ApplyTemplateField),
	split_atom_into_words(ApplyTemplateField, [Word]),
	lowercase_atom(Word, Word1),
	Word1 = applytemplate.

add_quotes_in_apply_template_arg(Arg, Arg) :-
	whitespace_atom(Arg),
	!.
add_quotes_in_apply_template_arg(Arg, Arg1) :-
	format_to_atom('"~w"', [Arg], Arg1),
	!.


