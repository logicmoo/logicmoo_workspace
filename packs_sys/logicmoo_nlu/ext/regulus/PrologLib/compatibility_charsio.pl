% Compatibility module for use under SICStus 4

:- module(charsio,
	  [format_to_chars/3,
	   write_to_chars/3,
	   write_to_chars/4,
	   write_term_to_chars/3,
	   write_term_to_chars/4,
	   read_from_chars/2,
	   read_from_chars/3,
	   open_chars_stream/2,
	   with_output_to_chars/2,
	   with_output_to_chars/3,
	   with_output_to_chars/4
	   ]
	 ).

:- meta_predicate with_output_to_chars(:, +).
:- meta_predicate with_output_to_chars(:, +, +).
:- meta_predicate with_output_to_chars(:, +, +, +).
 
%----------------------------------------------------------------------

:- use_module(library(codesio)).

%----------------------------------------------------------------------

format_to_chars(X, Y) :-
	format_to_codes(X, Y).

format_to_chars(X, Y, Z) :-
	format_to_codes(X, Y, Z).

write_to_chars(X, Y, Z) :-
	write_to_codes(X, Y, Z).

write_to_chars(X, Y, Z, W) :-
	write_to_codes(X, Y, Z, W).

write_term_to_chars(X, Y, Z) :-
	write_term_to_codes(X, Y, Z).

write_term_to_chars(X, Y, Z, W) :-
	write_term_to_codes(X, Y, Z, W).

read_from_chars(X, Y) :-
	read_from_codes(X, Y).

read_from_chars(X, Y, Z) :-
	read_term_from_codes(X, Y, Z).

open_chars_stream(X, Y) :-
	open_codes_stream(X, Y).

with_output_to_chars(X, Y) :-
	with_output_to_codes(X, Y).

with_output_to_chars(X, Y, Z) :-
	with_output_to_codes(X, Y, Z).

with_output_to_chars(X, Y, Z, W) :-
	with_output_to_codes(X, Y, Z, W).

