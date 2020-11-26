
% prolog_json_compact.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(prolog_json,
	  [prolog_json/2,
	   safe_prolog_json/2]
	 ).

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

:- use_module(library(lists)).

%------------------------------------------------------------------------------------

/*

prolog_json(?Prolog, ?JSONString)

Convert Prolog term into JSON format, or vice versa. Useful for passing data between Prolog
and non-Prolog processes.

Version to produce "compact" JSON (based on prolog_xml_compact). Key principles:

- Only map a subset of JSON

- Prolog atom xxx <-> JSON string "xxx"

- Prolog oompound foo(bar, baz) <-> JSON array ["*t*", "foo", "bar", "baz"]

- Treat some special Prolog structures specially (=, list, alist)

Example:

| ?- Term = foo(a,b,[c, d, 6, p=q, some_vars(F, F, G)]), prolog_json(Term, JSON), format('~N~s~n~n', [JSON]), prolog_json(Term1, JSON), format('~N~q~n~n', [Term1]), fail.


*/

safe_prolog_json(Prolog, JSONString) :-
	on_exception(
	Exception, 
	prolog_json(Prolog, JSONString),
	handle_exception_in_prolog_json(Exception)
    ),
	!.

handle_exception_in_prolog_json(Exception) :-
	format('~N~n*** Exception in call to prolog_json/2 ***~n~n', []),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

prolog_json(Prolog, JSONString) :-
	nonvar(Prolog),
	prolog_to_json_term(Prolog, JSONTerm),
	with_output_to_chars(print_json_term(JSONTerm), JSONString),  
	!.
prolog_json(Prolog, JSONString) :-
	%is_prolog_string(JSONString),
	is_list_of_non_negative_integers(JSONString),
	(   json_term_surrounded_by_optional_whitespace(JSONTerm, JSONString, []) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to read JSON string: ~n~s~n', [JSONString]),
	    fail
	),
	json_term_to_prolog(JSONTerm, Prolog),
	!.

%------------------------------------------------------------------------------------

json_term_surrounded_by_optional_whitespace(Term) -->
	optional_whitespace,
	json_term(Term),
	optional_whitespace.

json_term(N) -->
	number(N),
	!.
json_term(null) -->
	"null",
	!.
json_term(Atom) -->
	[0'"],  %"
	string(String),
	[0'"],  %"
	{json_hex_coding_to_prolog_in_string(String, String1)},
	{atom_codes(Atom, String1)},
	!.
json_term(List) -->
	[0'[],
	optional_whitespace,
	json_term_list(List),
	optional_whitespace,
	[0']],
	!.
json_term(CurlyBracketList) -->
	[0'{],
	optional_whitespace,
	json_feat_val_list(List),
	optional_whitespace,
	[0'}],
	{list_to_curly_bracket_list(List, CurlyBracketList)},
	!.

json_term_list([F | R]) -->
	json_term(F),
	!,
	json_term_list_rest(R).

json_term_list_rest([F | R]) -->
	optional_whitespace,
	[0',],
	optional_whitespace,
	json_term(F),
	!,
	json_term_list_rest(R).
json_term_list_rest([]) -->
	[].

json_feat_val_list([F | R]) -->
	json_feat_val(F),
	!,
	json_feat_val_list_rest(R).

json_feat_val_list_rest([F | R]) -->
	optional_whitespace,
	[0',],
	optional_whitespace,
	json_feat_val(F),
	!,
	json_feat_val_list_rest(R).
json_feat_val_list_rest([]) -->
	[].

json_feat_val(Key:Val) -->
	json_term(Key),
	optional_whitespace,
	[0':],
	optional_whitespace,
	json_term(Val),
	!.

number(N) -->
	number_string(String),
	{number_codes(N, String)},
	!.

number_string(String) -->
	optional_minus(String-StringNext1),
	digit_string(StringNext1-StringNext2),
	optional_decimal_string(StringNext2-[]).

optional_minus([0'- | R]-R) -->
	[0'-],
	!.
optional_minus(Str-Str) -->
	[].

digit_string(In-Out) -->
	digit(In-Next),
	!,
	digit_string_rest(Next-Out).

digit_string_rest(In-Out) -->
	digit(In-Next),
	!,
	digit_string_rest(Next-Out).
digit_string_rest(In-In) -->
	[].

optional_decimal_string([0'. | Next]-Out) -->
	[0'.],
	!,
	digit_string(Next-Out).
optional_decimal_string(In-In) -->
	[].

digit([Char | R]-R) -->
	[Char],
	{digit_char(Char)},
	!.

string([F | R]) -->
	string_char(F),
	%{non_quote_char(F)}, 
	!,
	string(R).
string([]) -->
	[].

string_char(0'\n) -->
	[0'\\, 0'n],
	!.
string_char(0'\t) -->
	[0'\\, 0't],
	!.
string_char(0'") -->
	[0'\\, 0'"], %"
	!.
string_char(0'\\) -->
	[0'\\, 0'\\], 
	!.
string_char(Char) -->
	[Char],
	{non_quote_char(Char)}, 
	!.

optional_whitespace -->
	whitespace_char,
	!,
	optional_whitespace.
optional_whitespace -->
	[].

whitespace_char -->
	[Char],
	{whitespace_char(Char)},
	!.

non_quote_char(Char) :-
	Char \== 0'". %"

%------------------------------------------------------------------------------------

json_hex_coding_to_prolog_in_string([], []).
json_hex_coding_to_prolog_in_string([0'\\, 0'u, HexDigitChar1, HexDigitChar2, HexDigitChar3, HexDigitChar4 | R],
				    [Char | R1]) :-
	hex_digit_char_to_number(HexDigitChar1, HexDigit1),
	hex_digit_char_to_number(HexDigitChar2, HexDigit2),
	hex_digit_char_to_number(HexDigitChar3, HexDigit3),
	hex_digit_char_to_number(HexDigitChar4, HexDigit4),
	Char is HexDigit4 + 16 * HexDigit3 + 16 * 16 * HexDigit2 + 16 * 16 * 16 * HexDigit1,
	!,
	json_hex_coding_to_prolog_in_string(R, R1).
json_hex_coding_to_prolog_in_string([F | R], [F | R1]) :-
	!,
	json_hex_coding_to_prolog_in_string(R, R1).

hex_digit_char_to_number(0'0, 0).
hex_digit_char_to_number(0'1, 1).
hex_digit_char_to_number(0'2, 2).
hex_digit_char_to_number(0'3, 3).
hex_digit_char_to_number(0'4, 4).
hex_digit_char_to_number(0'5, 5).
hex_digit_char_to_number(0'6, 6).
hex_digit_char_to_number(0'7, 7).
hex_digit_char_to_number(0'8, 8).
hex_digit_char_to_number(0'9, 9).
hex_digit_char_to_number(0'a, 10).
hex_digit_char_to_number(0'b, 11).
hex_digit_char_to_number(0'c, 12).
hex_digit_char_to_number(0'd, 13).
hex_digit_char_to_number(0'e, 14).
hex_digit_char_to_number(0'f, 15).
hex_digit_char_to_number(0'A, 10).
hex_digit_char_to_number(0'B, 11).
hex_digit_char_to_number(0'C, 12).
hex_digit_char_to_number(0'D, 13).
hex_digit_char_to_number(0'E, 14).
hex_digit_char_to_number(0'F, 15).

%------------------------------------------------------------------------------------

print_json_term(N) :-
	number(N),
	format('~w', [N]),
	!.
print_json_term(null) :-
	format('null', []),
	!.
print_json_term(Atom) :-
	atom(Atom),
	atom_codes(Atom, String),
	% If any character in the string needs to be escaped in Unicode, escape the whole string that way.
	(   ( member(Char, String), Char > 255 ) ->
	    string_to_json_unicode_string(String, String1)
	;
	    otherwise ->
	    quote_layout_chars_in_string_for_json(String, String1)
	), 
	format('"~s"', [String1]), 
	!.
print_json_term(Key:Value) :-
	print_json_term(Key),
	format(':', []),
	print_json_term(Value),
	!.
print_json_term(List) :-
	is_list(List),
	List \= [],
	format('[ ', []),
	print_json_term_list(List),
	format(' ]', []),
	!.
print_json_term(CurlyBracketList) :-
	curly_bracket_list_to_list(CurlyBracketList, List),
	format('{ ', []),
	print_json_term_list(List),
	format(' }', []),
	!.
print_json_term(Other) :-
	format('~N*** Error: bad call: ~w~n',
	       [print_json_term(Other)]),
	fail.

print_json_term_list([Last]) :-
	print_json_term(Last),
	!.
print_json_term_list([F | R]) :-
	print_json_term(F),
	format(', ', []),
	!,
	print_json_term_list(R).

%------------------------------------------------------------------------------------

prolog_to_json_term(Var, ['*var*', VarAtom]) :-
	var(Var),
	format_to_atom('~w', [Var], VarAtom),
	!.
prolog_to_json_term([], null) :-
	!.
prolog_to_json_term(Atom, Atom) :-
	atom(Atom),
	!.
prolog_to_json_term(N, N) :-
	number(N),
	!.
% Special case for key/val list
prolog_to_json_term(List, JSONList) :-
	is_key_val_list(List),
	prolog_key_val_list_to_json_key_val_list(List, JSONList0),
	list_to_curly_bracket_list(JSONList0, JSONList),
	!.
% Special case for list
prolog_to_json_term(List, JSONList) :-
	is_list(List),
	prolog_list_to_json_term_list(List, JSONList),
	!.
% Normal term
%prolog_to_json_term(Term, ['*t*', TagName | JSONArgs]) :-
prolog_to_json_term(Term, { JSONFunctorWithAsterisk:JSONArgs }) :-
	compound(Term),
	Term =.. [Functor | Args],
	prolog_to_json_term(Functor, JSONFunctor),
	format_to_atom('*~w', [JSONFunctor], JSONFunctorWithAsterisk),
	prolog_list_to_json_term_list(Args, JSONArgs),
	!.
prolog_to_json_term(Prolog, JSONTerm) :-
	format('~N*** Error: bad call: ~w~n', [prolog_to_json_term(Prolog, JSONTerm)]),
	fail.

prolog_list_to_json_term_list([], []).
prolog_list_to_json_term_list([F | R], [F1 | R1]) :-
	prolog_to_json_term(F, F1),
	!,
	prolog_list_to_json_term_list(R, R1).
prolog_list_to_json_term_list(Args, JSONArgs) :-
	format('~N*** Error: bad call: ~w~n', [prolog_list_to_json_term_list(Args, JSONArgs)]),
	fail.

prolog_key_val_list_to_json_key_val_list([], []).
prolog_key_val_list_to_json_key_val_list([F | R], [F1 | R1]) :-
	prolog_key_val_element_to_json_key_val_element(F, F1),
	!,
	prolog_key_val_list_to_json_key_val_list(R, R1).
prolog_key_val_list_to_json_key_val_list(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [prolog_key_val_list_to_json_key_val_list(X, Y)]),
	fail.

prolog_key_val_element_to_json_key_val_element(Key=Val, JSONKey:JSONVal) :-
	prolog_to_json_term(Key, JSONKey),
	prolog_to_json_term(Val, JSONVal),
	!.
prolog_key_val_element_to_json_key_val_element(F, F1) :-
	format('~N*** Error: bad call: ~w~n', [prolog_key_val_element_to_json_key_val_element(F, F1)]),
	fail.

is_key_val_list(List) :-
	is_list(List),
	List \== [],
	is_key_val_list1(List).

is_key_val_list1([]).
is_key_val_list1([F | R]) :-
	compound(F),
	F = (_ = _),
	!,
	is_key_val_list1(R).

%------------------------------------------------------------------------------------

prolog_functor_to_json_tag_name(Functor, TagName) :-
	atom_codes(Functor, String),
	string_to_json_tag_string(String, TagString),
	TagName = TagString.

%prolog_list_wrapper_to_json_tag_name(F, TagName) :-
%	prolog_functor_to_json_tag_name(F, F1),
%	append(F1, "---list", TagName).
prolog_list_wrapper_to_json_tag_name(F, TagName) :-
	prolog_functor_to_json_tag_name(F, TagName).
	
%------------------------------------------------------------------------------------

json_tag_name_to_prolog_functor(TagString, Functor) :-
	json_tag_string_to_string(TagString, String),
	atom_codes(Functor, String).

%json_tag_name_to_prolog_list_wrapper(TagString, F) :-
%	append(FString, "---list", TagString),
%	atom_codes(F, FString).
json_tag_name_to_prolog_list_wrapper(TagString, F) :-
	atom_codes(F, TagString).

%------------------------------------------------------------------------------------

json_term_to_prolog(JSONTerm, Prolog) :-
	json_term_to_prolog(JSONTerm, Prolog, []-_VarAssocFinal).

json_term_list_to_prolog_list(JSONTerm, Prolog) :-
	json_term_list_to_prolog_list(JSONTerm, Prolog, []-_VarAssocFinal).

json_term_to_prolog(['*var*', VarName], Var, VarAssocIn-VarAssocOut) :-
	atom(VarName),
	(   member(VarName-Var, VarAssocIn) ->
	    VarAssocOut = VarAssocIn
	;
	    otherwise ->
	    VarAssocOut = [VarName-Var | VarAssocIn]
	),
	!.
json_term_to_prolog(null, [], VarAssocIn-VarAssocIn) :-
	!.
json_term_to_prolog(Atom, Atom, VarAssocIn-VarAssocIn) :-
	atom(Atom),
	!.
json_term_to_prolog(N, N, VarAssocIn-VarAssocIn) :-
	number(N),
	!.
%json_term_to_prolog(['*t*', TagName | JSONArgs], Term, VarAssocIn-VarAssocOut) :-
%	json_term_to_prolog(TagName, Functor),
%	json_term_list_to_prolog_list(JSONArgs, List, VarAssocIn-VarAssocOut),
%	Term =.. [Functor | List],
%	!.
% Alist
json_term_to_prolog({JSONFunctor:JSONArgs}, Term, VarAssocIn-VarAssocOut) :-
	atom(JSONFunctor),
	atom_codes(JSONFunctor, [0'* | String]),
	atom_codes(Functor, String),
	is_list(JSONArgs),
	json_term_list_to_prolog_list(JSONArgs, Args, VarAssocIn-VarAssocOut),
	Term =.. [Functor | Args],
	!.
% Alist
json_term_to_prolog(JSONCurlyBracketList, List, VarAssocIn-VarAssocOut) :-
	JSONCurlyBracketList \== [],
	is_curly_bracket_list(JSONCurlyBracketList),
	curly_bracket_list_to_list(JSONCurlyBracketList, JSONList),
	json_key_value_list_to_prolog_list(JSONList, List, VarAssocIn-VarAssocOut),
	!.
% Special case for list
json_term_to_prolog(JSONList, List, VarAssocIn-VarAssocOut) :-
	is_list(JSONList),
	json_term_list_to_prolog_list(JSONList, List, VarAssocIn-VarAssocOut),
	!.
% Special case for key/val pair, e.g. foo=bar
json_term_to_prolog({"key":TagName, "value":JSONValue}, (Key = Value), VarAssocIn-VarAssocOut) :-
	json_term_to_prolog(TagName, Key),
	json_term_to_prolog(JSONValue, Value, VarAssocIn-VarAssocOut),
	!.
json_term_to_prolog(JSONTerm, Prolog, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [json_term_to_prolog(JSONTerm, Prolog, VarAssoc)]),
	fail.

json_term_list_to_prolog_list([], [], VarAssocIn-VarAssocIn) :-
	!.
json_term_list_to_prolog_list([F | R], [F1 | R1], VarAssocIn-VarAssocOut) :-
	json_term_to_prolog(F, F1, VarAssocIn-VarAssocNext),
	!,
	json_term_list_to_prolog_list(R, R1, VarAssocNext-VarAssocOut).
json_term_list_to_prolog_list(JSONList, List, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [json_term_list_to_prolog_list(JSONList, List, VarAssoc)]),
	fail.

json_key_value_list_to_prolog_list([], [], VarAssocIn-VarAssocIn).
json_key_value_list_to_prolog_list([F | R], [F1 | R1], VarAssocIn-VarAssocOut) :-
	json_key_value_list_element_to_prolog(F, F1, VarAssocIn-VarAssocNext),
	!,
	json_key_value_list_to_prolog_list(R, R1, VarAssocNext-VarAssocOut).
json_key_value_list_to_prolog_list(JSONList, List, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [json_key_value_list_to_prolog_list(JSONList, List, VarAssoc)]),
	fail.

json_key_value_list_element_to_prolog(JSONKey:JSONValue, Key=Value, VarAssocIn-VarAssocOut) :-
	json_term_to_prolog(JSONKey, Key, VarAssocIn-VarAssocNext),
	json_term_to_prolog(JSONValue, Value, VarAssocNext-VarAssocOut),
	!.
json_key_value_list_element_to_prolog(F, F1, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [json_key_value_list_element_to_prolog(F, F1, VarAssoc)]),
	fail.

%------------------------------------------------------------------------------------

json_tag_string_to_string(TagString, String) :-
	string_to_json_tag_string(String, TagString).

string_to_json_tag_string([], []).
string_to_json_tag_string([F | R], Result) :-
	escape_sequence_for_char(F, EscapeSequence),
	append(EscapeSequence, R1, Result),
	!,
	string_to_json_tag_string(R, R1).
string_to_json_tag_string([F | R], [F | R1]) :-
	!,
	string_to_json_tag_string(R, R1).

escape_sequence_for_char(_F, _EscapeSequence) :-
	fail.

%------------------------------------------------------------------------------------

