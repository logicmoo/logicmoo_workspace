
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(stepper_read,
	  [read_stepper_command/2,
	   interpret_stepper_command_chars/2,
	   choose_from_stepper_text_menu/3,
	   choose_number_from_list/4, 
	   get_stepper_confirmation/2,
	   set_answer_string/1,
	   get_answer_string/1,
	   handle_exception_for_java_gui/3]
	 ).
 
%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------

read_stepper_command(Command, LiteralCommand) :-
	format('~N~nSTEPPER>> ', []),
	read_line(user, Chars),
	atom_codes(LiteralCommand, Chars),
	(   interpret_stepper_command_chars(Chars, Command) ->
	    true ;

	    Command = unknown_command(LiteralCommand)
	),
	!.
 
interpret_stepper_command_chars(Chars, Command) :-
	tokenise(Chars, Words),
	stepper_command(Command, Words, []),
	!.

%----------------------------------------------------------------------

choose_from_stepper_text_menu(Question, Alist, Answer) :-
	format('~N~n', []),
	length(Alist, NChoices),
	display_stepper_menu_alist(Alist, 1),
	%format('~N~n~w ', [Question]),
	%read_line(user, Chars),
	alist_to_text_list(Alist, TextList), 
	print_prompt_and_get_answer('~N~n~w ', [Question], menu(TextList), Chars),
	tokenise(Chars, Words),
	(   ( Words = [N], integer(N), 0 < N, N =< NChoices ) ->
	    safe_nth(N, Alist, Answer-_Text)
	;
	    otherwise ->
	    format('~N~nPlease give a number from 1 to ~d~n', [NChoices]),
	    choose_from_stepper_text_menu(Question, Alist, Answer)
	).

display_stepper_menu_alist([], _N).
display_stepper_menu_alist([_Answer-Text | R], I) :-
	format('~N ~d~4|~w~n', [I, Text]),
	I1 is I + 1,
	!,
	display_stepper_menu_alist(R, I1).

alist_to_text_list([], []) :-
	!.
alist_to_text_list([_Answer-TextAtom | R], [Text | R1]) :-
	atom_codes(TextAtom, Text),
	!,
	alist_to_text_list(R, R1).
alist_to_text_list(Alist, TextList) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [alist_to_text_list(Alist, TextList)]),
	fail.

%----------------------------------------------------------------------

choose_number_from_list(Question, FailureMessage, List, Choice) :-
	is_list(List),
	%format(Question, [List]),
	%read_line(user, Chars),
	print_prompt_and_get_answer(Question, [List], choose_number(List), Chars),
	tokenise(Chars, Words),
	(   ( Words = [Answer], member(Answer, List) ) ->
	    Answer = Choice ;

	    ( Words = [Answer], format(FailureMessage, [Answer]) ),
	    !,
	    choose_number_from_list(Question, FailureMessage, List, Choice) ;

	    format('~NType a number in the list, followed by <return>', []),
	    !,
	    choose_number_from_list(Question, FailureMessage, List, Choice)

	).

%----------------------------------------------------------------------

get_stepper_confirmation(FormatAtom, Args) :-
	%format(FormatAtom, Args),
	%read_line(user, Chars),
	print_prompt_and_get_answer(FormatAtom, Args, yes_or_no, Chars),
	tokenise(Chars, Words),
	(   Words = [y] ->
	    true ;

	    Words = [n] ->
	    fail ;

	    format('~NPlease answer \'y\' or \'n\'~n', []),
	    !,
	    get_stepper_confirmation(FormatAtom, Args)
	).
 
%----------------------------------------------------------------------

print_prompt_and_get_answer(FormatAtom, Args, QueryDescription, Chars) :-
	format_to_chars(FormatAtom, Args, PromptString),
	format('~s', [PromptString]),
	print_prompt_and_get_answer1(PromptString, QueryDescription, Chars).

print_prompt_and_get_answer1(_PromptString, _QueryDescription, Chars) :-
	get_answer_string(Chars),
	!.
print_prompt_and_get_answer1(PromptString, QueryDescription, Chars) :-
	using_java_gui,
	\+ get_answer_string(Chars),
	signal_attempt_to_get_answer_to_menu_question(PromptString, QueryDescription).
print_prompt_and_get_answer1(_PromptString, _QueryDescription, Chars) :-
	read_line(user, Chars).

%----------------------------------------------------------------------

:- dynamic menu_answer_string/1.

set_answer_string(Chars) :-
	retractall(menu_answer_string(_)),
	assertz(menu_answer_string(Chars)).

get_answer_string(Chars) :-
	menu_answer_string(Chars),
	is_prolog_string(Chars),
	Chars \== "".

%----------------------------------------------------------------------

using_java_gui :-
	current_predicate(user:using_java_gui/0),
	user:using_java_gui.

signal_attempt_to_get_answer_to_menu_question(PromptString, QueryDescription) :-
	raise_exception(attempt_to_get_answer_to_menu_question(PromptString, QueryDescription)).

handle_exception_for_java_gui(Exception, Status, QueryDescriptionForGUI) :-
	Exception = attempt_to_get_answer_to_menu_question(PromptString, QueryDescription),
	format_query_description_for_java_gui(QueryDescription, PromptString, QueryDescriptionForGUI),
	Status = ok,
	!.
handle_exception_for_java_gui(Exception, Status, QueryDescriptionForGUI) :-
	inform_about_top_level_regulus_error(Exception),
	QueryDescriptionForGUI = error,
	Status = error.

%----------------------------------------------------------------------

format_query_description_for_java_gui(yes_or_no, PromptString,
				      yes_or_no(PromptString)) :-
	!.
format_query_description_for_java_gui(choose_number(List), PromptString,
				      choose_number(PromptString, ListTerm)) :-
	ListTerm =.. [list | List],
	!.
format_query_description_for_java_gui(menu(List), PromptString,
				      choose_from_menu(PromptString, ListTerm)) :-
	ListTerm =.. [list | List],
	!.
format_query_description_for_java_gui(X, Y, Z) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [format_query_description_for_java_gui(X, Y, Z)]),
	fail.

%----------------------------------------------------------------------

tokenise(Chars, Words) :-
	split_string_into_words(Chars, Words0),
	convert_atoms_to_numbers_where_possible(Words0, Words).

convert_atoms_to_numbers_where_possible([], []).
convert_atoms_to_numbers_where_possible([F | R], [F1 | R1]) :-
	(   atom_to_int(F, F1) ->
	    true ;

	    F1 = F
	),
	!,
	convert_atoms_to_numbers_where_possible(R, R1).

%----------------------------------------------------------------------

stepper_command(help) -->
	['HELP'].
	
stepper_command(lex(LexExpression)) -->
	['LEX'],
	non_trivial_word_sequence(LexExpressionList),
	{list_to_comma_list(LexExpressionList, LexExpression)}.

stepper_command(gap) -->
	['GAP'].

stepper_command(debug) -->
	['DEBUG'].

stepper_command(edit) -->
	['EDIT'].

stepper_command(load) -->
	['LOAD'].

stepper_command(load_generation) -->
	['LOAD_GENERATION'].

stepper_command(ebl_load) -->
	['EBL_LOAD'].

stepper_command(ebl_load_generation) -->
	['EBL_LOAD_GENERATION'].

stepper_command(parse(Atom)) -->
	['PARSE'],
	non_trivial_word_sequence(Words),
	{join_with_spaces(Words, Atom)}.

stepper_command(generate(ID)) -->
	['GENERATE'],
	integer(ID),
	!.

stepper_command(combine(IDs)) -->
	['COMBINE'],
	non_trivial_integer_sequence(IDs).

stepper_command(cut(ID, Node)) -->
	['CUT'],
	integer(ID),
	integer(Node).

stepper_command(uncut(ID, Node)) -->
	['UNCUT'],
	integer(ID),
	integer(Node).

stepper_command(join(ID1, Node, ID2)) -->
	['JOIN'],
	integer(ID1),
	integer(Node),
	integer(ID2).

stepper_command(join(ID1, ID2)) -->
	['JOIN'],
	integer(ID1),
	integer(ID2).

stepper_command(show(ID)) -->
	['SHOW'],
	integer(ID).

stepper_command(show(ID, Node)) -->
	['SHOW'],
	integer(ID),
	integer(Node).

stepper_command(rule(ID, Node)) -->
	['RULE'],
	integer(ID),
	integer(Node).

stepper_command(delete(ID)) -->
	['DELETE'],
	integer(ID),
	!.

stepper_command(delete(IDs)) -->
	['DELETE'],
	non_trivial_integer_sequence(IDs).

stepper_command(delete_all) -->
	['DELETE_ALL'].

stepper_command(summary) -->
	['SUMMARY'].

stepper_command(exit) -->
	['EXIT'].

non_trivial_word_sequence([F | R]) -->
	[F0],
	{coerce_int_to_atom(F0, F)},
	word_sequence(R).

word_sequence([F | R]) -->
	[F0],
	{coerce_int_to_atom(F0, F)},
	!,
	word_sequence(R).
word_sequence([]) -->
	[].

non_trivial_integer_sequence([F | R]) -->
	integer(F),
	integer_sequence(R).

integer_sequence([F | R]) -->
	integer(F),
	!,
	integer_sequence(R).
integer_sequence([]) -->
	[].

integer(N) -->
	[N],
	{integer(N)}.







	