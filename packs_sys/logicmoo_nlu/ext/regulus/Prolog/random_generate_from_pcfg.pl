
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(random_generate_from_pcfg,
	  [test/1,
	   parse_pcfg_file/1,
	   random_generate/5
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(random)).

%------------------------------------------------------------------------------------

test(parse_taxi) :-
	parse_pcfg_file('$TAXI_DRIVER/Generated/pcfg_trained/recogniser.grammar').
test(parse_medslt) :-
	parse_pcfg_file('$MED_SLT2/Eng/GeneratedFiles/pcfg_trained/recogniser.grammar').

test(generate_taxi) :-
	random_generate('$TAXI_DRIVER/Generated/pcfg_trained/recogniser.grammar',
			'.MAIN',
			10000,
			[],
			'$TAXI_DRIVER/corpora/random_sents.txt').
test(generate_medslt) :-
	random_generate('$MED_SLT2/Eng/GeneratedFiles/pcfg_trained/recogniser.grammar',
			'.MAIN',
			1000,
			[dont_expand='POLITENESS_PRE_NONE', dont_expand='UTTERANCE_ELLIPSIS_N'],
			'$MED_SLT2/corpora/headache_random_pcfg.txt').

%------------------------------------------------------------------------------------

random_generate(GrammarFile, StartSymbol, N, OtherArgs, OutputFile) :-
	set_random_generator_state_from_time,
	
	absolute_file_name(OutputFile, AbsOutputFile),
	parse_pcfg_file(GrammarFile),

	format('~N~n--- Start generating from ~w~n', [StartSymbol]),
	open(AbsOutputFile, write, S),
	random_generate_to_stream(StartSymbol, OtherArgs, S, 0-N),
	close(S),
	format('~N~n--- Finished, generated ~d examples~n', [N]),
	!.
random_generate(GrammarFile, StartSymbol, N, OtherArgs, OutputFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [random_generate(GrammarFile, StartSymbol, N, OtherArgs, OutputFile)]),
	fail.

%------------------------------------------------------------------------------------

random_generate_to_stream(_StartSymbol, _OtherArgs, _S, I-N) :-
	I >= N,
	!.
random_generate_to_stream(StartSymbol, OtherArgs, S, I-N) :-
	random_generate(non_terminal(StartSymbol), OtherArgs, Words-[]),
	join_with_spaces(Words, Atom),
	format(S, '~N~w~n', [Atom]),
	I1 is I + 1,
	(   0 is I1 mod 1000 ->
	    format('~d ', [I1]),
	    flush_output(user)
	;
	    true
	),
	!,
	random_generate_to_stream(StartSymbol, OtherArgs, S, I1-N).

%------------------------------------------------------------------------------------

random_generate(terminal(Word), _OtherArgs, WordsIn-WordsOut) :-
	WordsIn = [Word | WordsOut],
	!.
random_generate(non_terminal(Symbol), OtherArgs, WordsIn-WordsOut) :-
	member(dont_expand=Symbol, OtherArgs),
	WordsIn = [Symbol | WordsOut],
	!.
random_generate(non_terminal(Symbol), OtherArgs, WordsIn-WordsOut) :-
	pcfg_rule(Symbol, Rules),
	random(Index),
	choose_rule(0, Index, Rules, ChosenRuleBody),
	!,
	random_generate_body(ChosenRuleBody, OtherArgs, WordsIn-WordsOut).

random_generate_body([], _OtherArgs, WordsIn-WordsIn) :-
	!.
random_generate_body([F | R], OtherArgs, WordsIn-WordsOut) :-
	random_generate(F, OtherArgs, WordsIn-WordsNext),
	!,
	random_generate_body(R, OtherArgs, WordsNext-WordsOut).

choose_rule(TotalIn, Index, [rule(Prob, Rule) | R], ChosenRuleBody) :-
	TotalNext is TotalIn + Prob,
	(   TotalNext >= Index ->
	    ChosenRuleBody = Rule
	;
	    otherwise ->
	    choose_rule(TotalNext, Index, R, ChosenRuleBody)
	).

%------------------------------------------------------------------------------------

:- dynamic pcfg_rule/2.

parse_pcfg_file(GrammarFile) :-
	safe_absolute_file_name(GrammarFile, AbsGrammarFile),
	retractall(pcfg_rule(_, _)),
	open(AbsGrammarFile, read, S),
	format('~N~n--- Parsing PCFG grammar from ~w~n', [AbsGrammarFile]),
	parse_pcfg_file1(S, waiting_for_rule, 0-N),
	close(S),
	format('~N~n--- Parsed grammar, ~d non-terminals~n', [N]),
	!.

parse_pcfg_file1(S, InState, InN-OutN) :-
	read_and_parse_line(S, ParsedLine),
	process_parsed_line(ParsedLine, InState-NextState, InN-NextN),
	(   NextN > InN, 0 is NextN mod 100 ->
	    format('~d ', [NextN]),
	    flush_output(user)
	;
	    true
	),
	!,
	parse_pcfg_file2(ParsedLine, S, NextState, NextN-OutN).

parse_pcfg_file2(end_of_file, _S, _InState, InN-InN) :-
	!.
parse_pcfg_file2(_ParsedLine, S, InState, InN-OutN) :-
	parse_pcfg_file1(S, InState, InN-OutN).

read_and_parse_line(S, ParsedLine) :-
	read_line(S, Line),
	parse_line(Line, ParsedLine).

parse_line(end_of_file, end_of_file) :-
	!.
parse_line(Line, ParsedLine) :-
	line(ParsedLine, Line, []),
	!.
parse_line(Line, _ParsedLine) :-
	format2error('~N*** Error: unable to parse line "~s"~n', [Line]),
	fail.

/*
Typical rule:

NP_N_N_SUBJ_Y_NULL_NULL_NORMAL_N_N_N_N_N_N_N_N_N_N_N_N_N_N_DEGREE_SYMPTOM_NONE_3_SING
[
( the N_ANY_N_N_N_N_N_N_N_N_N_N_N_N_N_DEF_DEGREE_ANY_ANY_SYMPTOM_NONE_3_SING:v_0 )~0.766666666667
     {return( $v_0 )}
( POSSESSIVE:v_0 N_ANY_N_N_N_N_N_N_N_N_N_N_N_N_N_DEF_DEGREE_ANY_ANY_SYMPTOM_NONE_3_SING:v_1 )~0.116666666667
     {return( concat( $v_0 $v_1 ) )}
( D_N_ANY_ANY_N_DEF_3_SING:v_0 N_ANY_N_N_N_N_N_N_N_N_N_N_N_N_N_DEF_DEGREE_ANY_ANY_SYMPTOM_NONE_3_SING:v_1 )~0.116666666667
     {return( concat( ( ( spec $v_0 ) ) $v_1 ) )}
]
*/

%------------------------------------------------------------------------------------

% process_parsed_line(ParsedLine, InState-OutState, InN-OutN)

process_parsed_line(empty, InState-InState, InN-InN) :-
	!.
process_parsed_line(non_terminal(NonTerminal), waiting_for_rule-started_rule(NonTerminal), InN-InN) :-
	!.
process_parsed_line(left_square_bracket,
                    started_rule(NonTerminal)-filling_rule(NonTerminal, []),
		    InN-InN) :-
	!.
process_parsed_line(right_square_bracket,
                    filling_rule(NonTerminal, Rules)-waiting_for_rule,
		    InN-OutN) :-
	reverse(Rules, ReversedRules),
	assertz(pcfg_rule(NonTerminal, ReversedRules)),
	OutN is InN + 1,
	!.
process_parsed_line(semantic_return_value, InState-InState, InN-InN) :-
	!.
process_parsed_line(rule(Prob, Body),
                    filling_rule(NonTerminal, InRules)-filling_rule(NonTerminal, [rule(Prob, Body) | InRules]),
		    InN-InN) :-
	!.
process_parsed_line(end_of_file, InState-InState, InN-InN) :-
	!.
process_parsed_line(ParsedLine, State, N) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [process_parsed_line(ParsedLine, State, N)]),
	fail.

%------------------------------------------------------------------------------------

line(empty) -->
	whitespace_or_empty.
line(non_terminal(NonTerminal)) -->
	non_terminal(NonTerminal).
line(left_square_bracket) -->
	"[",
	whitespace_or_empty.
line(right_square_bracket) -->
	"]",
	whitespace_or_empty.
line(semantic_return_value) -->
	whitespace_or_empty,
	"{",
	anything.
line(rule(Prob, Body)) -->
	"(",
	whitespace_or_empty,
	body(Body),
	whitespace_or_empty,
	")~",
	probability_value(Prob).

body([F | R]) -->
	body_element(F),
	whitespace_or_empty,
	!,
	body(R).
body([]) -->
	whitespace_or_empty.

body_element(non_terminal(NonTerminal)) -->
	non_terminal(NonTerminal),
	optional_associated_var,
	!.
body_element(terminal(Terminal)) -->
	terminal(Terminal).

probability_value(DecimalNumber) -->
	"0.",
	digit_sequence(DigitsAfterPoint),
	{safe_number_codes(DecimalNumber, [0'0, 0'. | DigitsAfterPoint])},
	!.
probability_value(1) -->
	"1",
	!.

non_terminal(NonTerminal) -->
	".",
	nuance_uppercase_char(F),
	non_special_char_sequence(R),
	!,
	{atom_codes(NonTerminal, [0'., F | R])}.
non_terminal(NonTerminal) -->
	nuance_uppercase_char(F),
	non_special_char_sequence(R),
	!,
	{atom_codes(NonTerminal, [F | R])}.
	
terminal(Terminal) -->
	nuance_lowercase_char(F),
	non_special_char_sequence(R),
	!,
	{atom_codes(Terminal, [F | R])}.
	
optional_associated_var -->
	":",
	non_special_char_sequence(_),
	whitespace_or_empty,
	!.
optional_associated_var -->
	whitespace_or_empty.

non_special_char_sequence([F | R]) -->
	non_special_char(F),
	!,
	non_special_char_sequence(R).
non_special_char_sequence([]) -->
	[].

digit_sequence([F | R]) -->
	digit_char(F),
	!,
	digit_sequence(R).
digit_sequence([]) -->
	[].

whitespace_or_empty -->
	whitespace_char(_),
	!,
	whitespace_or_empty.
whitespace_or_empty -->
	[].

anything -->
	[_Any],
	!,
	anything.
anything -->
	[].

whitespace_char(Char) -->
	[Char],
	{whitespace_char(Char)}.

uppercase_char(Char) -->
	[Char],
	{uppercase_char(Char)}.

nuance_uppercase_char(Char) -->
	[Char],
	{nuance_uppercase_char(Char)}.

nuance_lowercase_char(0'\') -->
	[0'\'].
nuance_lowercase_char(Char) -->
	[Char],
	%{lowercase_char(Char)}.
	{nuance_lowercase_char(Char)}.

nuance_uppercase_char(Char) :-
	uppercase_char(Char),
	\+ accented_char(Char).

digit_char(Char) -->
	[Char],
	{digit_char(Char)}.

non_special_char(Char) -->
	[Char],
	{non_special_char(Char)}.

nuance_lowercase_char(Char) :-
	\+ uppercase_char(Char),
	\+ special_char(Char).
nuance_lowercase_char(Char) :-
	accented_char(Char),
	\+ special_char(Char).

non_uppercase_non_special_char(Char) :-
	\+ uppercase_char(Char),
	\+ special_char(Char).

non_special_char(Char) :-
	\+ special_char(Char).

special_char(0' ).
special_char(0'\n).
special_char(0'\t).
special_char(0':).
special_char(0'~).
special_char(0'().
special_char(0')).
special_char(0'[).
special_char(0']).
special_char(0'{).
special_char(0'}).
special_char(0'$).
special_char(0'~).
	    