
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(parse_regulus_generated_gsl,
	  [test/1,
	   parse_regulus_generated_gsl/2,
	   convert_parsed_gsl_to_normal_gsl/2
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%------------------------------------------------------------------------------------


test(parse_toy1) :-
	parse_regulus_generated_gsl('$REGULUS/Examples/Toy1/Generated/recogniser.grammar',
				    '$REGULUS/Examples/Toy1/Generated/recogniser.pl').
test(print_parsed_toy1) :-
	convert_parsed_gsl_to_normal_gsl('$REGULUS/Examples/Toy1/Generated/recogniser.pl',
					 '$REGULUS/Examples/Toy1/Generated/recogniser_from_parsed.grammar').

	
test(parse_jackie_dynamic) :-
	parse_regulus_generated_gsl('$JACKIE/Generated/jackie_specialised_recogniser_dynamic.grammar',
				    '$JACKIE/Generated/jackie_specialised_recogniser_dynamic.pl').
test(print_jackie_dynamic) :-
	convert_parsed_gsl_to_normal_gsl('$JACKIE/Generated/jackie_specialised_recogniser_dynamic.pl',
					 '$JACKIE/Generated/jackie_specialised_recogniser_dynamic_from_parsed.grammar').

test(parse_jackie_pcfg_static) :-
	parse_regulus_generated_gsl('$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_static.grammar',
				    '$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_static.pl').

test(parse_role_marked_ara) :-
	parse_regulus_generated_gsl('$MED_SLT2/Ara/GeneratedFiles/pcfg_trained_role_marked/specialised_recogniser_role_marked.grammar',
				    '$MED_SLT2/Ara/GeneratedFiles/pcfg_trained_role_marked/specialised_recogniser_role_marked.pl').

%------------------------------------------------------------------------------------

parse_regulus_generated_gsl(GrammarFile, ParsedFile) :-
	safe_absolute_file_name(GrammarFile, AbsGrammarFile),
	safe_absolute_file_name(ParsedFile, AbsParsedFile),
	open(AbsGrammarFile, read, SIn),
	open(AbsParsedFile, write, SOut),
	
	format('~N~n--- Parsing GSL grammar from ~w~n', [AbsGrammarFile]),
	parse_gsl1(SIn, waiting_for_rule_group, SOut, 0-N),
	
	close(SIn),
	close(SOut),
	format('~N~n--- Written parsed grammar (~d non-terminals) to ~w~n', [N, AbsParsedFile]),
	!.

convert_parsed_gsl_to_normal_gsl(ParsedFile, GrammarFile) :-
	safe_absolute_file_name(ParsedFile, AbsParsedFile),
	safe_absolute_file_name(GrammarFile, AbsGrammarFile),
	open(AbsParsedFile, read, SIn),
	open(AbsGrammarFile, write, SOut),
	
	format('~N~n--- Reading parsed GSL grammar from ~w~n', [AbsParsedFile]),
	print_parsed_gsl(SIn, SOut, 0-N),
	
	close(SIn),
	close(SOut),
	format('~N~n--- Written normal grammar (~d non-terminals) to ~w~n', [N, AbsGrammarFile]),
	!.

%------------------------------------------------------------------------------------

parse_gsl1(SIn, InState, SOut, InN-OutN) :-
	read_and_parse_line(SIn, ParsedLine),
	process_parsed_line(ParsedLine, InState-NextState, SOut, InN-NextN),
	(   NextN > InN, 0 is NextN mod 100 ->
	    format('~d ', [NextN]),
	    flush_output(user)
	;
	    true
	),
	!,
	parse_gsl2(ParsedLine, SIn, NextState, SOut, NextN-OutN).

parse_gsl2(end_of_file, _SIn, _InState, _SOut, InN-InN) :-
	!.
parse_gsl2(_ParsedLine, SIn, InState, SOut, InN-OutN) :-
	parse_gsl1(SIn, InState, SOut, InN-OutN).

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

%------------------------------------------------------------------------------------

print_parsed_gsl(SIn, SOut, InN-OutN) :-
	read(SIn, ParsedGSL),
	(   ParsedGSL = end_of_file ->
	    OutN = InN
	;
	    otherwise ->
	    print_parsed_gsl_item(SOut, ParsedGSL),
	    NextN is InN + 1,
	    (   NextN > InN, 0 is NextN mod 100 ->
		format('~d ', [NextN]),
		flush_output(user)
	    ;
		true
	    ),
	    !,
	    print_parsed_gsl(SIn, SOut, NextN-OutN)
	).

%------------------------------------------------------------------------------------

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

% process_parsed_line(ParsedLine, InState-OutState, InN-OutN)

process_parsed_line(empty, InState-InState, _SOut, InN-InN) :-
	!.
process_parsed_line(non_terminal(NonTerminal), waiting_for_rule_group-started_rule_group(NonTerminal), _SOut, InN-InN) :-
	!.
process_parsed_line(left_square_bracket,
                    started_rule_group(NonTerminal)-filling_rule_group(NonTerminal, []),
		    _SOut, 
		    InN-InN) :-
	!.
process_parsed_line(right_square_bracket,
                    filling_rule_group(NonTerminal, Rules)-waiting_for_rule_group,
		    SOut, 
		    InN-OutN) :-
	reverse(Rules, ReversedRules),
	format(SOut, '~N~n', []),
	prettyprintq_to_stream_unlimited_depth(SOut, rule_group(NonTerminal, ReversedRules)),
	format(SOut, '.~n', []),
	OutN is InN + 1,
	!.
process_parsed_line(syn_rule(Prob, Body),
                    filling_rule_group(NonTerminal, InRules)-
		   [filling_rule_group(NonTerminal, InRules), syn_rule(Prob, Body)],
		    _SOut,
		    InN-InN) :-
	!.
process_parsed_line(syn_rule(NewProb, NewBody),
                    [filling_rule_group(NonTerminal, InRules), syn_rule(Prob, Body)]-
		   [filling_rule_group(NonTerminal, [rule(Prob, Body, no_sem) | InRules]), syn_rule(NewProb, NewBody)],
		    _SOut,
		    InN-InN) :-
	!.
process_parsed_line(semantic_return_value(Sem),
                    [filling_rule_group(NonTerminal, InRules), syn_rule(Prob, Body)]-
		   filling_rule_group(NonTerminal, [rule(Prob, Body, Sem) | InRules]),
		    _SOut,
		    InN-InN) :-
	!.
process_parsed_line(end_of_file, InState-InState, _SOut, InN-InN) :-
	!.
process_parsed_line(ParsedLine, State, SOut, N) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [process_parsed_line(ParsedLine, State, SOut, N)]),
	fail.

%------------------------------------------------------------------------------------

/*
Typical parsed rules:

rule_group(public_rule('MAIN'), 
           [rule(no_prob,[non_terminal('TOP_USER_COMMAND',v_0)],'  < value $v_0 > ')]).

rule_group('NP_Y_N_SUBJ_N_NULL_NULL_NORMAL_N_N_N_N_N_N_N_N_N_N_N_N_N_N_EXHIBIT_NONE_NONE_3_SING',
           [rule(no_prob, [non_terminal('D_Y_ANY_Y_ANY_ANY_0111111_3_SING',v_0)], 
                 'return( ( ( spec $v_0 ) ) )'),
            rule(no_prob, 
                 [non_terminal('D_Y_ANY_Y_ANY_ANY_0111111_3_SING',v_0), 
                  non_terminal('N_ANY_N_N_N_N_N_N_N_N_N_N_N_N_N_QUANT_EXHIBIT_ANY_ANY_NONE_NONE_3_SING',
                               v_1)],
                 'return( concat( concat( ( ( spec $v_0 ) ) $v_1 ) () ) )'),
            rule(no_prob, 
                 [non_terminal('D_Y_ANY_Y_ANY_ANY_0111111_3_SING',v_0), 
                  non_terminal('TMP_CAT_41_QUANT_EXHIBIT_3_SING',v_1)],
                 'return( concat( concat( ( ( spec $v_0 ) ) $v_1.sem_2 ) $v_1.sem_1 ) )')]).

rule_group('POLITENESS_POST_YNQ', 
           [rule(no_prob,[terminal(please)],'return( ( ( politeness please ) ) )'), 
            rule(no_prob, 
                 external_grammar_non_terminal('../DynamicNuance/DYNAMIC_POLITENESS_POST_YNQ_1.gsl',
                                               v_0),
                 'return( $v_0 )'),
            rule(no_prob,[terminal(benjamin)],'return( ( ( politeness system_name ) ) )')]).		 
*/

print_parsed_gsl_item(S, ParsedGSL) :-
	ParsedGSL = rule_group(LHS, RHS),
	format(S, '~N~n', []),
	print_rule_lhs(S, LHS),
	format(S, '~N[~n', []),
	print_rule_rhs(S, RHS),
	format(S, '~N]~n', []),
	!.
print_parsed_gsl_item(_S, ParsedGSL) :-
	format2error('~N*** Error: unable to print parsed GSL rule:~n', []),
	prettyprintq(ParsedGSL),
	format('~N', []),
	!,
	fail.

print_rule_lhs(S, public_rule(NonTerminal)) :-
	format(S, '~w:public', [NonTerminal]),
	!.
print_rule_lhs(S, NonTerminal) :-
	atom(NonTerminal),
	format(S, '~w', [NonTerminal]),
	!.
print_rule_lhs(S, NonTerminal) :-
	format2error('~N*** Error: bad call: ~w~n', [print_rule_lhs(S, NonTerminal)]),
	fail.

print_rule_rhs(_S, []).
print_rule_rhs(S, [F | R]) :-
	print_rule_rhs_line(S, F),
	!,
	print_rule_rhs(S, R).

/*
rule(no_prob, 
     external_grammar_non_terminal('../DynamicNuance/DYNAMIC_POLITENESS_POST_YNQ_1.gsl',
				   v_0),
     'return( $v_0 )')
*/		 
print_rule_rhs_line(S, rule(Prob, external_grammar_non_terminal(File, Var), Sem)) :-
	format(S, '~N<file:~w>', [File]),
	print_optional_var(S, Var),
	print_optional_prob(S, Prob),
	format(S, '~n', []),
	print_optional_sem(S, Sem),
	!.
/*
rule(no_prob, 
     [non_terminal('D_Y_ANY_Y_ANY_ANY_0111111_3_SING',v_0), 
      non_terminal('N_ANY_N_N_N_N_N_N_N_N_N_N_N_N_N_QUANT_EXHIBIT_ANY_ANY_NONE_NONE_3_SING',
		   v_1)],
     'return( concat( concat( ( ( spec $v_0 ) ) $v_1 ) () ) )')
*/
print_rule_rhs_line(S, rule(Prob, Items, Sem)) :-
	format(S, '~N( ', []),
	print_rhs_items(S, Items),
	format(S, ')', []),
	print_optional_prob(S, Prob),
	format(S, '~n', []),
	print_optional_sem(S, Sem),
	!.
print_rule_rhs_line(S, Rule) :-
	format2error('~N*** Error: bad call to print_rule_rhs_line/2:~n', []),
	prettyprintq(print_rule_rhs_line(S, Rule)),
	fail.

print_rhs_items(_S, []).
print_rhs_items(S, [F | R]) :-
	print_rhs_item(S, F),
	format(S, ' ', []),
	!,
	print_rhs_items(S, R).

print_rhs_item(S, non_terminal(Symbol, Var)) :-
	format(S, '~w', [Symbol]),
	print_optional_var(S, Var),
	!.
print_rhs_item(S, terminal(Symbol)) :-
	format(S, '~w', [Symbol]),
	!.

print_optional_var(_S, no_var) :-
	!.
print_optional_var(S, Var) :-
	format(S, ':~w', [Var]),
	!.

print_optional_prob(_S, no_prob) :-
	!.
print_optional_prob(S, Prob) :-
	format(S, '~~~4f', [Prob]),
	!.

print_optional_sem(_S, no_sem) :-
	!.
print_optional_sem(S, Sem) :-
	format(S, '~N     {~w}~n', [Sem]),
	!.

%------------------------------------------------------------------------------------

line(empty) -->
	whitespace_or_empty.
line(non_terminal(public_rule(NonTerminal))) -->
	non_terminal(NonTerminal),
	":public".
line(non_terminal(NonTerminal)) -->
	non_terminal(NonTerminal).
line(left_square_bracket) -->
	"[",
	whitespace_or_empty.
line(right_square_bracket) -->
	"]",
	whitespace_or_empty.
line(semantic_return_value(Value)) -->
	whitespace_or_empty,
	"{",
	non_curly_bracket_sequence(String),
	"}",
	{atom_codes(Value, String)}.
line(syn_rule(Prob, Body)) -->
	"(",
	whitespace_or_empty,
	body(Body),
	whitespace_or_empty,
	")",
	probability_value(Prob).
line(syn_rule(Prob, ExternalGrammarNonTerminal)) -->
	external_grammar_non_terminal(ExternalGrammarNonTerminal, Prob).
	
body([F | R]) -->
	body_element(F),
	whitespace_or_empty,
	!,
	body(R).
body([]) -->
	whitespace_or_empty.

%<file:../DynamicNuance/DYNAMIC_POLITENESS_POST_YNQ_1.gsl>:v_0
external_grammar_non_terminal(external_grammar_non_terminal(File, Var), Prob) -->
	"<file:",
	non_special_char_sequence(FileString),
	">",
	{atom_codes(File, FileString)},
	optional_associated_var(Var),
	probability_value(Prob),
	!.

body_element(non_terminal(NonTerminal, Var)) -->
	non_terminal(NonTerminal),
	optional_associated_var(Var),
	!.
body_element(terminal(Terminal)) -->
	terminal(Terminal).

probability_value(DecimalNumber) -->
	"~0.",
	digit_sequence(DigitsAfterPoint),
	{safe_number_codes(DecimalNumber, [0'0, 0'. | DigitsAfterPoint])},
	!.
probability_value(1) -->
	"~1",
	!.
probability_value(no_prob) -->
	[],
	!.

non_terminal(NonTerminal) -->
	".",
	uppercase_char(F),
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
	
optional_associated_var(Var) -->
	":",
	non_special_char_sequence(String),
	{atom_codes(Var, String)},
	whitespace_or_empty,
	!.
optional_associated_var(no_var) -->
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

non_curly_bracket_sequence([F | R]) -->
	non_curly_bracket_char(F),
	!,
	non_curly_bracket_sequence(R).
non_curly_bracket_sequence([]) -->
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

digit_char(Char) -->
	[Char],
	{digit_char(Char)}.

non_special_char(Char) -->
	[Char],
	{non_special_char(Char)}.

non_special_char(Char) :-
	\+ special_char(Char).

nuance_lowercase_char(Char) :-
	\+ uppercase_char(Char),
	\+ special_char(Char).
nuance_lowercase_char(Char) :-
	accented_char(Char),
	\+ special_char(Char).

nuance_uppercase_char(Char) :-
	uppercase_char(Char),
	\+ accented_char(Char).

non_curly_bracket_char(Char) -->
	[Char],
	{\+ curly_bracket_char(Char)}.

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
special_char(0'<).
special_char(0'>).

curly_bracket_char(0'{).
curly_bracket_char(0'}).


				