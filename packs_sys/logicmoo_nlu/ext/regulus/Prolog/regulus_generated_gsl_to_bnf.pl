:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_generated_gsl_to_bnf,
	  [test_bnf/1,
	   regulus_generated_gsl_to_bnf/3
	  ]).

:- use_module('$REGULUS/Prolog/parse_regulus_generated_gsl').

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%-----------------------------------------------------------------

/*

regulus_generated_gsl_to_bnf(+GSLGrammarFile, +GrammarName, +BNFGrammarFile)

- GSLGrammarFile is a file containing Regulus-generated GSL
- GrammarName is an atom, which will be used for the BNF grammar name
- BNFGrammarFile is the generated BNF grammar file

*/

test_bnf(toy1) :-
	regulus_generated_gsl_to_bnf('$REGULUS/Examples/Toy1/Generated/recogniser.grammar',
				     toy1,
				     '$REGULUS/Examples/Toy1/Generated/recogniser.bnf').

test_bnf(jackie_static) :-
	regulus_generated_gsl_to_bnf('$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_static.grammar',
				     jackie_static,
				     '$JACKIE/Generated/pcfg_trained/jackie_specialised_recogniser_static.bnf').

%-----------------------------------------------------------------

regulus_generated_gsl_to_bnf(InFile, GrammarName, OutFile) :-
	tmp_regulus_file('regulus_gsl.pl', PrologGSLFile),

	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	safe_absolute_file_name(PrologGSLFile, AbsPrologGSLFile),
	
	parse_regulus_generated_gsl(AbsInFile, AbsPrologGSLFile),

	prolog_file_to_list(AbsPrologGSLFile, PrologGSLList),
	length(PrologGSLList, NIn),
	format('~N--- Read Prolog GSL file (~d records) ~w~n', [NIn, AbsPrologGSLFile]),

	prolog_gsl_list_to_bnf(PrologGSLList, GrammarName, BNFList),
	length(BNFList, NOut),

	write_atom_list_to_file(BNFList, AbsOutFile),
	format('~N--- Written BNF file (~d lines) ~w~n', [NOut, AbsOutFile]),
	!.

%-----------------------------------------------------------------

prolog_gsl_list_to_bnf(PrologGSLList, GrammarName, BNFList) :-
	bnf_start_decls(PrologGSLList, GrammarName, BNFStartDecls),
	prolog_gsl_list_to_bnf1(PrologGSLList, BNFListMain),
	append(BNFStartDecls, BNFListMain, BNFList),
	!.

prolog_gsl_list_to_bnf1([], []).
prolog_gsl_list_to_bnf1([F | R], [F1 | R1]) :-
	prolog_gsl_to_bnf(F, F1),
	!,
	prolog_gsl_list_to_bnf1(R, R1).

bnf_start_decls(PrologGSLList, GrammarName, BNFStartDecls) :-
	header_line(HeaderLine),
	grammar_name_line(GrammarName, GrammarNameLine),
	start_grammar_lines(PrologGSLList, StartGrammarLines),
	append([HeaderLine, GrammarNameLine], StartGrammarLines, BNFStartDecls),
	!.

header_line(HeaderLine) :-
	HeaderLine = '#BNF+EMV1.1;',
	!.

grammar_name_line(GrammarName, GrammarNameLine) :-
	format_to_atom('!grammar ~w;', [GrammarName], GrammarNameLine),
	!.

start_grammar_lines([], []).
start_grammar_lines([F | R], [F1 | R1]) :-
	start_grammar_rule(F, F1),
	!,
	start_grammar_lines(R, R1).
start_grammar_lines([_F | R], R1) :-
	!,
	start_grammar_lines(R, R1).

/*
rule_group('.MAIN',[rule(no_prob,[non_terminal('UTTERANCE',v_0)],'  < value $v_0 > ')]).
*/

start_grammar_rule(rule_group(GrammarName, _), StartRule) :-
	strip_leading_period_from_atom(GrammarName, RestGrammarName),
	gsl_grammar_name_to_bnf(RestGrammarName, BNFGrammarName),
	format_to_atom('!start ~w;', [BNFGrammarName], StartRule),
	!.

%-----------------------------------------------------------------

/*

rule_group('COMMAND', 
           [rule(no_prob, 
                 [non_terminal('VERB_SWITCHABLE_ANY_IMPERATIVE_SWITCH',v_0), 
                  non_terminal('ONOFF',v_1), non_terminal('NP_SWITCHABLE_ANY',v_2)],
                 'return( concat( ( ( utterance_type command ) ) concat( $v_0 concat( $v_1 $v_2 ) ) ) )'),
            rule(no_prob, [terminal(dim),non_terminal('NP_DIMMABLE_ANY',v_0)], 
                 'return( concat( ( ( utterance_type command ) ) concat( ( ( action dim ) ) $v_0 ) ) )')]).

<COMMAND> : <VERB_SWITCHABLE_ANY_IMPERATIVE_SWITCH> <ONOFF> <NP_SWITCHABLE_ANY> | dim <NP_DIMMABLE_ANY> ;

*/

prolog_gsl_to_bnf(Rule, BNFAtom) :-
	with_output_to_chars(prolog_gsl_to_bnf1(Rule), BNFChars),
	atom_codes(BNFAtom, BNFChars),
	!.
prolog_gsl_to_bnf(Rule, BNFAtom) :-
	format('~N*** Error: bad call: ~w~n', [prolog_gsl_to_bnf(Rule, BNFAtom)]),
	fail.

prolog_gsl_to_bnf1(rule_group(LHSGrammar, RHSList)) :-
	(   strip_leading_period_from_atom(LHSGrammar, LHSGrammar1) ->
	    true
	;
	    otherwise ->
	    LHSGrammar = LHSGrammar1
	),
	gsl_grammar_name_to_bnf(LHSGrammar1, BNFLHSGrammar),
	format('~N~w : ', [BNFLHSGrammar]),
	prolog_gsl_rhs_to_bnf(RHSList),
	format(';', []).

prolog_gsl_rhs_to_bnf([]) :-
	!.
prolog_gsl_rhs_to_bnf([Last]) :-
	!,
	prolog_gsl_rhs_rule_to_bnf(Last).
prolog_gsl_rhs_to_bnf([F | R]) :-
	prolog_gsl_rhs_rule_to_bnf(F),
	format(' | ', []),
	!,
	prolog_gsl_rhs_to_bnf(R).

/*

 rule(no_prob, [terminal(dim),non_terminal('NP_DIMMABLE_ANY',v_0)], 
                 'return( concat( ( ( utterance_type command ) ) concat( ( ( action dim ) ) $v_0 ) ) )'

 dim <NP_DIMMABLE_ANY>

*/ 

prolog_gsl_rhs_rule_to_bnf(rule(_Prob, List, _Sem)) :-
	prolog_gsl_rhs_rule_body_to_bnf(List),
	!.
prolog_gsl_rhs_rule_to_bnf(Other) :-
	format('~N*** Error: bad call: ~w~n', [prolog_gsl_rhs_rule_to_bnf(Other)]),
	fail.

prolog_gsl_rhs_rule_body_to_bnf([]) :-
	!.
prolog_gsl_rhs_rule_body_to_bnf([Last]) :-
	prolog_gsl_rhs_item_to_bnf(Last),
	!.
prolog_gsl_rhs_rule_body_to_bnf([F | R]) :-
	prolog_gsl_rhs_item_to_bnf(F),
	format(' ', []),
	!,
	prolog_gsl_rhs_rule_body_to_bnf(R).

prolog_gsl_rhs_item_to_bnf(terminal(Terminal)) :-
	format('~w', [Terminal]),
	!.
prolog_gsl_rhs_item_to_bnf(non_terminal(NonTerminal, _Sem)) :-
	gsl_grammar_name_to_bnf(NonTerminal, BNFNonTerminal),
	format('~w', [BNFNonTerminal]),
	!.
prolog_gsl_rhs_item_to_bnf(F) :-
	format('~N*** Error: bad call: ~w~n', [prolog_gsl_rhs_item_to_bnf(F)]),
	fail.

%-----------------------------------------------------------------

strip_leading_period_from_atom(GrammarName, RestGrammarName) :-	
	atom(GrammarName),
	atom_codes(GrammarName, GrammarNameCodes),
	GrammarNameCodes = [0'. | RestGrammarNameCodes],
	atom_codes(RestGrammarName, RestGrammarNameCodes),
	!.

gsl_grammar_name_to_bnf(GrammarName, BNFGrammarName) :-
	atom(GrammarName),
	format_to_atom('<~w>', [GrammarName], BNFGrammarName),
	!.
gsl_grammar_name_to_bnf(GrammarName, BNFGrammarName) :-
	format('~N*** Error: bad call: ~w~n', [gsl_grammar_name_to_bnf(GrammarName, BNFGrammarName)]),
	fail.
