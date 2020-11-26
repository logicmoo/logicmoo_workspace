% check_alterf_patterns.pl

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(lists)).
:- use_module(library(terms)).

%---------------------------------------------------------------

check_alterf_patterns(InFile, GrammarAtom, AlterfSentsFile, AlterfTreebankFile) :-
	absolute_file_name(InFile, AbsInFile),
	format('~N~nChecking Alterf patterns file ~w~n', [AbsInFile]),

	convert_alterf_patterns_file_to_sents_file(AbsInFile, AlterfSentsFile),
	make_ebl_training_data(AlterfSentsFile, GrammarAtom, [], 'C:/tmp/alterf_parsing_history_file.pl', AlterfTreebankFile),
	prolog_file_to_list(AlterfTreebankFile, TreebankList),
	check_alterf_patterns_list(TreebankList, 0-BadCount),

	summarise_alterf_pattern_checking(BadCount),
	!.

%---------------------------------------------------------------

check_alterf_patterns_list([], BadCount-BadCount).
check_alterf_patterns_list([F | R], BadCountIn-BadCountOut) :-
	check_single_alterf_pattern(F, BadCountIn-BadCountNext),
	!,
	check_alterf_patterns_list(R, BadCountNext-BadCountOut).

check_single_alterf_pattern(Term, BadCount) :-
	\+ safe_subsumes_chk(example(_TopCat, _Tree, _LF, _WordList, [_Pattern, _Atom, _Conds]), Term),
	!,
	format2error('~N*** Error: bad call: ~w~n', [check_single_alterf_pattern(Term, BadCount)]),
	fail.
check_single_alterf_pattern(example(_TopCat, _Tree, LF, WordList, [Pattern, Atom, Conds]), BadCountIn-BadCountOut) :-
	LF = 'NO_LF',
	!,
	BadCountOut is BadCountIn + 1,
	reconstruct_alterf_pattern_decl(Pattern, Atom, Conds, WordList, PatternDecl),
	format('~N~n***   ERROR: example sentence fails to parse.', []),
	format('~N*** Pattern: ~q.~n', [PatternDecl]).
check_single_alterf_pattern(example(_TopCat, _Tree, LF, WordList, [Pattern, Atom, Conds]), BadCountIn-BadCountOut) :-
	\+ pattern_matches_subterm_in_lf(LF, Pattern, Conds),
	reconstruct_alterf_pattern_decl(Pattern, Atom, Conds, WordList, PatternDecl),
	!,
	BadCountOut is BadCountIn + 1,
	format('~N~n***   ERROR: pattern fails to match any subterm of LF.', []),
	format('~N*** Pattern: ~q.', [PatternDecl]),
	format('~N***      LF: ~q~n', [LF]).
check_single_alterf_pattern(example(_TopCat, _Tree, _LF, _WordList, [_Pattern, _Atom, _Conds]), BadCount-BadCount).

%---------------------------------------------------------------

pattern_matches_subterm_in_lf(Var, _Pattern, _Conds) :-
	var(Var),
	!,
	fail.
pattern_matches_subterm_in_lf(Atom, Pattern, Conds) :-
	atomic(Atom),
	!,
	Atom = Pattern,
	call(Conds).
pattern_matches_subterm_in_lf(Term, Pattern, Conds) :-
	Term = Pattern,
	call(Conds),
	!.
pattern_matches_subterm_in_lf(Term, Pattern, Conds) :-
	functor(Term, _F, N),
	pattern_matches_subterm_in_lf_args(N, Term, Pattern, Conds).

pattern_matches_subterm_in_lf_args(I, Term, Pattern, Conds) :-
	I > 0,
	arg(I, Term, Arg),
	pattern_matches_subterm_in_lf(Arg, Pattern, Conds).
pattern_matches_subterm_in_lf_args(I, Term, Pattern, Conds) :-
	I > 1,
	I1 is I - 1,
	pattern_matches_subterm_in_lf_args(I1, Term, Pattern, Conds).

%---------------------------------------------------------------

convert_alterf_patterns_file_to_sents_file(AbsInFile, AlterfSentsFile) :-
	prolog_file_to_list(AbsInFile, List),
	convert_alterf_patterns_list_to_sents_list(List, List1),
	list_to_prolog_file(List1, AlterfSentsFile),
	!.
convert_alterf_patterns_file_to_sents_file(AbsInFile, AlterfSentsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [convert_alterf_patterns_file_to_sents_file(AbsInFile, AlterfSentsFile)]),
	fail.

convert_alterf_patterns_list_to_sents_list([], []).
convert_alterf_patterns_list_to_sents_list([F | R], [F1 | R1]) :-
	convert_alterf_pattern_to_sent_item(F, F1),
	convert_alterf_patterns_list_to_sents_list(R, R1).

convert_alterf_pattern_to_sent_item(alterf_pattern(Pattern, Atom, Sent), sent(Sent, [Pattern, Atom, true])) :-
	!.
convert_alterf_pattern_to_sent_item(( alterf_pattern(Pattern, Atom, Sent) :- Conds ), sent(Sent, [Pattern, Atom, Conds])) :-
	!.
convert_alterf_pattern_to_sent_item(F, F1) :-
	format2error('~N*** Error: bad call: ~w~n', [convert_alterf_pattern_to_sent_item(F, F1)]),
	fail.	

%---------------------------------------------------------------

reconstruct_alterf_pattern_decl(Pattern, Atom, Conds, WordList, PatternDecl) :-
	join_with_spaces(WordList, Example),
	(   Conds = true ->
	    PatternDecl = alterf_pattern(Pattern, Atom, Example) ;
	    PatternDecl = ( alterf_pattern(Pattern, Atom, Example) :- Conds )
	),
	numbervars(PatternDecl, 0, _),
	!.
reconstruct_alterf_pattern_decl(Pattern, Atom, Conds, WordList, PatternDecl) :-
	format2error('~N*** Error: bad call: ~w~n', [reconstruct_alterf_pattern_decl(Pattern, Atom, Conds, WordList, PatternDecl)]),
	fail.	

%---------------------------------------------------------------

summarise_alterf_pattern_checking(0) :-
	format('~N~nAll Alterf patterns OK~n', []),
	!.
summarise_alterf_pattern_checking(1) :-
	format('~N~n1 incorrect Alterf pattern~n', []),
	!.
summarise_alterf_pattern_checking(BadCount) :-
	format('~N~n~d incorrect Alterf patterns~n', [BadCount]).

