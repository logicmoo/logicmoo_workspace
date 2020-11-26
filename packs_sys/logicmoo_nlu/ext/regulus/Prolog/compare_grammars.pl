
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(compare_grammars,
	  [init_store_grammar_comparison/0,
	   store_grammar_comparison/2,
	   grammar_comparison_available/0,
	   corresponding_grammar_rule/2,
	   non_lexical_grammar_rules_changed/0,
	   changed_lexical_item/2,
	   replace_line_items_based_on_grammar_comparison/2]
	 ).

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

:- dynamic grammar_comparison_available/0.
:- dynamic grammar_rule_summary/3.
:- dynamic corresponding_grammar_rule/4.
:- dynamic non_lexical_grammar_rules_changed/0.
:- dynamic changed_lexical_item/2.

%---------------------------------------------------------------

store_grammar_comparison(_OldGrammar, _NewGrammar) :-
	\+ user:grammar_is_loaded,
	!,
	format('~N~n--- Grammar is not loaded. Unable to store grammar comparison.~n', []),
	fail.
store_grammar_comparison(OldGrammar, NewGrammar) :-
	format('~N~n--- Trying to produce comparison between old and new grammars~n', []),
	timed_call(store_grammar_comparison_main(OldGrammar, NewGrammar),
		   TimeTaken),
	print_grammar_comparison_summary,
	format('~N--- Finished storing grammar comparison (~1f seconds)~n', [TimeTaken]),
	!.
store_grammar_comparison(OldGrammar, NewGrammar) :-
	format2error('~N*** Error: bad call: ~w~n', [store_grammar_comparison(OldGrammar, NewGrammar)]),
	fail.

store_grammar_comparison_main(OldGrammar, NewGrammar) :-
	init_store_grammar_comparison,
	store_old_grammar(OldGrammar),
	store_new_grammar(NewGrammar),
	store_grammar_rule_summary,
	store_corresponding_grammar_rules,
	store_non_lexical_grammar_rules_changed,
	store_changed_lexical_items,
	assertz(grammar_comparison_available),
	!.

%---------------------------------------------------------------

init_store_grammar_comparison :-
	retractall(grammar_comparison_available),
	retractall(grammar_rule_summary(_, _, _)),
	retractall(corresponding_grammar_rule(_, _, _, _)),
	retractall(non_lexical_grammar_rules_changed),
	retractall(changed_lexical_item(_, _)).

%---------------------------------------------------------------

print_grammar_comparison_summary :-
	findall(x, grammar_rule_summary(old_grammar, _, _), OldGrammarItems),
	length(OldGrammarItems, NOldGrammarItems),
	format('~N -- ~d old grammar items~n', [NOldGrammarItems]),

	findall(x, grammar_rule_summary(new_grammar, _, _), NewGrammarItems),
	length(NewGrammarItems, NNewGrammarItems),
	format('~N -- ~d new grammar items~n', [NNewGrammarItems]),

	findall(x, corresponding_grammar_rule(_, _, _, for_old), RuleOldCorrespondences),
	length(RuleOldCorrespondences, NOldRuleCorrespondences),
	format('~N -- ~d new items corresponding to old ones~n', [NOldRuleCorrespondences]),

	findall(x, corresponding_grammar_rule(_, _, _, for_new), RuleNewCorrespondences),
	length(RuleNewCorrespondences, NNewRuleCorrespondences),
	format('~N -- ~d old items corresponding to new ones~n', [NNewRuleCorrespondences]),

	(   non_lexical_grammar_rules_changed ->
	    format('~N -- Non-lexical grammar rules have changed~n', [])
	;
	    format('~N -- No non-lexical grammar rules have changed~n', [])
	),

	findall(x, changed_lexical_item(_, _), ChangedLexicalItems),
	length(ChangedLexicalItems, NChangedLexicalItems),
	format('~N -- ~d changed lexical items~n', [NChangedLexicalItems]),
	!.
print_grammar_comparison_summary :-
	format2error('~N*** Error: bad call: ~w~n', [print_grammar_comparison_summary]),
	fail.

%---------------------------------------------------------------	

store_old_grammar(File) :-
	safe_compile_with_redefine_warnings_off(old_grammar, File).

store_new_grammar(_File).
	% Don't need to do anything, assume new grammar is loaded in usual place.
	%safe_compile_with_redefine_warnings_off(new_grammar, File).

%---------------------------------------------------------------

store_grammar_rule_summary :-
	get_dcg_clause(old_grammar, LineInfo, _GeneralHead, Body),
	dcg_clause_body_lexical_value(Body, Lexical),
	store_grammar_rule_summary(old_grammar, LineInfo, Lexical),
	fail.
store_grammar_rule_summary :-
	get_dcg_clause(new_grammar, LineInfo, _GeneralHead, Body),
	dcg_clause_body_lexical_value(Body, Lexical),
	store_grammar_rule_summary(new_grammar, LineInfo, Lexical),
	fail.
store_grammar_rule_summary.

%---------------------------------------------------------------

store_corresponding_grammar_rules :-
	store_corresponding_new_grammar_rules_for_old_grammar,
	store_corresponding_old_grammar_rules_for_new_grammar.

store_corresponding_new_grammar_rules_for_old_grammar :-
	get_dcg_clause(old_grammar, OldLineInfo, GeneralHead, Body),
	store_corresponding_new_grammar_rule_for_old_grammar_rule(OldLineInfo, GeneralHead, Body),
	fail.
store_corresponding_new_grammar_rules_for_old_grammar.

store_corresponding_old_grammar_rules_for_new_grammar :-
	get_dcg_clause(new_grammar, NewLineInfo, GeneralHead, Body),
	store_corresponding_old_grammar_rule_for_new_grammar_rule(NewLineInfo, GeneralHead, Body),
	fail.
store_corresponding_old_grammar_rules_for_new_grammar.

store_corresponding_new_grammar_rule_for_old_grammar_rule(OldLineInfo, GeneralHead, Body) :-
	get_dcg_clause(new_grammar, NewLineInfo, GeneralHead, Body),
	dcg_clause_body_lexical_value(Body, Lexical),
	store_corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, for_old),
	!.	

store_corresponding_old_grammar_rule_for_new_grammar_rule(NewLineInfo, GeneralHead, Body) :-
	get_dcg_clause(old_grammar, OldLineInfo, GeneralHead, Body),
	dcg_clause_body_lexical_value(Body, Lexical),
	store_corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, for_new),
	!.

/*
dcg_clause(adj(phrase(adj,
		      line_info(16,
				68-68,
				'c:/cygwin/home/speech/regulus/examples/toy1specialised/regulus/toy1_lex.regulus'),
		      (lex(switched),lex(off))),
	       [adjpos=bv(0,0,0,1),adjtype=bv(0,1,1,1,1,1,1,1,1,1,1),can_be_nbar=_,pp_sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),subj_sem_n_type=bv(0,0,0,0,A,A,A,A,A,1,1,1,1),takes_about_pp=bv(0,0,1),takes_attrib_pp=bv(0,0,1),takes_cost_pp=bv(0,0,1),takes_date_pp=bv(0,0,1),takes_duration_pp=bv(0,0,1),takes_frequency_pp=bv(0,0,1),takes_from_pp=bv(0,0,1),takes_loc_pp=bv(0,0,1),takes_passive_by_pp=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),takes_side_pp=bv(0,0,1),takes_time_pp=bv(0,0,1),takes_to_pp=bv(0,0,1),takes_with_pp=bv(0,0,1)],
	       [[adj,off]],
	       _,
	       B,
	       C),
	   (B=[switched|D],D=[off|C])).
*/

get_dcg_clause(Version, LineInfo, GeneralHead, Body) :-
	module_for_grammar_version(Version, Module),
	Module:dcg_clause(Head, Body),
	compound(Head),
	functor(Head, F, 6),
	Head =.. [F, Tree, Feats, Sem1, Sem2, In, Out],
	Tree = phrase(_Cat, LineInfo, Rest),
	GeneralTree = phrase(_Any, Rest),
	GeneralHead =.. [F, GeneralTree, Feats, Sem1, Sem2, In, Out],
	make_ground(GeneralHead),
	make_ground(Body).

module_for_grammar_version(old_grammar, old_grammar) :-
	!.
module_for_grammar_version(new_grammar, user) :-
	!.
module_for_grammar_version(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [module_for_grammar_version(X, Y)]),
	fail,
	!.

%---------------------------------------------------------------

dcg_clause_body_lexical_value(Body0, Lexical) :-
	unground(Body0, Body),
	dcg_clause_body_lexical_value1(Body, Lex, []),
	Lex \== [],
	Lexical = lexical(Lex),
	!.
dcg_clause_body_lexical_value(_Body, non_lexical) :-
	!.

dcg_clause_body_lexical_value1(In=Out0, In, Out) :-
	var(Out0),
	In = Out0,
	Out0 = Out,
	!.
dcg_clause_body_lexical_value1(In=[Word | Out0], In, Out) :-
	atomic(Word),
	var(Out0),
	In = [Word | Out],
	Out0 = Out,
	!.
dcg_clause_body_lexical_value1((P, Q), In, Out) :-
	dcg_clause_body_lexical_value1(P, In, Next),
	dcg_clause_body_lexical_value1(Q, Next, Out),
	!.

%---------------------------------------------------------------

replace_line_items_based_on_grammar_comparison(V, V) :-
	var(V),
	!.
replace_line_items_based_on_grammar_comparison(Atom, Atom) :-
	atomic(Atom),
	!.
replace_line_items_based_on_grammar_comparison(line_info(OldN, OldLines, OldFile),
                                               line_info(NewN, NewLines, NewFile)) :-
	corresponding_grammar_rule(line_info(OldN, OldLines, OldFile),
				   line_info(NewN, NewLines, NewFile),
				   _Lexical,
				   for_old),
	!.
replace_line_items_based_on_grammar_comparison(line_info(OldN, OldLines, OldFile), _) :-
	format2error('~N*** Error: unable to find rule matching: ~w~n',
		     [line_info(OldN, OldLines, OldFile)]),
	fail.

replace_line_items_based_on_grammar_comparison(T, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	!,
	replace_line_items_based_on_grammar_comparison_args(N, T, T1).

replace_line_items_based_on_grammar_comparison_args(I, _T, _T1) :-
	I =< 0,
	!.
replace_line_items_based_on_grammar_comparison_args(I, T, T1) :-
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	replace_line_items_based_on_grammar_comparison(Arg, Arg1),
	I1 is I - 1,
	!,
	replace_line_items_based_on_grammar_comparison_args(I1, T, T1).
	
%---------------------------------------------------------------	

store_non_lexical_grammar_rules_changed :-
	grammar_rule_summary(old_grammar, LineInfo, non_lexical),
	\+ corresponding_grammar_rule(LineInfo, _NewLineInfo, non_lexical, for_old),
	assertz(non_lexical_grammar_rules_changed),
	!.
store_non_lexical_grammar_rules_changed :-
	grammar_rule_summary(new_grammar, LineInfo, non_lexical),
	\+ corresponding_grammar_rule(_OldLineInfo, LineInfo, non_lexical, for_new),
	assertz(non_lexical_grammar_rules_changed),
	!.
store_non_lexical_grammar_rules_changed.
	
%---------------------------------------------------------------

store_changed_lexical_items :-
	grammar_rule_summary(old_grammar, LineInfo, lexical(Lex)),
	\+ corresponding_grammar_rule(LineInfo, _NewLineInfo, lexical(Lex), for_old),
	store_changed_lexical_item(Lex),
	fail.
store_changed_lexical_items :-
	grammar_rule_summary(new_grammar, LineInfo, lexical(Lex)),
	\+ corresponding_grammar_rule(_OldLineInfo, LineInfo, lexical(Lex), for_new),
	store_changed_lexical_item(Lex),
	fail.
store_changed_lexical_items.

%---------------------------------------------------------------	

store_grammar_rule_summary(Version, LineInfo, Lexical) :-
	grammar_rule_summary(Version, LineInfo, Lexical),
	!.
store_grammar_rule_summary(Version, LineInfo, Lexical) :-
	assertz(grammar_rule_summary(Version, LineInfo, Lexical)),
	!.

store_corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, ForOldOrNew) :-
	corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, ForOldOrNew),
	!.
store_corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, ForOldOrNew) :-
	assertz(corresponding_grammar_rule(OldLineInfo, NewLineInfo, Lexical, ForOldOrNew)),
	!.

store_changed_lexical_item(Lex) :-
	changed_lexical_item(_Lex1, Lex),
	!.
store_changed_lexical_item(Lex) :-
	append(Lex, _UninstantiatedTail, Lex1),
	assertz(changed_lexical_item(Lex1, Lex)),
	!.
