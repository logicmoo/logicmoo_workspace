% ebl_train.pl
 
%---------------------------------------------------------------

:- module(ebl_train,
	  [ebl_train/4,
	   ebl_train/5,
	   ebl_train_single_example/6,
	   write_stored_ebl_created_sent_data_to_file/1]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/ebl_generalise').
:- use_module('$REGULUS/Prolog/ebl_operational').
:- use_module('$REGULUS/Prolog/ebl_include_lex').

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).

%---------------------------------------------------------------

:- dynamic stored_lex_data/3.
:- dynamic stored_sent_data/2.

%---------------------------------------------------------------

ebl_train(InFile, Operationality, EBLIncludeLexFiles, OutFile) :-
	ebl_train(InFile, Operationality, EBLIncludeLexFiles, [], OutFile).
	
ebl_train(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile) :-
	timed_call(ebl_train_main(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile),
		   TimeTaken),
	format('~N~nEBL training complete, ~2f secs~n', [TimeTaken]).

ebl_train_main(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile) :-
	check_that_grammar_is_loaded,
	check_or_load_operationality(Operationality, Operationality1),
	init_stored_lex_data,
	init_stored_sent_data,
	ebl_train_main1(InFile, Operationality1, EBLIncludeLexFiles, IgnoredSubdomains, OutFile),
	!.
ebl_train_main(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [ebl_train_main(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile)]),
	fail.

%---------------------------------------------------------------

ebl_train_single_example(GrammarAtom, Operationality, Tree0, WordList0, LF, RegulusRules) :-
	check_that_grammar_is_loaded,
	check_or_load_operationality(Operationality, Operationality1),

	remove_lexical_information_from_tree_and_word_list_if_necessary(Tree0, WordList0, Tree, WordList),
	Goal =.. [GrammarAtom, Tree, _SynFeats, _Local, Global, WordList, []],
	
	%generalise(Goal, Operationality1, PrologRules),
	%real_lf_out_of_global(Global, LF1),
	%LF = LF1,
	best_generalise(Goal, WordList, Global, LF, WordList0, Operationality1, PrologRules),
	Annotations = [],
	learned_ebl_rules_to_regulus(PrologRules, WordList, Annotations, RegulusRules).

%---------------------------------------------------------------


check_that_grammar_is_loaded :-
	current_predicate(user:dcg_clause/2),
	true,
	!.
check_that_grammar_is_loaded :-
	format2error('~N*** Error: no grammar appears to be loaded, unable to do EBL training~n', []),
	fail.

%---------------------------------------------------------------

check_or_load_operationality(file(File), Operationality) :-
	load_operationality_file(File),
	Operationality = from_file,
	!.
check_or_load_operationality(file(File), _Operationality) :-
	!,
	format2error('~N*** Error: unable to load operationality defs from file: ~w~n', [File]),
	fail.
check_or_load_operationality(Operationality, Operationality1) :-
	defined_operationality(Operationality),
	Operationality1 = Operationality,
	!.
check_or_load_operationality(Operationality, _Operationality1) :-
	\+ defined_operationality(Operationality),
	format2error('~N~nError: operationality ~w not defined~n', [Operationality]),
	fail.
check_or_load_operationality(Operationality, Operationality1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [check_or_load_operationality(Operationality, Operationality1)]),
	fail.

%---------------------------------------------------------------

load_operationality_file(File) :-
	abolish_if_defined(tmp_ebl_operational:operational_goal/3),
	abolish_if_defined(tmp_ebl_operational:change_context_goal/3),
	%safe_compile(user, File),
	safe_compile_with_redefine_and_single_var_warnings_off(user, File),
	(   current_predicate(tmp_ebl_operational:operational_goal/3) ->
	    true
	;
	    format2error('~N*** Error: operational_goal/3 not defined in operationality file', []),
	    fail
	).

%---------------------------------------------------------------

init_stored_lex_data :-
	retractall(stored_lex_data(_, _, _)).

init_stored_sent_data :-
	retractall(stored_sent_data(_, _)).

%---------------------------------------------------------------

ebl_train_main1(InFile, Operationality, EBLIncludeLexFiles, IgnoredSubdomains, OutFile) :-
	absolute_file_name(InFile, AbsoluteInFile),
	absolute_file_name(OutFile, AbsoluteOutFile),
 
	(   \+ file_exists(AbsoluteInFile, read) ->
	    format2error('~N~nError: could not find file ~w:~n', [AbsoluteInFile]),
	    fail
	;
	    IgnoredSubdomains = [] ->
	    format('~N~nDoing EBL training on parsed data from ~w, using all subdomains:~n', [AbsoluteInFile])
	;
	    otherwise ->
	    format('~N~nDoing EBL training on parsed data from ~w, ignoring subdomains ~w:~n',
		   [AbsoluteInFile, IgnoredSubdomains])
	),

	open(InFile, read, SIn),
	open_regulus_file(OutFile, write, SOut),
	%trace,

	(   ebl_train1(SIn, Operationality, SOut, 0-N, 0-NBad) ->
	    format('~N~nOutput of EBL learning phase (~d records, ~d bad) written to ~w.~n',
		   [N, NBad, AbsoluteOutFile]),
	    maybe_warn_about_treebank_problems(NBad, N)
	;
	    format2error('~N~nError: could not create file ~w.~n', [AbsoluteOutFile]),
	    fail
	),

	close(SIn),

	collect_stored_lex_data(StoredLexData),
	call_ebl_include_lex(EBLIncludeLexFiles, StoredLexData, IgnoredSubdomains, SOut),
	
	close(SOut).
  
%---------------------------------------------------------------

maybe_warn_about_treebank_problems(NBad, N) :-
	NBad > 3,
	N > 0,
	PC is ( NBad * 100.0 ) / N,
	(   PC > 20 ->
	    format('~N*** Warning: ~1f% of treebank items failed EBL training. Your treebank may be corrupted.~n', [PC]),
	    format('~N             Things to try:', []),
	    format('~N             ', []),
	    format('~N             1. Do INCREMENTAL_TREEBANKING_OFF and then redo EBL_TREEBANK and EBL_TRAIN', []),
	    format('~N             2. Remove files of form *trees.pl from your Generated directory and redo training', []),
	    format('~N             3. If you\'re in SICStus, exit, come back and redo training.~n', [])
	;
	    PC > 0 ->
	    format('~N*** Warning: ~1f% of treebank items failed EBL training.~n', [PC])
	;
	    otherwise ->
	    true
	).
maybe_warn_about_treebank_problems(_NBad, _N).
	
%---------------------------------------------------------------

call_ebl_include_lex(EBLIncludeLexFiles, _StoredLexData, _IgnoredSubdomains, _SOut) :-
	EBLIncludeLexFiles = [],
	!.
call_ebl_include_lex(EBLIncludeLexFiles, StoredLexData, IgnoredSubdomains, SOut) :-
	create_ebl_included_lex_entries(EBLIncludeLexFiles, StoredLexData, IgnoredSubdomains, LexEntries),
	length(LexEntries, NLexEntries),
	write_regulus_rules_to_stream(LexEntries, SOut),
	format('~N~nIncluded ~d lexical entries directly from general grammar (some of these will probably already have been acquired)~n', [NLexEntries]),
	!.
call_ebl_include_lex(EBLIncludeLexFiles, _StoredLexData, _IgnoredSubdomains, _SOut) :-
	format2error('~N*** Error: could not create included lex entries from ~w~n', [EBLIncludeLexFiles]),
	fail.

%---------------------------------------------------------------

ebl_train1(SIn, Operationality, SOut, NIn-NOut, NBadIn-NBadOut) :-
	safe_read(SIn, T),
	ebl_train2(T, SIn, Operationality, SOut, NIn-NOut, NBadIn-NBadOut).

ebl_train2(end_of_file, _SIn, _Operationality, _SOut, NIn-NIn, NBadIn-NBadIn).
ebl_train2(T, SIn, Operationality, SOut, NIn-NOut, NBadIn-NBadOut) :-
	ebl_train_from_example(T, Operationality, SOut, NBadIn-NBadNext),
	NNext is NIn + 1,
	(   0 is NNext mod 100 ->
	    
	    format(' (~d) ~n', [NNext]),
	    flush_output(user) ;
	    
	    true
	),
	!,
	ebl_train1(SIn, Operationality, SOut, NNext-NOut, NBadNext-NBadOut).

%---------------------------------------------------------------

ebl_train_from_example(TIn, _Operationality, _SOut, NBadIn-NBadIn) :-
	TIn = example(_GrammarAtom, 'NO_PARSE', _LF, _WordList, _Annotations),
	format('-', []),
	flush_output(user),
	!.
/*
ebl_train_from_example(TIn, _Operationality, _SOut) :-
	TIn = example(_GrammarAtom, Tree, _LF, WordList, _Annotations),
	\+ ground(Tree),
	format2error('~N*** Error: tree not ground: skipping example: ~w ~w~n', [WordList, Tree]),
	!.
*/
ebl_train_from_example(TIn, _Operationality, _SOut, NBadIn-NBadOut) :-
	TIn = example(_GrammarAtom, Tree, _LF, WordList, _Annotations),
	term_contains_functor(Tree, no_tree/0),
	format2error('~N*** Error: parse tree contained a variable node (probably an internal parser error). Skipping example: ~w~n', [WordList]),
	NBadOut is NBadIn + 1,
	!.
ebl_train_from_example(TIn, Operationality, SOut, NBadIn-NBadIn) :-
	TIn = example(GrammarAtom, Tree0, LF, WordList0, Annotations),
	remove_lexical_information_from_tree_and_word_list_if_necessary(Tree0, WordList0, Tree, WordList),
	Goal =.. [GrammarAtom, Tree, _SynFeats, _Local, Global, WordList, []],
	%generalise(Goal, Operationality, PrologRules),
	%real_lf_out_of_global(Global, LF1),
	%LF = LF1,
	best_generalise(Goal, WordList, Global, LF, WordList0, Operationality, PrologRules),
	learned_ebl_rules_to_regulus(PrologRules, WordList, Annotations, RegulusRules),
	write_regulus_rules_to_stream(RegulusRules, SOut),
	store_lex_data_for_learned_ebl_rules(RegulusRules),
	store_sent_data_after_ebl_training(WordList, Annotations),
	format('.', []),
	flush_output(user),
	!.
ebl_train_from_example(TIn, _Operationality, _SOut, NBadIn-NBadOut) :-
	TIn = example(_GrammarAtom, _Tree, _LF, WordList, _Annotations),
	NBadOut is NBadIn + 1,
	format2error('~N*** Error: unable to create training data for: ~w~n', [WordList]).
ebl_train_from_example(TIn, _Operationality, _SOut, NBadIn-NBadOut) :-
	NBadOut is NBadIn + 1,
	format2error('~N*** Error: unable to create training data for: ~w~n', [TIn]).

%---------------------------------------------------------------

% If WordList is fully instantiated, just pick the first answer returned by generalise/3 which gives the right LF.
best_generalise(Goal, WordList, Global, LF, _WordList0, Operationality, PrologRules) :-
	ground(WordList),
	generalise(Goal, Operationality, PrologRules),
	real_lf_out_of_global(Global, LF1),
	LF = LF1,
	!.
% If WordList is not fully instantiated, pick the answer most like the original WordList0
best_generalise(Goal, WordList, Global, LF, WordList0, Operationality, PrologRules) :-
	copy_term([Goal, WordList, Global], [GenGoal, GenWordList, GenGlobal]),
	findall(Score-[GenGoal, GenWordList, GenGlobal, PrologRules],
		(   generalise(GenGoal, Operationality, PrologRules),
		    word_similarity_score(GenWordList, WordList0, Score),
		    real_lf_out_of_global(GenGlobal, LF1),
		    LF = LF1
		),
		Pairs),
	pick_top_from_best_generalise(Pairs, BestGoal, BestWordList, BestGlobal, BestPrologRules),
	[Goal, WordList, GenGlobal, PrologRules] =
	[BestGoal, BestWordList, BestGlobal, BestPrologRules],
	!.
best_generalise(Goal, WordList, Global, LF, WordList0, Operationality, PrologRules) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [best_generalise(Goal, WordList, Global, LF, WordList0, Operationality, PrologRules)]).

pick_top_from_best_generalise(Pairs, _Goal, _WordList, _Global, _PrologRules) :-
	Pairs = [],
	!,
	fail.
pick_top_from_best_generalise(Pairs, Goal, WordList, Global, PrologRules) :-
	keysort(Pairs, SortedPairs),
	SortedPairs = [_BestScore-[Goal, WordList, Global, PrologRules] | _],
	!.
pick_top_from_best_generalise(Pairs, Goal, WordList, Global, PrologRules) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [pick_top_from_best_generalise(Pairs, Goal, WordList, Global, PrologRules)]).

word_similarity_score(WordList, WordList1, Score) :-
	insertions_deletions_substitutions(WordList, WordList1, Total, _I, _D, _S),
	Score = Total,
	!.
word_similarity_score(_WordList, _WordList1, Score) :-
	Score = 0.

%---------------------------------------------------------------

remove_lexical_information_from_tree_and_word_list_if_necessary(Tree0, _WordList0, Tree, _WordList) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(discard_lexical_info_in_ebl_training, yes),
	remove_lexical_info_in_parse_tree(Tree0, Tree),
	%length(WordList0, N),
	%length(WordList, N),
	!.
remove_lexical_information_from_tree_and_word_list_if_necessary(Tree0, WordList0, Tree, WordList) :-
	Tree0 = Tree,
	WordList0 = WordList.

%---------------------------------------------------------------

real_lf_out_of_global(Global, LF) :-
	nonvar(Global),
	Global=[_Feat=UnevaluatedLF | _],
	user:regulus_eval_text(UnevaluatedLF, LF),
	!.

%---------------------------------------------------------------
 
learned_ebl_rules_to_regulus([], _WordList, _Annotations, []).
learned_ebl_rules_to_regulus([F | R], WordList, Annotations, [F1 | R1]) :-
	learned_ebl_rule_to_regulus(F, WordList, Annotations, F1),
	learned_ebl_rules_to_regulus(R, WordList, Annotations, R1).

learned_ebl_rule_to_regulus(AnnotatedPrologRule, WordList, Annotations, AnnotatedRegulusRule) :-
	AnnotatedPrologRule = rule(PrologRule, PrologRuleContext, Fringe),
	remove_non_syntactic_information_from_prolog_rule(PrologRuleContext, PrologRuleContext1),
	AnnotatedRegulusRule = rule(PrologRule, PrologRuleContext1, WordList, Fringe, Annotations),
	!.
learned_ebl_rule_to_regulus(AnnotatedPrologRule, WordList, Annotations, RegulusRule) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [learned_ebl_rule_to_regulus(AnnotatedPrologRule, WordList, Annotations, RegulusRule)]),
	fail.

%---------------------------------------------------------------

remove_non_syntactic_information_from_prolog_rule((H :- B), (H1 :- B1)) :-
	remove_non_syntactic_information_from_prolog_rule_goal(H, H1),
	remove_non_syntactic_information_from_prolog_rule_body(B, B1),
	!.
remove_non_syntactic_information_from_prolog_rule(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [remove_non_syntactic_information_from_prolog_rule(X, Y)]),
	fail.

remove_non_syntactic_information_from_prolog_rule_body((F, R), (F1, R1)) :-
	!,
	remove_non_syntactic_information_from_prolog_rule_body(F, F1),
	remove_non_syntactic_information_from_prolog_rule_body(R, R1).
remove_non_syntactic_information_from_prolog_rule_body(Other, Other1) :-
	remove_non_syntactic_information_from_prolog_rule_goal(Other, Other1).

remove_non_syntactic_information_from_prolog_rule_goal(true, true) :-
	!.
remove_non_syntactic_information_from_prolog_rule_goal(Goal, Goal1) :-
	functor(Goal, 'C', 3),
	functor(Goal1, 'C', 3),
	!.
remove_non_syntactic_information_from_prolog_rule_goal(Goal, Goal1) :-
	functor(Goal, merge_globals, 2),
	functor(Goal1, merge_globals, 2),
	!.
remove_non_syntactic_information_from_prolog_rule_goal(Goal, Goal1) :-
	functor(Goal, F, 6),
	functor(Goal1, F, 6),
	arg(2, Goal, SynFeats),
	arg(2, Goal1, SynFeats),
	!.
remove_non_syntactic_information_from_prolog_rule_goal(Goal, Goal1) :-
	format2error('~N*** Error: bad call: ~w~n', [remove_non_syntactic_information_from_prolog_rule_goal(Goal, Goal1)]),
	fail.

%---------------------------------------------------------------

write_regulus_rules_to_stream([], _SOut).
write_regulus_rules_to_stream([F | R], SOut) :-
	write_regulus_rule_to_stream(F, SOut),
	write_regulus_rules_to_stream(R, SOut).

write_regulus_rule_to_stream(Rule, SOut) :-
	format(SOut, '~n', []),
	portray_clause(SOut, Rule).

%---------------------------------------------------------------

store_lex_data_for_learned_ebl_rules([]).
store_lex_data_for_learned_ebl_rules([F | R]) :-
	store_lex_data_for_learned_ebl_rule(F),
	!,
	store_lex_data_for_learned_ebl_rules(R).

/*
Raw EBL training result:

rule((p(phrase(p,lex(on)),[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],[[prep,on]],_,A,B):-'C'(A,on,B)), (p(_,[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],_,_,_,_):-'C'(_,_,_)), [switch,on,the,light], [on], [default]).

*/

store_lex_data_for_learned_ebl_rule(Record) :-
	Record = rule((Goal :- Body), _RuleContext, _WholeExample, _Words, Tags),
	functor(Goal, Cat, _),
	arg(3, Goal, Sem),
	lex_body(Body),
	!,
	store_lex_data_for_learned_ebl_rule1(Cat, Sem, Tags).
store_lex_data_for_learned_ebl_rule(_Record).

% This clause for Sicstus 4 version of DCG
lex_body(true).
lex_body('C'(_,_,_)).
lex_body((P, Q)) :-
	lex_body(P),
	lex_body(Q).

store_lex_data_for_learned_ebl_rule1(Cat, Sem, Tags) :-
	stored_lex_data(Cat, Sem, Tags),
	!.
store_lex_data_for_learned_ebl_rule1(Cat, Sem, Tags) :-
	assertz(stored_lex_data(Cat, Sem, Tags)).

collect_stored_lex_data(StoredLexData) :-
	findall(stored_lex_data(Cat, Sem, Tags),
		stored_lex_data(Cat, Sem, Tags),
		StoredLexData0),
	sort(StoredLexData0, StoredLexData).

%---------------------------------------------------------------

store_sent_data_after_ebl_training(WordList, Annotations) :-
	join_with_spaces(WordList, Sent),
	assertz(stored_sent_data(Sent, Annotations)).

%---------------------------------------------------------------

write_stored_ebl_created_sent_data_to_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	findall(sent(Sent, Annotations),
		stored_sent_data(Sent, Annotations),
		List),
	length(List, N),
	list_to_prolog_file(List, AbsFile),
	format('~N--- Written corpus file produced by EBL training (~d records) ~w~n',
	       [N, AbsFile]),
	!.
write_stored_ebl_created_sent_data_to_file(File) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [write_stored_ebl_created_sent_data_to_file(File)]),
	fail.
