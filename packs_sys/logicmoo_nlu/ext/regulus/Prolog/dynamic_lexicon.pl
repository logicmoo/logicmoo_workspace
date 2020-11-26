:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(dynamic_lexicon,
	  [expand_dynamic_lex_entry/2,
	   process_dynamic_lex_entries_in_rules/2,
	   load_intermediate_dynamic_lex_associations_file_if_defined/1,
	   load_dynamic_lex_associations_file_if_defined/1,
	   current_dynamic_lexicon_entries/1,
	   init_dynamic_lex_associations_table/1,
	   zero_cached_dynamic_lex_entries/0,
	   zero_intermediate_dynamic_lex_associations/0,
	   current_grammar_is_dynamic/0,
	   is_dynamic_nuance_rule/1,
	   write_dynamic_rule_group_list_item_and_store_associations/4,
	   write_dynamic_lex_associations_table/0
	  ]).

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).

%---------------------------------------------------------------

% This predicate must be non-deterministic, hence no cut at end.
expand_dynamic_lex_entry(dynamic_lexicon(LexIn), dynamic_lexicon(LexIn, LexOut)) :-
	regulus_read:expand_macros_in_term(LexIn, LexExpanded),
	flatten_and_check_expanded_dynamic_lex_entry(LexExpanded, LexOut).

flatten_and_check_expanded_dynamic_lex_entry(LexExpanded, LexOut) :-
	nonvar(LexExpanded),
	LexExpanded = ( LHS --> RHS ),
	regulus_read:flatten_featval_lists_in_rule_body(LHS, LHS1),
	regulus_read:is_macro_expanded_regulus_rule_head(LHS1),
	var(RHS),
	LexOut = ( LHS1 --> RHS ).
	 
%---------------------------------------------------------------

load_intermediate_dynamic_lex_associations_file_if_defined(defined) :-
	get_intermediate_dynamic_lex_associations_file(AbsFile),
	safe_file_exists(AbsFile),
	safe_compile_with_redefine_warnings_off(user, AbsFile),
	format('~N--- Loaded intermediate dynamic lex associations from ~w~n', [AbsFile]),
	!.
load_intermediate_dynamic_lex_associations_file_if_defined(undefined).

%---------------------------------------------------------------

load_dynamic_lex_associations_file_if_defined(defined) :-
	get_dynamic_lex_associations_file(AbsFile),
	safe_file_exists(AbsFile),
	safe_compile_with_redefine_warnings_off(user, AbsFile),
	format('~N--- Loaded dynamic lex associations from ~w~n', [AbsFile]),
	!.
load_dynamic_lex_associations_file_if_defined(undefined).

%---------------------------------------------------------------

safe_get_intermediate_dynamic_lex_association(Macro, LexConstants, SemConstants) :-
	current_predicate(user:intermediate_dynamic_lexicon/3),
	user:intermediate_dynamic_lexicon(Macro, LexConstants, SemConstants).

%---------------------------------------------------------------

process_dynamic_lex_entries_in_rules(RulesIn, RulesIn) :-
	\+ term_contains_functor(RulesIn, dynamic_lexicon/2),
	write_out_dynamic_lexicon_entries([]),
	!.
process_dynamic_lex_entries_in_rules(RulesIn, RulesOut) :-
	process_dynamic_lex_entries_in_rules1(RulesIn, RulesNext, DynamicLexEntries-[], DynamicPairs-[]),
	get_intermediate_dynamic_lex_associations(DynamicPairs, Associations),
	reformat_lex_and_sem_consts(RulesNext, RulesOut),
	write_out_dynamic_lexicon_entries(DynamicLexEntries),
	write_out_intermediate_dynamic_lex_associations(Associations),
	!.
process_dynamic_lex_entries_in_rules(_RulesIn, _RulesOut) :-
	regulus_error('~NInternal error: bad call to process_dynamic_lex_entries_in_rules/2~n', []).

%---------------------------------------------------------------

write_out_dynamic_lexicon_entries(DynamicLexEntries0) :-
	is_list(DynamicLexEntries0),
	sort(DynamicLexEntries0, DynamicLexEntries),
	length(DynamicLexEntries, N),
	(   N = 0 ->
	    true
	;
	    otherwise ->
	    get_cached_dynamic_lexicon_entries_file(File),
	    list_to_prolog_file(DynamicLexEntries, File),
	    format('~N--- Written plain dynamic lex entries (~d items) to ~w~n', [N, File])
	),
	!.
write_out_dynamic_lexicon_entries(_DynamicPairs) :-
	regulus_error('~NInternal error: bad call to write_out_dynamic_lexicon_entries/1~n', []).

%---------------------------------------------------------------

write_out_intermediate_dynamic_lex_associations(Associations) :-
	is_list(Associations),
	length(Associations, N),
	(   N = 0 ->
	    true
	;
	    otherwise ->
	    get_intermediate_dynamic_lex_associations_file(File),
	    list_to_prolog_file(Associations, File),
	    format('~N--- Written dynamic lex associations (~d items) to ~w~n', [N, File])
	),
	!.
write_out_intermediate_dynamic_lex_associations(_Associations) :-
	regulus_error('~NInternal error: bad call to write_out_intermediate_dynamic_lex_associations/1~n', []).

%---------------------------------------------------------------

process_dynamic_lex_entries_in_rules1([], [], Lex-Lex, Pairs-Pairs).
process_dynamic_lex_entries_in_rules1([F | R], [F1 | R1], LexIn-LexOut, PairsIn-PairsOut) :-
	process_dynamic_lex_entry(F, F1, LexIn-LexNext, PairsIn-PairsNext),
	!,
	process_dynamic_lex_entries_in_rules1(R, R1, LexNext-LexOut, PairsNext-PairsOut).

process_dynamic_lex_entry(rule(Label, dynamic_lexicon(LexMacro, LexEntry), LineInfo),
			  rule(Label, LexEntry, LineInfo),
			  [MacroEntry, DynamicLexEntry | RLex]-RLex,
			  [dynamic_lexicon(LineInfo, LexMacro, LexEntry) | RPairs]-RPairs) :-
	!,
	dynamic_lexicon_record_to_macro_and_dynamic_lex_entry(dynamic_lexicon(LexMacro, LexEntry),
							      MacroEntry, DynamicLexEntry), 
	process_dynamic_lex_entry_nontrivial(LexEntry, LexMacro, LineInfo).
process_dynamic_lex_entry(Other, Other, Lex-Lex, Pairs-Pairs).

dynamic_lexicon_record_to_macro_and_dynamic_lex_entry(DynamicLexRecord0, MacroEntry, DynamicLexEntry) :-
	copy_term(DynamicLexRecord0, DynamicLexRecord),
	numbervars(DynamicLexRecord, 0, _),
	DynamicLexRecord = dynamic_lexicon(LexMacro, LexEntry),
	LexMacro = @MacroHead,
	MacroEntry = macro(MacroHead, LexEntry),
	DynamicLexEntry = dynamic_lexicon( @MacroHead ),
	!.
dynamic_lexicon_record_to_macro_and_dynamic_lex_entry(X, Y, Z) :-
	regulus_error('~NInternal error: bad call ~w~n',
		      [dynamic_lexicon_record_to_macro_and_dynamic_lex_entry(X, Y, Z)]).
	
process_dynamic_lex_entry_nontrivial(LexEntry, LexMacro, LineInfo) :-
	(   safe_instantiate_vars_in_dynamic_lex_entry(LexEntry, LexMacro, LineInfo) ->
	    true
	;
	    otherwise ->
	    inform_about_regulus_exception(failed, LineInfo)
	).	

safe_instantiate_vars_in_dynamic_lex_entry(LexEntry, LexMacro, LineInfo) :-
	on_exception(Exception,
		     instantiate_vars_in_dynamic_lex_entry(LexEntry, LexMacro),
		     inform_about_regulus_exception(Exception, LineInfo)
		    ).

instantiate_vars_in_dynamic_lex_entry(LexEntry, LexMacro) :-
	LexEntry = (LHS --> RHS),
	instantiate_surface_words_var(RHS),
	instantiate_sem_vars_in_lhs(LHS, LexMacro),
	!.
	
instantiate_sem_vars_in_lhs(LHS, LexMacro) :-
	LHS = _Cat:Feats,
	nonvar(Feats),
	member(sem=Sem, Feats),
	term_variables_bag(LexMacro, VarsInMacro),
	term_variables_bag(Sem, VarsInSem),
	instantiate_sem_vars_in_lhs1(VarsInSem, VarsInMacro),
	!.
instantiate_sem_vars_in_lhs(_LHS, _LexMacro).

instantiate_sem_vars_in_lhs1([], _VarsInMacro).
instantiate_sem_vars_in_lhs1([F | R], VarsInMacro) :-
	instantiate_sem_var(F, VarsInMacro),
	!,
	instantiate_sem_vars_in_lhs1(R, VarsInMacro).

instantiate_surface_words_var(RHS) :-
	var(RHS),
	RHS = lex(_),
	!.
instantiate_surface_words_var(_RHS).

instantiate_sem_var(Var, VarsInMacro) :-
	var(Var),
	id_member(Var, VarsInMacro),
	Var = sem(_),
	!.
instantiate_sem_var(_Var, _VarsInMacro).

%---------------------------------------------------------------

get_intermediate_dynamic_lex_associations(DynamicPairs, Associations) :-
	merge_dynamic_pairs_by_line_info(DynamicPairs, MergedDynamicPairs),
	get_intermediate_dynamic_lex_associations1(MergedDynamicPairs, Associations, 1-_, 1-_).

merge_dynamic_pairs_by_line_info(DynamicPairs, MergedDynamicPairs) :-
	empty_assoc_generic(AssocIn),
	merge_dynamic_pairs_by_line_info1(DynamicPairs, AssocIn-AssocOut),
	assoc_generic_to_list(AssocOut, MergedDynamicPairs).

merge_dynamic_pairs_by_line_info1([], Assoc-Assoc).
merge_dynamic_pairs_by_line_info1([F | R], AssocIn-AssocOut) :-
	merge_dynamic_pair_by_line_info(F, AssocIn-AssocNext),
	!,
	merge_dynamic_pairs_by_line_info1(R, AssocNext-AssocOut).

merge_dynamic_pair_by_line_info(dynamic_lexicon(LineInfo, LexMacro, _LexEntry), AssocIn-AssocOut) :-
	(   get_assoc_generic(LineInfo, AssocIn, MergedLexMacros) ->
	    MergedLexMacros = LexMacro
	;
	    true
	),
	put_assoc_generic(LineInfo, AssocIn, LexMacro, AssocOut),
	!.
merge_dynamic_pair_by_line_info(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [merge_dynamic_pair_by_line_info(X, Y)]),
	fail.

get_intermediate_dynamic_lex_associations1([], [], Lex-Lex, Sem-Sem).
get_intermediate_dynamic_lex_associations1([F | R], [F1 | R1], LexIn-LexOut, SemIn-SemOut) :-
	get_intermediate_dynamic_lex_association(F, F1, LexIn-LexNext, SemIn-SemNext),
	!,
	get_intermediate_dynamic_lex_associations1(R, R1, LexNext-LexOut, SemNext-SemOut).

get_intermediate_dynamic_lex_association(Input, Output, LexIn-LexOut, SemIn-SemOut) :-
	Input = LineInfo-LexMacro,
	reformat_lex_and_sem_consts(LexMacro, LexMacro1, LexIn-LexOut, SemIn-SemOut, LexConstIn-[], SemConstIn-[]),
	Output = intermediate_dynamic_lexicon(LexMacro1, LexConstIn, SemConstIn),
	(   ground(LexMacro1) ->
	    true
	;
	    otherwise ->
	    numbervars(LexMacro, 0, _),
	    Exception = regulus_exception('~NDynamic lexicon macro vars can only refer to surface strings or semantic constants:~n~w~n',
					  [LexMacro]),
	    inform_about_regulus_exception(Exception, LineInfo),
	    fail
	),
	!.
get_intermediate_dynamic_lex_association(LineInfo, DynamicPairs, Output) :-
	format('~N*** Error: bad call: ~w~n',
	       [get_intermediate_dynamic_lex_association(LineInfo, DynamicPairs, Output)]),
	fail.

%---------------------------------------------------------------

reformat_lex_and_sem_consts(T, T1) :-
	reformat_lex_and_sem_consts(T, T1, 1-_, 1-_, _-[], _-[]).

reformat_lex_and_sem_consts(Var, Var, Lex-Lex, Sem-Sem, LexC-LexC, SemC-SemC) :-
	var(Var),
	!.
reformat_lex_and_sem_consts(Atom, Atom, Lex-Lex, Sem-Sem, LexC-LexC, SemC-SemC) :-
	atomic(Atom),
	!.
reformat_lex_and_sem_consts(lex(Var), Var, LexIn-LexOut, Sem-Sem, LexCIn-LexCOut, SemC-SemC) :-
	(   var(Var) ->
	    new_lex_const(Var, LexIn-LexOut),
	    LexCIn = [Var | LexCOut]
	;
	    LexIn = LexOut,
	    LexCIn = LexCOut
	),
	!.
reformat_lex_and_sem_consts(sem(Var), Var, Lex-Lex, SemIn-SemOut, LexC-LexC, SemCIn-SemCOut) :-
	(   var(Var) ->
	    new_sem_const(Var, SemIn-SemOut),
	    SemCIn = [Var | SemCOut]
	;
	    SemIn = SemOut,
	    SemCIn = SemCOut
	),
	!.
reformat_lex_and_sem_consts(T, T1, LexIn-LexOut, SemIn-SemOut, LexCIn-LexCOut, SemCIn-SemCOut) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	reformat_lex_and_sem_consts_args(N, T, T1, LexIn-LexOut, SemIn-SemOut, LexCIn-LexCOut, SemCIn-SemCOut).

reformat_lex_and_sem_consts_args(I, _T, _T1, Lex-Lex, Sem-Sem, LexC-LexC, SemC-SemC) :-
	I < 1,
	!.
reformat_lex_and_sem_consts_args(I, T, T1, LexIn-LexOut, SemIn-SemOut, LexCIn-LexCOut, SemCIn-SemCOut) :-
	I >= 1,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	reformat_lex_and_sem_consts(Arg, Arg1, LexIn-LexNext, SemIn-SemNext, LexCIn-LexCNext, SemCIn-SemCNext),
	I1 is I - 1,
	!,
	reformat_lex_and_sem_consts_args(I1, T, T1, LexNext-LexOut, SemNext-SemOut,
					 LexCNext-LexCOut, SemCNext-SemCOut).

new_lex_const(Var, LexIn-LexOut) :-
	format_to_atom('LEX_~d', [LexIn], Var),
	LexOut is LexIn + 1,
	!.

new_sem_const(Var, SemIn-SemOut) :-
	format_to_atom('SEM_~d', [SemIn], Var),
	SemOut is SemIn + 1,
	!.

%---------------------------------------------------------------

:- dynamic dynamic_lex_association/8.

init_dynamic_lex_associations_table(AbsFile) :-
	retractall(dynamic_lex_association(_, _, _, _, _, _, _, _)),
	store_dynamic_file_dir(AbsFile),
	zero_dynamic_grammar_name_counter.

write_dynamic_lex_associations_table :-
	get_dynamic_lex_associations_file(File),
	findall(Record,
		dynamic_lex_association_record(Record),
		Records0),
	sort(Records0, Records),
	length(Records, N),
	N > 0,
	list_to_prolog_file(Records, File),
	format('~N--- Written dynamic associations file (~d records) ~w~n', [N, File]),
	!.
write_dynamic_lex_associations_table.

dynamic_lex_association_record(Record) :-
	dynamic_lex_association(_MacroName, GrammarTag, _Lex, _ReturnValue,
				AbstractMacro, AbstractLex, AbstractReturnValue, DynamicGrammarName),
	get_dynamic_file_dir(Dir),
	format_to_atom('~w/DynamicNuance/~w.gsl', [Dir, DynamicGrammarName], File),
	Record0 = dynamic_lex_association(AbstractMacro, GrammarTag,
					  DynamicGrammarName, AbstractLex, AbstractReturnValue, File),
	copy_term(Record0, Record),
	numbervars(Record, 0, _).

%---------------------------------------------------------------

% intermediate_dynamic_lexicon(@switchable_dev_noun(['LEX_2','LEX_1'],'SEM_1'),['LEX_1','LEX_2'],['SEM_1']).

is_dynamic_nuance_rule(Rule) :-
	Rule = ( _LHS --> RHS ),
	is_dynamic_nuance_rule_rhs(RHS).

is_dynamic_nuance_rule_rhs(Lex) :-
	atom(Lex),
	get_intermediate_dynamic_lexicon_entry(_Macro, LexConstants, _SemConstants),
	member(Lex, LexConstants).
is_dynamic_nuance_rule_rhs((P, Q)) :-
	(   is_dynamic_nuance_rule_rhs(P)
	;
	    is_dynamic_nuance_rule_rhs(Q)
	),
	!.
 
write_dynamic_rule_group_list_item_and_store_associations(S, HeadCat, GrammarTag, Rule) :-
	Rule = ( cat(_CatName, _SynFeatsWithVals, ReturnValue) --> Lex ),
	get_intermediate_dynamic_lexicon_entry(Macro, LexConstants, SemConstants),
	member(Lex, LexConstants),
	get_or_store_dynamic_lex_association(HeadCat, GrammarTag, Lex, ReturnValue, Macro,
					     LexConstants, SemConstants, DynamicGrammarName),
	write_dynamic_rule_group_list_item(S, DynamicGrammarName),
	!.
write_dynamic_rule_group_list_item_and_store_associations(S, HeadCat, GrammarTag, Rule) :-
	regulus_error('~NInternal error: bad call ~w~n',
		      [write_dynamic_rule_group_list_item_and_store_associations(S, HeadCat, GrammarTag, Rule)]).

write_dynamic_rule_group_list_item(S, DynamicGrammarName) :-
	NuanceReturnVar = 'v_0',
	format(S, '~N<file:../DynamicNuance/~w.gsl>:~w', [DynamicGrammarName, NuanceReturnVar]),
	format(S, '~n     {return( ', []),
	format(S, '$~w', [NuanceReturnVar]),
	format(S, ' )}~n', []),
	!.

%---------------------------------------------------------------

get_or_store_dynamic_lex_association(_HeadCat, GrammarTag, Lex, ReturnValue,
				     Macro, _LexConstants, _SemConstants, DynamicGrammarName) :-
	macro_name_from_macro_call(Macro, MacroName),
	dynamic_lex_association(MacroName, GrammarTag, Lex, ReturnValue,
				_AbstractMacro, _AbstractLex, _AbstractReturnValue, DynamicGrammarName),
	!.
get_or_store_dynamic_lex_association(_HeadCat, GrammarTag, Lex, ReturnValue,
				     Macro, LexConstants, SemConstants, DynamicGrammarName) :-
	macro_name_from_macro_call(Macro, MacroName),
	new_dynamic_grammar_name(MacroName, Lex, GrammarTag, DynamicGrammarName),
	append(LexConstants, SemConstants, AllConstants),
	replace_constants_by_variables(AllConstants,
				       [Macro, Lex, ReturnValue],
				       [AbstractMacro, AbstractLex, AbstractReturnValue]),
	assertz(dynamic_lex_association(MacroName, GrammarTag, Lex, ReturnValue,
					AbstractMacro, AbstractLex, AbstractReturnValue, DynamicGrammarName)),
	!.
get_or_store_dynamic_lex_association(HeadCat, GrammarTag, Lex, ReturnValue,
				     Macro, LexConstants, SemConstants, DynamicGrammarName) :-
	regulus_error('~NInternal error: bad call: ~w~n',
		      [get_or_store_dynamic_lex_association(HeadCat, GrammarTag, Lex, ReturnValue,
							    Macro, LexConstants, SemConstants,
							    DynamicGrammarName)]).

%---------------------------------------------------------------

replace_constants_by_variables([], T, T).
replace_constants_by_variables([F | R], TIn, TOut) :-
	replace_constant_by_variable(F, TIn, TNext),
	!,
	replace_constants_by_variables(R, TNext, TOut).

replace_constant_by_variable(Const, TIn, TOut) :-
	substitute_in_term(TIn, Const, _NewVar, TOut).

%---------------------------------------------------------------

:- dynamic dynamic_grammar_name_counter/2.

zero_dynamic_grammar_name_counter :-
	retractall(dynamic_grammar_name_counter(_, _)).

new_dynamic_grammar_name(MacroName, Lex, GrammarTag, DynamicGrammarName) :-
	new_dynamic_grammar_name_counter(MacroName, Lex, GrammarTag, I),
	(   I = 1 ->
	    format_to_atom('dynamic_~w~w_~w', [MacroName, GrammarTag, Lex], DynamicGrammarName)
	;
	    otherwise ->
	    format_to_atom('dynamic_~w~w_~w_~d', [MacroName, GrammarTag, Lex, I], DynamicGrammarName)
	),
	!.

new_dynamic_grammar_name_counter(MacroName, Lex, GrammarTag, I1) :-
	dynamic_grammar_name_counter([MacroName, Lex, GrammarTag], I),
	I1 is I + 1,
	retract(dynamic_grammar_name_counter([HeadCat, GrammarTag], I)),
	assertz(dynamic_grammar_name_counter([HeadCat, GrammarTag], I1)),
	!.
new_dynamic_grammar_name_counter(MacroName, Lex, GrammarTag, 1) :-
	assertz(dynamic_grammar_name_counter([MacroName, Lex, GrammarTag], 1)),
	!.

%---------------------------------------------------------------

current_grammar_is_dynamic :-
	current_predicate(user:intermediate_dynamic_lexicon/3),
	user:intermediate_dynamic_lexicon(_Macro, _LexConstants, _SemConstants).

%---------------------------------------------------------------

get_intermediate_dynamic_lexicon_entry(Macro, LexConstants, SemConstants) :-
	current_predicate(user:intermediate_dynamic_lexicon/3),
	user:intermediate_dynamic_lexicon(Macro, LexConstants, SemConstants).

%---------------------------------------------------------------

current_dynamic_lexicon_entries(Entries) :-
	get_cached_dynamic_lexicon_entries_file(AbsFile),
	safe_file_exists(AbsFile),
	prolog_file_to_list(AbsFile, Entries),
	!.
current_dynamic_lexicon_entries([]).

%---------------------------------------------------------------

zero_cached_dynamic_lex_entries :-
	get_cached_dynamic_lexicon_entries_file(File),
	zero_file(File),
	!.

zero_intermediate_dynamic_lex_associations :-
	get_intermediate_dynamic_lex_associations_file(File),
	zero_file(File),
	abolish_if_defined(user:intermediate_dynamic_lexicon/3),
	!.

%---------------------------------------------------------------

get_cached_dynamic_lexicon_entries_file(AbsFile) :-
	check_config_file_is_loaded,
	user:get_regulus_config_item(cached_dynamic_lex_entries, File),
	safe_absolute_file_name(File, AbsFile).

get_intermediate_dynamic_lex_associations_file(AbsFile) :-
	check_config_file_is_loaded,
	user:get_regulus_config_item(intermediate_dynamic_lex_associations, File),
	safe_absolute_file_name(File, AbsFile).

get_dynamic_lex_associations_file(AbsFile) :-
	check_config_file_is_loaded,
	user:get_regulus_config_item(dynamic_lex_associations, File),
	safe_absolute_file_name(File, AbsFile).

%---------------------------------------------------------------

:- dynamic dynamic_file_dir/1.

store_dynamic_file_dir(GrammarFile) :-
	safe_absolute_file_name(GrammarFile, AbsGrammarFile),
	directory_and_file_for_pathname(AbsGrammarFile, GrammarDir, _BaseFile),
	parent_directory(GrammarDir, ParentDir),
	retractall(dynamic_file_dir(_)),
	assertz(dynamic_file_dir(ParentDir)),
	!.
 
get_dynamic_file_dir(Dir) :-
	dynamic_file_dir(Dir),
	!.

%---------------------------------------------------------------

zero_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	safe_file_exists(AbsFile),
	open(File, write, S),
	close(S),
	format('~N--- Zeroed file ~w~n', [AbsFile]).
zero_file(_File).

%---------------------------------------------------------------

macro_name_from_macro_call(Macro, MacroName) :-
	compound(Macro),
	Macro = @Body,
	compound(Body),
	functor(Body, MacroName, _N).
