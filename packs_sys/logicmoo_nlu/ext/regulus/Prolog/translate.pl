% translate.pl
 
% Top-level for translation

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(translate,
	[translation_rules_are_loaded/0,
	 ellipsis_rules_are_loaded/0,
	 
	 transfer_resources_defined_for_module/1,
	 zero_transfer_resources_defined_for_module/0,
	 set_transfer_resources_defined_for_module/1,

	 compile_transfer_file_or_files/5,
	 compile_ellipsis_class_file/5,

	 allow_undefined_interlingua_elements/0,
	 dont_allow_undefined_interlingua_elements/0,

	 zero_interlingua_constant_used_table/0,
	 create_filtered_interlingua_declarations_file/2,

	 transfer_representation/2,
	 transfer_representation/3,
	 
	 transfer_representation_to_source_discourse/2,
	 transfer_representation_to_source_discourse/3,
	 transfer_representation_to_source_discourse/4,

	 transfer_representation_to_interlingua/2,
	 transfer_representation_to_interlingua/3,
	 transfer_representation_to_interlingua/4,
	 
	 transfer_representation_from_interlingua/2,
	 transfer_representation_from_interlingua/3,
	 transfer_representation_from_interlingua/4,
	 
	 transfer_representation_through_interlingua/4,
	 transfer_representation_through_interlingua/5,

	 transfer_representation_to_surface_interlingua_using_context/4,
	 transfer_representation_to_surface_interlingua_using_context/5,
	 transfer_representation_to_interlingua_and_surface_interlingua_using_context/6,
	 transfer_representation_to_interlingua_and_surface_interlingua_using_context/7,

	 check_interlingua_structure/2,
	 check_alternate_interlingua_structure/3,
	 get_module_for_alternate_interlingua_structure/2,

	 switch_on_transfer_tracing/0,
	 switch_off_transfer_tracing/0,

	 switch_on_interlingua_tracing/0,
	 switch_off_interlingua_tracing/0,

	 switch_on_interlingua_structure_debugging/0,
	 switch_off_interlingua_structure_debugging/0,

	 zero_stored_transfer_trace/0,
	 collect_stored_transfer_trace_info_into_alist/1,

	 safe_parse_transfer_and_generate/5,
	 safe_parse_transfer_and_generate/6,
	 safe_transfer_and_generate_lf_to_interlingua/4,
	 
	 safe_parse_transfer_generate_and_backtranslate/6,
	 safe_transfer_generate_and_backtranslate/7,
	 safe_transfer_generate_and_backtranslate/6,
	 safe_transfer_generate_and_backtranslate_delayed/6,
	 perform_delayed_transfer_and_generation/2,
  
	 parse_transfer_and_generate/4,
	 parse_transfer_and_generate/5,
	 parse_transfer_and_generate_file/3,
	 parse_transfer_and_generate_file/4,
	 recognise_parse_transfer_and_generate_file/11,

	 make_translation_file_from_csv_version/1,

	 zero_stored_translate_corpus_summary/0,
	 set_stored_translate_corpus_summary_id/1,
	 get_stored_translate_corpus_summary/2,

	 error_result/1,
	 bad_interlingua_surface/1,

	 package_translation_results_for_java_gui/4,
	 package_translation_corpus_results_file_for_java_gui/2,
	 unpack_and_store_judged_translation_output_file_from_string/2,

	 check_backtranslation_on_corpus_output/1,
	 store_target_vocabulary_for_corpus_output/1,

	 convert_interlingua/2,
	 convert_interlingua_in_file/3,

	 qa_file_to_q_file/2,
	 qa_file_and_translation_output_to_a_file/3,

	 print_parse_time_report_for_translation_result_file/1,
	 print_timing_summary_for_translation_result_file/1,
 
	 update_translation_judgements/4,
	 update_recognition_judgements/2,
	 make_tmp_recognition_judgements_file/2]
    ).
  
%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/generate').
:- use_module('$REGULUS/Prolog/resolve').
:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/nbest').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/analysis_constraints').
:- use_module('$REGULUS/Prolog/progress').
:- use_module('$REGULUS/Prolog/recognition').
:- use_module('$REGULUS/Prolog/java_gui_utils').
:- use_module('$REGULUS/Prolog/regulus_declarations').

:- use_module('$REGULUS/PrologLib/batchrec_tools').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(terms)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
:- use_module(library(timeout)).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

%----------------------------------------------------------------------

% Maximum time allowed for analysis, in seconds

analysis_time_limit(TimeLimit) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(analysis_time_limit, TimeLimit0),
	(   number(TimeLimit0) ->
	    TimeLimit = TimeLimit0
	;
	    format2error('~N*** Error: generation time limit = ~w, should be a number. Using default value.~n', [TimeLimit0]),
	    fail
	),
	!.
analysis_time_limit(5).

%----------------------------------------------------------------------

translation_rules_are_loaded :-
	(   current_predicate(user:transfer_lexicon/4)
	;
	    current_predicate(user:to_interlingua_lexicon/4)
	;
	    current_predicate(user:from_interlingua_lexicon/4)
	;
	    current_predicate(user:to_interlingua_rule/6)
	),
	!.

ellipsis_rules_are_loaded :-
	current_predicate(user:ellipsis_class_example/2),
	!.

:- dynamic transfer_resources_defined_for_module/1.

zero_transfer_resources_defined_for_module :-
	retractall(zero_transfer_resources_defined_for_module(_)).

set_transfer_resources_defined_for_module(Module) :-
	transfer_resources_defined_for_module(Module),
	!.
set_transfer_resources_defined_for_module(Module) :-
	assertz(transfer_resources_defined_for_module(Module)).

%----------------------------------------------------------------------

compile_transfer_file_or_files(InFileOrFiles, OutFile, RuleType, Direction, Representation) :-
	check_interlingua_declarations_are_loaded_if_rule_type_involves_interlingua(RuleType),
	
	absolute_file_name(OutFile, AbsOutFile),
	
	read_transfer_file_or_files(InFileOrFiles, RawTransferRules, _TransferDecls),

	compile_transfer_rules(RawTransferRules, CompiledTransferRules, RuleType, Direction, Representation),
	length(CompiledTransferRules, NRules),
	count_compiled_transfer_rule_types(CompiledTransferRules, TypesAssoc),

	list_to_prolog_file(CompiledTransferRules, AbsOutFile),
	format('~N -- Compiled transfer rule file (~d items) written to: ~w~n', [NRules, AbsOutFile]),
	(   NRules > 0 ->
	    print_compiled_transfer_rule_types(TypesAssoc)
	;
	    true
	),
	!.
compile_transfer_file_or_files(InFile, _OutFile, _RuleType, _Direction) :-
	format2error('~N~n*** Error: unable to compile transfer rule file or files ~w~n', [InFile]),
	fail.

%----------------------------------------------------------------------

count_compiled_transfer_rule_types(List, TypesAssoc) :-
	empty_assoc_generic(InitAssoc),
	count_compiled_transfer_rule_types1(List, InitAssoc-TypesAssoc),
	!.
count_compiled_transfer_rule_types(_List, _TypesAssoc) :-
	format2error('~N~n*** Error: bad call to translate:count_compiled_transfer_rule_types/2~n', []),
	fail.

count_compiled_transfer_rule_types1([], Assoc-Assoc).
count_compiled_transfer_rule_types1([F | R], AssocIn-AssocOut) :-
	classify_compiled_transfer_rule(F, Type),
	inc_assoc_generic(AssocIn, Type, AssocNext),
	!,
	count_compiled_transfer_rule_types1(R, AssocNext-AssocOut).

classify_compiled_transfer_rule(Rule, Type) :-
	compound(Rule),
	functor(Rule, Pred, _N),
	classify_compiled_transfer_rule_pred(Pred, MainType),
	classify_compiled_transfer_rule1(MainType, Rule, Type),
	!.
classify_compiled_transfer_rule(_Rule, Type) :-
	Type = 'Other',
	!.
classify_compiled_transfer_rule(Rule, Type) :-
	format2error('~N~n*** Error: bad call: ~w~n', [classify_compiled_transfer_rule(Rule, Type)]),
	fail.

classify_compiled_transfer_rule1('Complex', Rule, Type) :-
	compound(Rule),
	functor(Rule, _F, 6),
	arg(5, Rule, ContextConditions),
	(   ContextConditions = true ->
	    Type = 'Complex, no conditions'
	;
	    Type = 'Complex, conditions'
	),
	!.
classify_compiled_transfer_rule1('Role', Rule, Type) :-
	compound(Rule),
	functor(Rule, _F, 4),
	arg(3, Rule, ContextConditions),
	(   ContextConditions = true ->
	    Type = 'Role, no conditions'
	;
	    Type = 'Role, conditions'
	),
	!.
classify_compiled_transfer_rule1(Type, _Rule, Type) :-
	!.

classify_compiled_transfer_rule_pred(transfer_rule, 'Complex') :-
	!.
classify_compiled_transfer_rule_pred(to_source_discourse_rule, 'Complex') :-
	!.
classify_compiled_transfer_rule_pred(to_interlingua_rule, 'Complex') :-
	!.
classify_compiled_transfer_rule_pred(from_interlingua_rule, 'Complex') :-
	!.
classify_compiled_transfer_rule_pred(role_transfer_rule, 'Role') :-
	!.
classify_compiled_transfer_rule_pred(to_source_discourse_role_rule, 'Role') :-
	!.
classify_compiled_transfer_rule_pred(to_interlingua_role_rule, 'Role') :-
	!.
classify_compiled_transfer_rule_pred(from_interlingua_role_rule, 'Role') :-
	!.
classify_compiled_transfer_rule_pred(transfer_lexicon, 'Simple') :-
	!.
classify_compiled_transfer_rule_pred(to_source_discourse_lexicon, 'Simple') :-
	!.
classify_compiled_transfer_rule_pred(to_interlingua_lexicon, 'Simple') :-
	!.
classify_compiled_transfer_rule_pred(from_interlingua_lexicon, 'Simple') :-
	!.
classify_compiled_transfer_rule_pred(_Other, 'Other') :-
	!.

print_compiled_transfer_rule_types(TypesAssoc) :-
	assoc_generic_to_list(TypesAssoc, Alist),
	format('~N -- Breakdown of translation rules by type:~n', []),
	print_compiled_transfer_rule_types1(Alist),
	format('~N~n', []).
print_compiled_transfer_rule_types(_TypesAssoc) :-
	format2error('~N~n*** Error: bad call to transfer:print_compiled_transfer_rule_types/1~n',
		     []),
	fail.

print_compiled_transfer_rule_types1([]).
print_compiled_transfer_rule_types1([F | R]) :-
	print_compiled_transfer_rule_types_line(F),
	!,
	print_compiled_transfer_rule_types1(R).

print_compiled_transfer_rule_types_line(Type-N) :-
	format('~N~4|~w~30|~d~n', [Type, N]),
	!.
print_compiled_transfer_rule_types_line(Other) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_compiled_transfer_rule_types_line(Other)]),
	fail.

%----------------------------------------------------------------------

check_interlingua_declarations_are_loaded_if_rule_type_involves_interlingua(RuleType) :-
	(   RuleType = direct ;
	    RuleType = to_source_discourse
	),
	!.
check_interlingua_declarations_are_loaded_if_rule_type_involves_interlingua(InterlinguaRuleType) :-
	(   current_predicate(user:interlingua_constant/1) ->
	    true ;
	    
	    format2error('~N~n*** Error: attempt to compile ~w file, but no interlingua declarations are loaded.~n', [InterlinguaRuleType]),
	    fail
	).

%----------------------------------------------------------------------

create_filtered_interlingua_declarations_file(CurrentFile, FilteredFile) :-
	safe_absolute_file_name(CurrentFile, AbsCurrentFile),
	safe_absolute_file_name(FilteredFile, AbsFilteredFile),
	
	prolog_file_to_list(AbsCurrentFile, CurrentList),
	sort(CurrentList, SortedCurrentList),
	open_regulus_file(AbsFilteredFile, write, S),

	filter_interlingua_declarations_list(SortedCurrentList, S, 0-NUsed, 0-NNotUsed),

	close(S),
	format('~N~n -- Filtered interlingua declarations file (~d entries kept, ~d commented out) written to: ~w~n',
	       [NUsed, NNotUsed, AbsFilteredFile]),
	!.
create_filtered_interlingua_declarations_file(_CurrentFile, _FilteredFile) :-
	format2error('~N~n*** Error: unable to create filtered interlingua file ~w~n', []),
	fail.

filter_interlingua_declarations_list([], _S, NUsed-NUsed, NNotUsed-NNotUsed) :-
	!.
filter_interlingua_declarations_list([F | R], S, NUsedIn-NUsedOut, NNotUsedIn-NNotUsedOut) :-
	interlingua_declaration_is_used(F),
	make_ground(F),
	format(S, '~N~q.~n', [F]),
	NUsedNext is NUsedIn + 1,
	NNotUsedNext is NNotUsedIn,
	!,
	filter_interlingua_declarations_list(R, S, NUsedNext-NUsedOut, NNotUsedNext-NNotUsedOut).
filter_interlingua_declarations_list([F | R], S, NUsedIn-NUsedOut, NNotUsedIn-NNotUsedOut) :-
	make_ground(F),
	format(S, '~N%~q.~n', [F]),
	NUsedNext is NUsedIn,
	NNotUsedNext is NNotUsedIn + 1,
	!,
	filter_interlingua_declarations_list(R, S, NUsedNext-NUsedOut, NNotUsedNext-NNotUsedOut).
	      
interlingua_declaration_is_used(interlingua_constant(Const)) :-
	interlingua_constant_has_been_used(Const, to_interlingua),
	!.
interlingua_declaration_is_used(( interlingua_constant(Const) :- Body )) :-
	copy_term( ( interlingua_constant(Const) :- Body ), ( interlingua_constant(Const1) :- Body1 ) ),
	interlingua_constant_has_been_used(Const1, to_interlingua),
	call(Body1),
	!.

%----------------------------------------------------------------------

compile_ellipsis_class_file(InFile, TopLevelCat, SentsFile, TreebankFile, OutFile) :-
	absolute_file_name(InFile, AbsInFile),
	absolute_file_name(OutFile, AbsOutFile),
	
	format('~N~n--- Compiling ellipsis class file: ~w~n', [AbsInFile]),
	prolog_file_to_list(AbsInFile, RawEllipsisClasses),

	compile_ellipsis_classes(RawEllipsisClasses, TopLevelCat, SentsFile, TreebankFile, CompiledEllipsisClasses),
	length(CompiledEllipsisClasses, NEntries),

	list_to_prolog_file(CompiledEllipsisClasses, AbsOutFile),
	format('~N~n--- Compiled ellipsis class file (~d items) written to: ~w~n~n', [NEntries, AbsOutFile]),
	!.
compile_ellipsis_class_file(InFile, _TopLevelCat, _SentsFile, _TreebankFile, _OutFile) :-
	format2error('~N~n*** Error: unable to compile ellipsis class file ~w~n', [InFile]),
	fail.
	
%----------------------------------------------------------------------

compile_transfer_rules(RawRules, AllCompiledRules, RuleType, Direction, Representation) :-
	init_compile_transfer_rules,
	compile_transfer_rules1(RawRules, TransferRules, TransferLexicon, RuleType, Direction, Representation),
	add_transfer_variable_variants(TransferRules, TransferRules1),
	append(TransferRules1, TransferLexicon, AllCompiledRules),
	!.

compile_transfer_rules1([], [], [], _RuleType, _Direction, _Representation).
compile_transfer_rules1([F | R], RulesIn, LexiconIn, RuleType, Direction, Representation) :-
	compile_transfer_item(F, RulesIn-RulesNext, LexiconIn-LexiconNext, RuleType, Direction, Representation),
	!,
	compile_transfer_rules1(R, RulesNext, LexiconNext, RuleType, Direction, Representation).

compile_transfer_item(Item, RulesIn-RulesOut, LexiconIn-LexiconOut, RuleType, Direction, Representation) :-
	Item = rule(Rule, LineInfo),
	compile_transfer_item1(Rule, RulesIn-RulesOut, LexiconIn-LexiconOut, LineInfo, RuleType, Direction, Representation),
	!.
compile_transfer_item(Item, RulesIn-RulesIn, LexiconIn-LexiconIn, _RuleType, _Direction, _Representation) :-
	Item = rule(Rule, LineInfo),
	format2error('~N*** Error: unable to compile item in transfer rule file: ~w ', [Rule]),
	inform_about_line_info(LineInfo),
	format2error('~N~n', []),
	!.

compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	(   Rule = transfer_rule(_L, _R) ;
	    Rule = ( transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = forward,
	compile_transfer_rule(Rule, LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, RulesIn-RulesIn, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	(   Rule = transfer_rule(_L, _R) ;
	    Rule = ( transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = backward,
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	(   (   Rule = reverse_transfer_rule(L, R),
		Rule1 = transfer_rule(R, L)
	    ) ;
	    (   Rule = ( reverse_transfer_rule(L, R) :- ContextConditions ),
		Rule1 = ( transfer_rule(R, L) :- ContextConditions )
	    )
	),
	Direction = backward,
	compile_transfer_rule(Rule1, LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, RulesIn-RulesIn, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	(   Rule = reverse_transfer_rule(_L, _R) ;
	    Rule = ( reverse_transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = forward,
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = bidirectional_transfer_rule(L, R),
	Direction = forward,
	compile_transfer_rule(transfer_rule(L, R), LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = bidirectional_transfer_rule(L, R),
	Direction = backward,
	compile_transfer_rule(transfer_rule(R, L), LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	(   Rule = role_transfer_rule(_L, _R) ;
	    Rule = ( role_transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = forward,
	compile_role_transfer_rule(Rule, LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, RulesIn-RulesIn, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	(   Rule = role_transfer_rule(_L, _R) ;
	    Rule = ( role_transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = backward,
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	(   (   Rule = reverse_role_transfer_rule(L, R),
		Rule1 = role_transfer_rule(R, L)
	    ) ;
	    (   Rule = ( reverse_role_transfer_rule(L, R) :- ContextConditions ),
		Rule1 = ( role_transfer_rule(R, L) :- ContextConditions )
	    )
	),
	Direction = backward,
	compile_role_transfer_rule(Rule1, LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, RulesIn-RulesIn, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	(   Rule = reverse_role_transfer_rule(_L, _R) ;
	    Rule = ( reverse_role_transfer_rule(_L, _R) :- _ContextConditions )
	),
	Direction = forward,
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = bidirectional_role_transfer_rule(L, R),
	Direction = forward,
	compile_role_transfer_rule(role_transfer_rule(L, R), LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = bidirectional_role_transfer_rule(L, R),
	Direction = backward,
	compile_role_transfer_rule(role_transfer_rule(R, L), LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesOut]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = ( role_list_transfer_rule([L:R]) :- ContextConditions ),
	Direction = forward,
	Rule1 = ( role_transfer_rule(L, R) :- ContextConditions ),
	compile_role_transfer_rule(Rule1, LineInfo, CompiledRule, RuleType, Representation),
	!.
compile_transfer_item1(Rule, [CompiledRule | RulesNext]-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation) :-
	Rule = ( role_list_transfer_rule([L:R | Rest]) :- ContextConditions ),
	Direction = forward,
	Rule1 = ( role_transfer_rule(L, R) :- ContextConditions ),
	Rule2 = ( role_list_transfer_rule(Rest) :- ContextConditions ),
	compile_role_transfer_rule(Rule1, LineInfo, CompiledRule, RuleType, Representation),
	compile_transfer_item1(Rule2, RulesNext-RulesOut, LexiconIn-LexiconIn, LineInfo, RuleType, Direction, Representation),
	!.
compile_transfer_item1(Lex, RulesOut-RulesOut, [CompiledLex | LexiconOut]-LexiconOut, LineInfo, RuleType, Direction, Representation) :-
	Lex = transfer_lexicon(_L, _R),
	Direction = forward,
	compile_transfer_lexicon_entry(Lex, LineInfo, CompiledLex, RuleType, Representation),
	!.
compile_transfer_item1(Lex, RulesOut-RulesOut, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	Lex = transfer_lexicon(_L, _R),
	Direction = backward,
	!.
compile_transfer_item1(Lex, RulesOut-RulesOut, [CompiledLex | LexiconOut]-LexiconOut, LineInfo, RuleType, Direction, Representation) :-
	Lex = reverse_transfer_lexicon(L, R),
	Direction = backward,
	Lex1 = transfer_lexicon(R, L),
	compile_transfer_lexicon_entry(Lex1, LineInfo, CompiledLex, RuleType, Representation),
	!.
compile_transfer_item1(Lex, RulesOut-RulesOut, LexiconIn-LexiconIn, _LineInfo, _RuleType, Direction, _Representation) :-
	Lex = reverse_transfer_lexicon(_L, _R),
	Direction = forward,
	!.
compile_transfer_item1(Lex, RulesOut-RulesOut, [CompiledLex | LexiconOut]-LexiconOut, LineInfo, RuleType, Direction, Representation) :-
	Lex = bidirectional_transfer_lexicon(L, R),
	Direction = forward,
	compile_transfer_lexicon_entry(transfer_lexicon(L, R), LineInfo, CompiledLex, RuleType, Representation),
	!. 
compile_transfer_item1(Lex, RulesOut-RulesOut, [CompiledLex | LexiconOut]-LexiconOut, LineInfo, RuleType, Direction, Representation) :-
	Lex = bidirectional_transfer_lexicon(L, R),
	Direction = backward,
	compile_transfer_lexicon_entry(transfer_lexicon(R, L), LineInfo, CompiledLex, RuleType, Representation),
	!.

%----------------------------------------------------------------------
 
compile_transfer_rule(Rule, LineInfo, CompiledRule, RuleType, Representation) :-
	(   Rule = ( transfer_rule(L0, R0) :- ContextConditions0 ) ->
	    make_context_conditions_canonical(ContextConditions0, Representation, ContextConditions) ;
	    Rule = transfer_rule(L0, R0) ->
	    ContextConditions = true 
	),
	is_list(L0),
	(   Representation = role_marked_linear ->
	    add_roles_in_transfer_rule_expression(L0, L, lhs),
	    add_roles_in_transfer_rule_expression(R0, R, rhs)
	;
	    Representation = nested ->
	    add_clause_markers_in_nested_representation(L0, L, top),
	    add_clause_markers_in_nested_representation(R0, R, top)
	;
	    otherwise ->
	    [L, R] = [L0, R0]
	),
	(   ( RuleType = direct ; RuleType = to_source_discourse ) ->
	    true ;

	    RuleType = to_interlingua ->
	    check_interlingua_representation(R, to_interlingua, LineInfo) ;

	    RuleType = from_interlingua ->
	    check_interlingua_representation(L, from_interlingua, LineInfo)
	),
	
	compile_transfer_variables(L, R, L1, R1),
	check_transfer_rule_for_duplicate_transfer_vars(L1, LineInfo),
	
	order_transfer_rule_lhs_and_extract_first_element(L1, L2, FirstElt),
	elements_with_uninstantianted_roles_in_list(L1, L1ForRoles),
	
	(   RuleType = direct ->
	    CompiledRule = transfer_rule(FirstElt, L2, L1ForRoles, R1, ContextConditions, LineInfo) ;

	    RuleType = to_source_discourse ->
	    CompiledRule = to_source_discourse_rule(FirstElt, L2, L1ForRoles, R1, ContextConditions, LineInfo) ;

	    RuleType = to_interlingua ->
	    CompiledRule = to_interlingua_rule(FirstElt, L2, L1ForRoles, R1, ContextConditions, LineInfo) ;

	    RuleType = from_interlingua ->
	    CompiledRule = from_interlingua_rule(FirstElt, L2, L1ForRoles, R1, ContextConditions, LineInfo)
	),

	check_transfer_rule_for_duplication(FirstElt, L2, ContextConditions, LineInfo).

compile_role_transfer_rule(Rule, LineInfo, CompiledRule, RuleType, Representation) :-
	(   Rule = ( role_transfer_rule(L, R) :- ContextConditions0 ) ->
	    make_context_conditions_canonical(ContextConditions0, Representation, ContextConditions) ;
	    Rule = role_transfer_rule(L, R) ->
	    ContextConditions = true 
	),
	atomic(L),
	
	(   RuleType = direct ->
	    CompiledRule = role_transfer_rule(L, R, ContextConditions, LineInfo) ;

	    RuleType = to_source_discourse ->
	    CompiledRule = to_source_discourse_role_rule(L, R, ContextConditions, LineInfo) ;

	    RuleType = to_interlingua ->
	    CompiledRule = to_interlingua_role_rule(L, R, ContextConditions, LineInfo) ;

	    RuleType = from_interlingua ->
	    CompiledRule = from_interlingua_role_rule(L, R, ContextConditions, LineInfo)
	).

compile_transfer_lexicon_entry(Lex, LineInfo, CompiledLex, RuleType, Representation) :-
	Lex = transfer_lexicon(L0, R0),
	(   Representation = role_marked_linear ->
	    add_roles_in_transfer_rule_item(L0, L, lhs),
	    add_roles_in_transfer_rule_item(R0, R, rhs)
	;
	    otherwise ->
	    [L, R] = [L0, R0]
	),
	elements_with_uninstantianted_roles_in_list([L], LForRoles),
	
	(   ( RuleType = direct ; RuleType = to_source_discourse ) ->
	    true ;

	    RuleType = to_interlingua ->
	    check_interlingua_representation([R], to_interlingua, LineInfo) ;

	    RuleType = from_interlingua ->
	    check_interlingua_representation([L], from_interlingua, LineInfo)
	),

	(   RuleType = direct ->
	    CompiledLex = transfer_lexicon(L, LForRoles, R, LineInfo) ;

	    RuleType = to_source_discourse ->
	    CompiledLex = to_source_discourse_lexicon(L, LForRoles, R, LineInfo) ;

	    RuleType = to_interlingua ->
	    CompiledLex = to_interlingua_lexicon(L, LForRoles, R, LineInfo) ;

	    RuleType = from_interlingua ->
	    CompiledLex = from_interlingua_lexicon(L, LForRoles, R, LineInfo)
	),

	check_transfer_lexicon_entry_for_duplication(L, LineInfo).

%----------------------------------------------------------------------

elements_with_uninstantianted_roles_in_list([], []).
elements_with_uninstantianted_roles_in_list([Role=Item | R], [Role=Item | R1]) :-
	var(Role),
	!,
	elements_with_uninstantianted_roles_in_list(R, R1).
elements_with_uninstantianted_roles_in_list([_Other | R], R1) :-
	!,
	elements_with_uninstantianted_roles_in_list(R, R1).

%----------------------------------------------------------------------

:- dynamic previous_transfer_rule_info/4.
:- dynamic previous_transfer_lex_info/2.

init_compile_transfer_rules :-
	retractall(previous_transfer_rule_info(_, _, _, _)),
	retractall(previous_transfer_lex_info(_, _)).

%----------------------------------------------------------------------

add_clause_markers_in_nested_representation([], [], _Top) :-
	!.
add_clause_markers_in_nested_representation(Var, Var, _Top) :-
	var(Var),
	!.
add_clause_markers_in_nested_representation(Pair, Pair, _Top) :-
	is_simple_pair(Pair),
	!.
add_clause_markers_in_nested_representation([clause, L], [clause, L1], _Top) :-
	is_list(L),
	!,
	add_clause_markers_in_nested_representation_list(L, L1, non_top).
add_clause_markers_in_nested_representation(L, [clause, L1], non_top) :-
	is_list_of_simple_pairs(L),
	!,
	add_clause_markers_in_nested_representation_list(L, L1).
add_clause_markers_in_nested_representation(L, L1, top) :-
	is_list(L),
	!,
	add_clause_markers_in_nested_representation_list(L, L1).
add_clause_markers_in_nested_representation(Other, Other).

add_clause_markers_in_nested_representation_list([], []).
add_clause_markers_in_nested_representation_list([F | R], [F1 | R1]) :-
	add_clause_markers_in_nested_representation(F, F1, non_top),
	!,
	add_clause_markers_in_nested_representation_list(R, R1).

%----------------------------------------------------------------------

add_roles_in_transfer_rule_expression([], [], _Side) :-
	!.
add_roles_in_transfer_rule_expression([F | R], [F1 | R1], Side) :-
	add_roles_in_transfer_rule_item(F, F1, Side),
	add_roles_in_transfer_rule_expression(R, R1, Side),
	!.
add_roles_in_transfer_rule_expression(X, Y, Side) :-
	format2error('~N*** Error: bad call: ~w~n', [add_roles_in_transfer_rule_expression(X, Y, Side)]),
	fail.

add_roles_in_transfer_rule_item(Role=[clause, Clause], Role1=[clause, Clause1], Side) :-
	(   Side = rhs, var(Role) ->
	    Role1 = '*explicitly_uninstantiated*'
	;
	    Role1 = Role
	),
	add_roles_in_transfer_rule_expression(Clause, Clause1, Side),
	!.
add_roles_in_transfer_rule_item([conj, ConjType | Conjs], [conj, ConjType | Conjs1], Side) :-
	add_roles_in_transfer_rule_expression_list(Conjs, Conjs1, Side),
	!.
add_roles_in_transfer_rule_item([clause, Clause], _Role=[clause, Clause1], Side) :-
	add_roles_in_transfer_rule_expression(Clause, Clause1, Side),
	!.
add_roles_in_transfer_rule_item(Role=Item, Role1=Item, Side) :-
	(   Side = rhs, var(Role) ->
	    Role1 = '*explicitly_uninstantiated*'
	;
	    Role1 = Role
	),
	!.
add_roles_in_transfer_rule_item(Item, _Role=Item, _Side) :-
	!.
add_roles_in_transfer_rule_item(X, Y, Side) :-
	format2error('~N*** Error: bad call: ~w~n', [add_roles_in_transfer_rule_item(X, Y, Side)]),
	fail.

add_roles_in_transfer_rule_expression_list([], [], _Side).
add_roles_in_transfer_rule_expression_list([F | R], [F1 | R1], Side) :-
	add_roles_in_transfer_rule_expression(F, F1, Side),
	!,
	add_roles_in_transfer_rule_expression_list(R, R1, Side).

%----------------------------------------------------------------------

/*
We count LHS transfer variables as duplicated if they are the same up to renaming
of variables, and occur in the same list. The most efficient way to do the checking
is first to ground each element individually.
*/

check_transfer_rule_for_duplicate_transfer_vars(LHS, LineInfo) :-
	compile_transfer_rule_lhs_has_duplicate_transfer_vars(LHS),
	inform_about_duplicate_transfer_vars(LineInfo),
	!.
check_transfer_rule_for_duplicate_transfer_vars(_LHS, _LineInfo).

compile_transfer_rule_lhs_has_duplicate_transfer_vars(LHS) :-
	copy_term(LHS, LHS1),
	ground_elements_in_transfer_rule_lhs(LHS1),
	compile_transfer_rule_lhs_has_duplicate_transfer_vars1(LHS1).

ground_elements_in_transfer_rule_lhs([]).
ground_elements_in_transfer_rule_lhs([F | R]) :-
	ground_elements_in_transfer_rule_lhs_item(F),
	!,
	ground_elements_in_transfer_rule_lhs(R).

ground_elements_in_transfer_rule_lhs_item([conj, _ConjType | Conjs]) :-
	ground_elements_in_transfer_rule_lhs_list(Conjs),
	!.
ground_elements_in_transfer_rule_lhs_item([clause, Body]) :-
	ground_elements_in_transfer_rule_lhs_item(Body),
	!.
ground_elements_in_transfer_rule_lhs_item(_Role=[clause, Body]) :-
	ground_elements_in_transfer_rule_lhs_item(Body),
	!.
ground_elements_in_transfer_rule_lhs_item(Other) :-
	make_ground(Other),
	!.

ground_elements_in_transfer_rule_lhs_list([]).
ground_elements_in_transfer_rule_lhs_list([F | R]) :-
	ground_elements_in_transfer_rule_lhs(F),
	!,
	ground_elements_in_transfer_rule_lhs_list(R).

compile_transfer_rule_lhs_has_duplicate_transfer_vars1([F | R]) :-
	compile_transfer_rule_lhs_element_has_duplicate_transfer_vars(F, R),
	!.
compile_transfer_rule_lhs_has_duplicate_transfer_vars1([_F | R]) :-
	compile_transfer_rule_lhs_has_duplicate_transfer_vars1(R),
	!.

compile_transfer_rule_lhs_element_has_duplicate_transfer_vars([clause, Body], _Context) :-
	compile_transfer_rule_lhs_has_duplicate_transfer_vars1(Body),
	!.
compile_transfer_rule_lhs_element_has_duplicate_transfer_vars(_Role=[clause, Body], _Context) :-
	compile_transfer_rule_lhs_has_duplicate_transfer_vars1(Body),
	!.
compile_transfer_rule_lhs_element_has_duplicate_transfer_vars(compiled_tr(Pattern, Var), Context) :-
	member(compiled_tr(Pattern, Var), Context),
	!.
compile_transfer_rule_lhs_element_has_duplicate_transfer_vars(Role=compiled_tr(Pattern, Var), Context) :-
	member(Role=compiled_tr(Pattern, Var), Context),
	!.

%----------------------------------------------------------------------

check_transfer_lexicon_entry_for_duplication(LHS, LineInfo) :-
	copy_term(LHS, LHS1),
	make_ground(LHS1),
	
	(   previous_transfer_lex_info(LHS1, OtherLineInfo) ->
	    
	    inform_about_previous_transfer_lexicon_entry(OtherLineInfo, LineInfo),
	    fail ;
	    
	    assertz(previous_transfer_lex_info(LHS1, LineInfo))
	).

check_transfer_rule_for_duplication(FirstElt, LHS, ContextConditions, LineInfo) :-
	copy_term([FirstElt, LHS, ContextConditions],
		  [FirstElt1, LHS1, ContextConditions1]),
	make_ground([FirstElt1, LHS1, ContextConditions1]),
		    
	(   previous_transfer_rule_info(FirstElt1, LHS1, ContextConditions1, OtherLineInfo) ->
	    
	    inform_about_previous_transfer_rule_entry(OtherLineInfo, LineInfo),
	    fail ;

	    assertz(previous_transfer_rule_info(FirstElt1, LHS1, ContextConditions1, LineInfo))
	).

inform_about_previous_transfer_lexicon_entry(OtherLineInfo, LineInfo) :-
	OtherLineInfo = LineInfo,
	format2error('~N*** Alternate macro expansion produces same entry. Probably a duplication in the macro set.~n', []),
	!.
inform_about_previous_transfer_lexicon_entry(OtherLineInfo, LineInfo) :-
	OtherLineInfo \== LineInfo,
	format2error('~N*** Earlier transfer lexicon entry with same LHS ', []),
	inform_about_line_info(OtherLineInfo),
	!.

inform_about_previous_transfer_rule_entry(OtherLineInfo, LineInfo) :-
	OtherLineInfo = LineInfo,
	format2error('~N*** Alternate macro expansion produces same entry. Probably a duplication in the macro set.~n', []),
	!.
inform_about_previous_transfer_rule_entry(OtherLineInfo, LineInfo) :-
	OtherLineInfo \== LineInfo,
	format2error('~N*** Earlier transfer rule entry with same LHS and conditions ', []),
	inform_about_line_info(OtherLineInfo),
	!.

inform_about_duplicate_transfer_vars(LineInfo) :-
	format2error('~N~n*** Warning: formally equivalent transfer variables. Rule is underconstrained ', []),
	inform_about_line_info(LineInfo),
	!.

%----------------------------------------------------------------------

/*
In LIn, transfer variables have the form tr(<Id>, <Restriction>), where
  <Id> is an atom
  <Restriction> is either a term or of the form ( <Restriction1> ; <Restriction2> )
%
In RIn, transfer variables have the form tr(<Id>)
%
Every <Id> in RIn must occur once in LIn.
Every <Id> in LIn must occur at least once in RIn.
%
%
In LOut, transfer variables have the form compiled_tr(<Restriction>, <Var>), where
  <Restriction> is as in LIn
  <Var> is a variable
%
In ROut, transfer variables have the form <Var>,
where <Var> is the variable in a left-hand-side compiled transfer variable.
*/

compile_transfer_variables(LIn, RIn, LOut, ROut) :-
	check_that_lhs_isnt_just_transfer_variables(LIn),
	replace_transfer_variables_in_lhs(LIn, LOut, LHSTransferVarPairs-[]),
	check_no_duplicates_in_lhs_transfer_var_pairs(LHSTransferVarPairs),
	check_all_transfer_vars_in_lhs_occur_in_rhs(LHSTransferVarPairs, RIn),
	replace_transfer_variables_in_rhs(RIn, LHSTransferVarPairs, ROut),
	!.
compile_transfer_variables(_LIn, _RIn, _LOut, _ROut) :-
	format2error('~N*** Error: unable to compile transfer variables~n', []),
	!,
	fail.

%----------------------------------------------------------------------

%replace_transfer_variables_in_lhs(LIn, LOut, TransferVarPairsIn-TransferVarPairsOut)

replace_transfer_variables_in_lhs([], [], TransferVarPairsIn-TransferVarPairsIn) :-
	!.
replace_transfer_variables_in_lhs([F | R], [F1 | R1], TransferVarPairsIn-TransferVarPairsOut) :-
	replace_transfer_variables_in_lhs_element(F, F1, TransferVarPairsIn-TransferVarPairsNext),
	!,
	replace_transfer_variables_in_lhs(R, R1, TransferVarPairsNext-TransferVarPairsOut).

replace_transfer_variables_in_lhs_element([conj, ConjType | ConjsIn], [conj, ConjType | ConjsOut],
					  TransferVarPairsIn-TransferVarPairsOut) :-
	replace_transfer_variables_in_lhs_list(ConjsIn, ConjsOut, TransferVarPairsIn-TransferVarPairsOut),
	!.
replace_transfer_variables_in_lhs_element([clause, ClauseIn], [clause, ClauseOut],
					  TransferVarPairsIn-TransferVarPairsOut) :-
	!,
	replace_transfer_variables_in_lhs(ClauseIn, ClauseOut, TransferVarPairsIn-TransferVarPairsOut).
replace_transfer_variables_in_lhs_element(Role=[clause, ClauseIn], Role=[clause, ClauseOut],
					  TransferVarPairsIn-TransferVarPairsOut) :-
	!,
	replace_transfer_variables_in_lhs(ClauseIn, ClauseOut, TransferVarPairsIn-TransferVarPairsOut).
replace_transfer_variables_in_lhs_element(tr(Id, Restrictions),
					  compiled_tr(Restrictions, Var),
					  [TransferVarPair | TransferVarPairsOut]-TransferVarPairsOut) :-
	!,
	check_transfer_var_id_ok(Id),
	check_transfer_var_restrictions_ok(Restrictions),
	TransferVarPair = Id-Var.
replace_transfer_variables_in_lhs_element(Role=tr(Id, Restrictions),
					  Role=compiled_tr(Restrictions, Var),
					  [TransferVarPair | TransferVarPairsOut]-TransferVarPairsOut) :-
	!,
	check_transfer_var_id_ok(Id),
	check_transfer_var_restrictions_ok(Restrictions),
	TransferVarPair = Id-Var.
replace_transfer_variables_in_lhs_element(Other, Other, TransferVarPairsIn-TransferVarPairsIn) :-
	!.

replace_transfer_variables_in_lhs_list([], [], TransferVarPairsIn-TransferVarPairsIn).
replace_transfer_variables_in_lhs_list([F | R], [F1 | R1], TransferVarPairsIn-TransferVarPairsOut) :-
	replace_transfer_variables_in_lhs(F, F1, TransferVarPairsIn-TransferVarPairsNext),
	!,
	replace_transfer_variables_in_lhs_list(R, R1, TransferVarPairsNext-TransferVarPairsOut).

check_transfer_var_id_ok(Id) :-
	atomic(Id),
	!.
check_transfer_var_id_ok(Id) :-
	format2error('~N*** Error: non-atomic transfer var ID: ~w~n', [Id]),
	!,
	fail.

check_transfer_var_restrictions_ok((_P ; _Q)) :-
	!,
	format2error('~N*** Error: disjunctive restrictions in transfer variables no longer supported. Use a macro or duplicate the rule.~n', []),
	fail.
check_transfer_var_restrictions_ok(Term) :-
	nonvar(Term),
	Term = [Key, _Val],
	atomic(Key),
	!.
check_transfer_var_restrictions_ok(Term) :-
	format2error('~N*** Error: bad restrictions in transfer variable: ~w~n', [Term]),
	!,
	fail.

%----------------------------------------------------------------------

check_that_lhs_isnt_just_transfer_variables(LHS) :-
	member(Element, LHS),
	\+ transfer_variable(Element),
	!.
check_that_lhs_isnt_just_transfer_variables(_LHS) :-
	format2error('~N*** Error: LHS in transfer rule may not consist only of transfer variables~n', []),
	fail.

transfer_variable(Element) :-
	compound(Element),
	functor(Element, tr, 2),
	!.

%----------------------------------------------------------------------

check_no_duplicates_in_lhs_transfer_var_pairs(LHSTransferVarPairs) :-
	duplicate_in_lhs_transfer_var_pairs(LHSTransferVarPairs, Id),
	!,
	format2error('~N*** Error: more than one transfer var with ID "~w" in LHS of rule~n', [Id]),
	fail.
check_no_duplicates_in_lhs_transfer_var_pairs(_LHSTransferVarPairs).

duplicate_in_lhs_transfer_var_pairs([Id-_Var | Rest], Id) :-
	member(Id-_OtherVar, Rest),
	!.
duplicate_in_lhs_transfer_var_pairs([_F | R], Id) :-
	duplicate_in_lhs_transfer_var_pairs(R, Id).

%----------------------------------------------------------------------

check_all_transfer_vars_in_lhs_occur_in_rhs([], _RHS) :-
	!.
check_all_transfer_vars_in_lhs_occur_in_rhs([Id-_Var | Rest], RHS) :-
	check_transfer_var_occurs_in_rhs(Id, RHS),
	!,
	check_all_transfer_vars_in_lhs_occur_in_rhs(Rest, RHS).

check_transfer_var_occurs_in_rhs(Id, RHS) :-
	transfer_var_occurs_in_rhs(Id, RHS),
	!.
check_transfer_var_occurs_in_rhs(Id, _RHS) :-
	!,
	format2error('~N*** Error: transfer var with ID "~w" occurs in LHS of rule but not in RHS~n', [Id]),
	fail.

transfer_var_occurs_in_rhs(Id, [tr(Id) | _Rest]) :-
	!.
transfer_var_occurs_in_rhs(Id, [_Role=tr(Id) | _Rest]) :-
	!.
transfer_var_occurs_in_rhs(Id, [[conj, _ConjType | Conjs] | _Rest]) :-
	transfer_var_occurs_in_rhs_list(Id, Conjs),
	!.
transfer_var_occurs_in_rhs(Id, [[clause, Clause] | _Rest]) :-
	transfer_var_occurs_in_rhs(Id, Clause),
	!.
transfer_var_occurs_in_rhs(Id, [_Role=[clause, Clause] | _Rest]) :-
	transfer_var_occurs_in_rhs(Id, Clause),
	!.
transfer_var_occurs_in_rhs(Id, [_F | Rest]) :-
	transfer_var_occurs_in_rhs(Id, Rest).

transfer_var_occurs_in_rhs_list(Id, [F | _R]) :-
	transfer_var_occurs_in_rhs(Id, F),
	!.
transfer_var_occurs_in_rhs_list(Id, [_F | R]) :-
	transfer_var_occurs_in_rhs_list(Id, R).

%----------------------------------------------------------------------

% replace_transfer_variables_in_rhs(+RIn, +LHSTransferVarPairs, -ROut)

replace_transfer_variables_in_rhs([], _LHSTransferVarPairs, []) :-
	!.
replace_transfer_variables_in_rhs([F | Rest], LHSTransferVarPairs, [F1 | Rest1]) :-
	replace_transfer_variables_in_rhs_element(F, LHSTransferVarPairs, F1),
	!,
	replace_transfer_variables_in_rhs(Rest, LHSTransferVarPairs, Rest1).

replace_transfer_variables_in_rhs_element(tr(Id), LHSTransferVarPairs, Var) :-
	member(Id-Var, LHSTransferVarPairs),
	!.
replace_transfer_variables_in_rhs_element(Role=tr(Id), LHSTransferVarPairs, Role=Var) :-
	member(Id-Var, LHSTransferVarPairs),
	!.
replace_transfer_variables_in_rhs_element(tr(Id), _LHSTransferVarPairs, _Var) :-
	!,
	format2error('~N*** Error: transfer var with ID "~w" occurs in RHS of rule but not in LHS~n', [Id]),
	fail.
replace_transfer_variables_in_rhs_element([conj, ConjType | Conjs], LHSTransferVarPairs, [conj, ConjType | Conjs1]) :-
	replace_transfer_variables_in_rhs_list(Conjs, LHSTransferVarPairs, Conjs1),
	!.
replace_transfer_variables_in_rhs_element([clause, Clause], LHSTransferVarPairs, [clause, Clause1]) :-
	replace_transfer_variables_in_rhs(Clause, LHSTransferVarPairs, Clause1),
	!.
replace_transfer_variables_in_rhs_element(Role=[clause, Clause], LHSTransferVarPairs, Role=[clause, Clause1]) :-
	replace_transfer_variables_in_rhs(Clause, LHSTransferVarPairs, Clause1),
	!.
replace_transfer_variables_in_rhs_element(Other, _LHSTransferVarPairs, Other) :-
	!.
replace_transfer_variables_in_rhs_element(F, LHSTransferVarPairs, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [replace_transfer_variables_in_rhs_element(F, LHSTransferVarPairs, F1)]),
	fail.

replace_transfer_variables_in_rhs_list([], _LHSTransferVarPairs, []).
replace_transfer_variables_in_rhs_list([F | R], LHSTransferVarPairs, [F1 | R1]) :-
	replace_transfer_variables_in_rhs(F, LHSTransferVarPairs, F1),
	!,
	replace_transfer_variables_in_rhs_list(R, LHSTransferVarPairs, R1).

%----------------------------------------------------------------------

/*
When we use a transfer variable, we can get one of these situations when there is another element
with the same functor:

compiled_tr([F, _], V), [F, A]
R1=compiled_tr([F, _], V), R2=[F, A]

We need to have the elements in either order. Expand non-deterministically to allow this.
*/

add_transfer_variable_variants([], []).
add_transfer_variable_variants([F | R], [F | R1]) :-
	\+ term_contains_functor(F, compiled_tr/2),
	!,
	add_transfer_variable_variants(R, R1).
add_transfer_variable_variants([F | R], Out) :-
	findall(Variant,
		permute_similar_compiled_transfer_vars(F, Variant),
		Variants),
	append(Variants, R1, Out),
	!,
	add_transfer_variable_variants(R, R1).

permute_similar_compiled_transfer_vars(Var, Var) :-
	var(Var),
	!.
permute_similar_compiled_transfer_vars(A, A) :-
	atomic(A),
	!.
permute_similar_compiled_transfer_vars(In,
				       Out) :-
	safe_subsumes_chk([compiled_tr([F, V], TrV), [F, A] | R], In),
	!,
	[compiled_tr([F, V], TrV), [F, A] | R] = In,
	(   Out = [compiled_tr([F, V], TrV), [F, A] | R1]
	;
	    Out = [[F, A], compiled_tr([F, V], TrV) | R1]
	),
	permute_similar_compiled_transfer_vars(R, R1).
permute_similar_compiled_transfer_vars(In,
				       Out) :-
	safe_subsumes_chk([Role1=compiled_tr([F, V], TrV), Role2=[F, A] | R], In),
	!,
	[Role1=compiled_tr([F, V], TrV), Role2=[F, A] | R] = In,
	(   Out = [Role1=compiled_tr([F, V], TrV), Role2=[F, A] | R1]
	;
	    Out = [Role2=[F, A], Role1=compiled_tr([F, V], TrV) | R1]
	),
	permute_similar_compiled_transfer_vars(R, R1).
permute_similar_compiled_transfer_vars(T, T1) :-
	functor(T, F, N),
	functor(T1, F, N),
	permute_similar_compiled_transfer_vars_args(N, T, T1).

permute_similar_compiled_transfer_vars_args(I, _T, _T1) :-
	I < 1,
	!.
permute_similar_compiled_transfer_vars_args(I, T, T1) :-
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	permute_similar_compiled_transfer_vars(Arg, Arg1),
	I1 is I - 1,
	permute_similar_compiled_transfer_vars_args(I1, T, T1).

%----------------------------------------------------------------------

/*
- Compile classes of substitutable phrases
  - Source: file of records of type 
    ellipsis_class(<Id>, [<Phrase1>,...<PhraseN>]).
  - Compiled form: file of records of type
    ellipsis_class_example(<SourceDiscourse1>, <Id>)
  - Tranformation of <Phrase1> to <SourceDiscourse1>
    - Parse <Phrase1>
    - Transfer <Phrase1> to source discourse representation
    - Remove [utterance_type, phrase]
      - (Would be good to do this more generally)
    - Sort
*/

compile_ellipsis_classes(RawEllipsisClasses, TopLevelCat, SentsFile, TreebankFile, CompiledEllipsisClasses) :-
	raw_ellipsis_classes_to_unparsed_records(RawEllipsisClasses, UnparsedRecords),
	unparsed_records_to_parsed_and_transferred_records(UnparsedRecords, TopLevelCat, SentsFile, TreebankFile, CompiledEllipsisClasses0),
	merge_wh_with_contexts_if_necessary(CompiledEllipsisClasses0, CompiledEllipsisClasses1),
	remove_ellipsis_class_examples_with_failed_parses(CompiledEllipsisClasses1, CompiledEllipsisClasses2),
	postprocess_compiled_ellipsis_classes(CompiledEllipsisClasses2, CompiledEllipsisClasses3),
	safe_remove_duplicates_preserving_order(CompiledEllipsisClasses3, CompiledEllipsisClasses).

raw_ellipsis_classes_to_unparsed_records(RawEllipsisClasses, UnparsedRecords) :-
	raw_ellipsis_classes_to_unparsed_records1(RawEllipsisClasses, UnparsedRecords-[]).

raw_ellipsis_classes_to_unparsed_records1([], UnparsedRecords-UnparsedRecords).
raw_ellipsis_classes_to_unparsed_records1([F | R], UnparsedRecordsIn-UnparsedRecordsOut) :-
	raw_ellipsis_class_to_unparsed_records(F, UnparsedRecordsIn-UnparsedRecordsNext),
	raw_ellipsis_classes_to_unparsed_records1(R, UnparsedRecordsNext-UnparsedRecordsOut).

raw_ellipsis_class_to_unparsed_records(ellipsis_class(Id, Phrases), UnparsedRecordsIn-UnparsedRecordsOut) :-
	raw_ellipsis_class_to_unparsed_records1(Phrases, Id, UnparsedRecordsIn-UnparsedRecordsOut),
	!.
raw_ellipsis_class_to_unparsed_records(X, _Y) :-
	\+ safe_subsumes_chk(ellipsis_class(_Id, _Phrases), X),
	format2error('~N*** Error: unknown entry ~w in ellipsis class file~n', [X]),
	fail.
raw_ellipsis_class_to_unparsed_records(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [raw_ellipsis_class_to_unparsed_records(X, Y)]),
	fail.

raw_ellipsis_class_to_unparsed_records1([], _Id, UnparsedRecordsIn-UnparsedRecordsIn) :-
	!.
raw_ellipsis_class_to_unparsed_records1([F | R], Id, [F1, F2 | R1]-Out) :-
	raw_ellipsis_class_to_unparsed_records(F, Id, F1, F2),
	!,
	raw_ellipsis_class_to_unparsed_records1(R, Id, R1-Out).
raw_ellipsis_class_to_unparsed_records1([F | R], Id, [F1 | R1]-Out) :-
	raw_ellipsis_class_to_unparsed_record(F, Id, F1),
	!,
	raw_ellipsis_class_to_unparsed_records1(R, Id, R1-Out).

raw_ellipsis_class_to_unparsed_records(wh_with_context-[Sent1, Sent2], Id,
				       sent(Sent1, wh-Id),
				       sent(Sent2, wh_context-Id)) :-
	!.

raw_ellipsis_class_to_unparsed_record(wh-Sent, Id, sent(Sent, wh-Id)) :-
	!.
raw_ellipsis_class_to_unparsed_record(Sent-Constraints, Id, sent(Sent, Id, Constraints)).
raw_ellipsis_class_to_unparsed_record(Sent, Id, sent(Sent, Id)).

unparsed_records_to_parsed_and_transferred_records(UnparsedRecords, TopLevelCat, SentsFile, TreebankFile, CompiledEllipsisClasses) :-
	unparsed_records_to_parsed_records(UnparsedRecords, TopLevelCat, SentsFile, TreebankFile, ParsedRecords),
	parsed_records_to_transferred_records(ParsedRecords, CompiledEllipsisClasses),
	!.

unparsed_records_to_parsed_records(UnparsedRecords, TopLevelCat, SentsFile, TreebankFile, TreebankList) :-
	list_to_prolog_file(UnparsedRecords, SentsFile),
	user:get_regulus_config_item(parsing_history_file, ParsingHistoryFile),
	user:make_ebl_training_data(SentsFile, TopLevelCat, [], ParsingHistoryFile, TreebankFile),
	prolog_file_to_list(TreebankFile, TreebankList),
	!.

parsed_records_to_transferred_records([], []).
parsed_records_to_transferred_records([F | R], [F1 | R1]) :-
	parsed_record_to_transferred_record(F, F1),
	!,
	parsed_records_to_transferred_records(R, R1).
parsed_records_to_transferred_records([_F | R], R1) :-
	!,
	parsed_records_to_transferred_records(R, R1).

parsed_record_to_transferred_record(Term, _) :-
	\+ safe_subsumes_chk(example(_TopCat, _Tree, _LF, _WordList, _Id), Term),
	!,
	format2error('~N*** Error: unexpected term in treebank file: ~w~n', [Term]),
	fail.
parsed_record_to_transferred_record(example(_TopCat, _Tree, LF, WordList, Id),
				    ellipsis_class_example(failed_to_parse(Atom), Id)) :-	
	LF = 'NO_LF',
	!,
	join_with_spaces(WordList, Atom),
	format2error('~N~n*** WARNING: ellipsis class example failed to parse: "~w"~n', [Atom]).
parsed_record_to_transferred_record(example(_TopCat, _Tree, LF, _WordList, Id),
				    ellipsis_class_example(SortedSourceDiscourse, Id)) :-
	transfer_representation_to_source_discourse(LF, SourceDiscourse),
	%remove_utterance_type(SourceDiscourse, SourceDiscourseWithoutUtteranceType),
	remove_phrase_utterance_marking(SourceDiscourse, SourceDiscourseWithoutUtteranceType),
	% We want WH and WH_CONTEXT entries to stay unchanged, but for everything else 
	% we strip out the lexical content
	(   Id = wh-_RealId ->
	    SourceDiscourseWithoutUtteranceType1 = SourceDiscourseWithoutUtteranceType ;

	    Id = wh_context-_RealId ->
	    SourceDiscourseWithoutUtteranceType1 = SourceDiscourseWithoutUtteranceType ;

	    leave_only_types_in_representation(SourceDiscourseWithoutUtteranceType, SourceDiscourseWithoutUtteranceType1)
	),
	sort(SourceDiscourseWithoutUtteranceType1, SortedSourceDiscourse),
	%make_ground(SortedSourceDiscourse),
	!.
parsed_record_to_transferred_record(example(_TopCat, _Tree, _LF, WordList, Id),
				    ellipsis_class_example(failed_to_compile(Atom), Id)) :-
	join_with_spaces(WordList, Atom),
	format2error('~N~n*** WARNING: ellipsis class example failed to compile: "~w"~n', [Atom]).

%remove_utterance_type([], []).
%remove_utterance_type([[utterance_type, _] | R1], R) :-
%	!,
%	remove_utterance_type(R1, R).
%remove_utterance_type([F | R], [F | R1]) :-
%	!,
%	remove_utterance_type(R, R1).

leave_only_types_in_representation([], []).
leave_only_types_in_representation([[Key, _Value] | R], [[Key, _AnonymousValue] | R1]) :-
	leave_only_types_in_representation(R, R1).
leave_only_types_in_representation([_Role=[Key, _Value] | R], [_AnonymousRole=[Key, _AnonymousValue] | R1]) :-
	leave_only_types_in_representation(R, R1).

merge_wh_with_contexts_if_necessary([], []).
merge_wh_with_contexts_if_necessary([ellipsis_class_example(Example1, wh-Id),
				     ellipsis_class_example(Example2, wh_context-Id) |
				    R],
				    [ellipsis_class_example([Example1, Example2], wh_with_context-Id) |
				    R1]) :-
	!,
	merge_wh_with_contexts_if_necessary(R, R1).
merge_wh_with_contexts_if_necessary([F | R], [F | R1]) :-
	merge_wh_with_contexts_if_necessary(R, R1).

remove_ellipsis_class_examples_with_failed_parses([], []).
remove_ellipsis_class_examples_with_failed_parses([F | R], R1) :-
	failed_parse_example(F),
	!,
	remove_ellipsis_class_examples_with_failed_parses(R, R1).
remove_ellipsis_class_examples_with_failed_parses([F | R], [F | R1]) :-
	!,
	remove_ellipsis_class_examples_with_failed_parses(R, R1).

failed_parse_example(ellipsis_class_example(failed_to_parse(_), _Id)) :-
	!.
failed_parse_example(ellipsis_class_example(failed_to_compile(_), _Id)) :-
	!.
failed_parse_example(ellipsis_class_example([Example1, Example2], _Id)) :-
	(   Example1 = failed_to_parse(_) ;
	    Example2 = failed_to_parse(_)
	).	

postprocess_compiled_ellipsis_classes([], []).
postprocess_compiled_ellipsis_classes([F | R], [F1 | R1]) :-
	postprocess_compiled_ellipsis_class_entry(F, F1),
	postprocess_compiled_ellipsis_classes(R, R1).

postprocess_compiled_ellipsis_class_entry(In, Out) :-
	In = ellipsis_class_example([Representation, Context], wh_with_context-Tag),
	make_transfer_representation_canonical_and_unpack_if_necessary(Representation, Representation1),
	make_transfer_representation_canonical_and_unpack_if_necessary(Context, Context1), 
	copy_term([Representation1, Context1], RepresentationAndContext2),
	make_ground(RepresentationAndContext2),
	Out = ellipsis_class_example(RepresentationAndContext2, wh_with_context-Tag),
	!.
postprocess_compiled_ellipsis_class_entry(In, Out) :-
	In = ellipsis_class_example(Representation, Tag),
	make_transfer_representation_canonical_and_unpack_if_necessary(Representation, Representation1),
	copy_term(Representation1, Representation2),
	make_ground(Representation2),
	Out = ellipsis_class_example(Representation2, Tag),
	!.
postprocess_compiled_ellipsis_class_entry(In, Out) :-
	format2error('~N*** Error: bad call: ~w~n', [postprocess_compiled_ellipsis_class_entry(In, Out)]),
	fail.

%----------------------------------------------------------------------

transfer_representation(Source, Target) :-
	transfer_representation(Source, Target, _Trace).

transfer_representation(Source, Target, Trace) :-
	make_transfer_representation_canonical(Source, SourceOS),
	transfer_representation1(SourceOS, SourceOS, Target, Trace, direct).

transfer_representation_to_source_discourse(Source, SourceDiscourse) :-
	transfer_representation_to_source_discourse(Source, SourceDiscourse, _Trace).

transfer_representation_to_source_discourse(Source, SourceDiscourse, Trace) :-
	transfer_representation_to_source_discourse(Source, SourceDiscourse, Trace, user).

transfer_representation_to_source_discourse(Source, SourceDiscourse, [], Module) :-
	\+ current_predicate(Module:to_source_discourse_rule/6),
	\+ current_predicate(Module:to_source_discourse_lexicon/4),    
	Source = SourceDiscourse,
	!.
transfer_representation_to_source_discourse(Source, SourceDiscourse, Trace, Module) :-
	make_transfer_representation_canonical(Source, SourceOS),
	transfer_representation1(SourceOS, Module, SourceOS, SourceDiscourse0, Trace, to_source_discourse),
	make_transfer_representation_canonical_and_unpack_if_necessary(SourceDiscourse0, SourceDiscourse).

transfer_representation_to_interlingua(Source, Interlingua) :-
	transfer_representation_to_interlingua(Source, Interlingua, _Trace).

transfer_representation_to_interlingua(Source, Interlingua, Trace) :-
	transfer_representation_to_interlingua(Source, Interlingua, Trace, user).

transfer_representation_to_interlingua(Source, Interlingua, Trace, Module) :-
	make_transfer_representation_canonical(Source, SourceOS),
	transfer_representation1(SourceOS, Module, SourceOS, Interlingua0, Trace, to_interlingua),
	make_transfer_representation_canonical_and_unpack_if_necessary(Interlingua0, Interlingua).

transfer_representation_from_interlingua(Interlingua, Target) :-
	transfer_representation_from_interlingua(Interlingua, Target, _Trace).

transfer_representation_from_interlingua(Interlingua, Target, Trace) :-
	transfer_representation_from_interlingua(Interlingua, Target, Trace, user).

transfer_representation_from_interlingua(Interlingua, Target, Trace, Module) :-
	make_transfer_representation_canonical(Interlingua, InterlinguaOS),
	transfer_representation1(InterlinguaOS, Module, InterlinguaOS, Target0, Trace, from_interlingua),
	make_transfer_representation_canonical_and_unpack_if_necessary(Target0, Target).

transfer_representation_to_surface_interlingua_using_context(Source, ContextIn, ContextOut, Target) :-
	transfer_representation_to_surface_interlingua_using_context(Source, ContextIn, ContextOut, Target, _Tree).

transfer_representation_to_surface_interlingua_using_context(Source, ContextIn, ContextOut, Target, Tree) :-
	transfer_representation_to_interlingua_and_surface_interlingua_using_context(Source, check_interlingua, ContextIn, ContextOut, _InterlinguaOS, Target, Tree).

transfer_representation_to_interlingua_and_surface_interlingua_using_context(Source, ContextIn, ContextOut, InterlinguaOS, Target, Tree) :-
	transfer_representation_to_interlingua_and_surface_interlingua_using_context(Source, check_interlingua, ContextIn, ContextOut, InterlinguaOS, Target, Tree).

transfer_representation_to_interlingua_and_surface_interlingua_using_context(Source, Tag, ContextIn, ContextOut, InterlinguaOS, Target, Tree) :-
	get_module_for_alternate_interlingua_structure(Tag, Module),
	current_predicate(Module:check_interlingua/3),
	transfer_representation_to_source_discourse(Source, UnresolvedSourceDiscourse, _SDTrace, Tag),

	get_preceding_source_discourse(PrecedingSourceDiscourse, ContextIn),
	perform_resolution(UnresolvedSourceDiscourse, PrecedingSourceDiscourse, ResolvedSourceDiscourse, _ResolutionProcessing),
	make_transfer_representation_canonical(ResolvedSourceDiscourse, ResolvedSourceDiscourseOS),
	set_preceding_source_discourse(ResolvedSourceDiscourseOS, ContextIn, ContextNext),

	transfer_representation1(ResolvedSourceDiscourseOS, Tag, ResolvedSourceDiscourseOS, Interlingua, _Trace1, to_interlingua),

	make_transfer_representation_canonical_and_unpack_if_necessary(Interlingua, InterlinguaOS),
	set_preceding_resolved_interlingua(InterlinguaOS, ContextNext, ContextOut),
	%check_interlingua_structure(InterlinguaOS, Target, Tree).
	check_interlingua_structure(InterlinguaOS, Tag, Target, Tree, _OtherSurfaceForms).

transfer_representation_through_interlingua(Source, ContextIn, ContextOut, Target) :-
	transfer_representation_through_interlingua(Source, ContextIn, ContextOut, Target).
	
transfer_representation_through_interlingua(Source, Module, ContextIn, ContextOut, Target) :-	
	transfer_representation_to_source_discourse(Source, UnresolvedSourceDiscourse, _Trace0, Module),

	get_preceding_source_discourse(PrecedingSourceDiscourse, ContextIn),
	perform_resolution(UnresolvedSourceDiscourse, PrecedingSourceDiscourse, ResolvedSourceDiscourse, _ResolutionProcessing),
	make_transfer_representation_canonical_and_unpack_if_necessary(ResolvedSourceDiscourse, ResolvedSourceDiscourseOS),
	set_preceding_source_discourse(ResolvedSourceDiscourseOS, ContextIn, ContextNext1),

	transfer_representation1(ResolvedSourceDiscourseOS, Module, ResolvedSourceDiscourseOS, ResolvedInterlingua, _Trace1, to_interlingua),
	make_transfer_representation_canonical_and_unpack_if_necessary(ResolvedInterlingua, ResolvedInterlinguaOS),

	check_interlingua_structure(ResolvedInterlinguaOS, _InterlinguaSurfaceForm),

	transfer_representation1(ResolvedInterlinguaOS, Module, ResolvedInterlinguaOS, Target0, _Trace2, from_interlingua),
	make_transfer_representation_canonical_and_unpack_if_necessary(Target0, Target),
	set_preceding_resolved_interlingua(ResolvedInterlinguaOS, ContextNext1, ContextOut).

%----------------------------------------------------------------------

transfer_representation1(Source, Context, Target, Trace, RuleType) :-
	transfer_representation1(Source, user, Context, Target, Trace, RuleType).

% Flatten transfer representation in case we have transfer variables that produce lists.
transfer_representation1(Source, Module0, Context, Target, Trace, RuleType) :-
	(   transfer_resources_defined_for_module(Module0) ->
	    Module = Module0
	;
	    otherwise ->
	    Module = user
	),
	transfer_representation2(Source, Module, Context, Target0, Trace-[], RuleType, transfer_vars_ok),
	flatten_transfer_representation(Target0, Target1),
	replace_explicitly_uninstantiated_roles(Target1, Target),
	print_transfer_trace(RuleType, Trace),
	!.

transfer_representation2([], _Module, _Context, [], TraceIn-TraceIn, _RuleType, _TransferVarsOK) :-
	!.
% Always use a complex rule if you can. This might get us into trouble
% if we use a bad rule but had a possibility to translate compositionally,
% but if we don't do this we're liable to run into a combinatorial
% explosion.
transfer_representation2([F | R], Module, Context, Result, TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	% Treat conj/clause elements specially since they may match rules containing conj/clauses with transfer variables
	(   F = [conj | _] ->
	    FirstEltPattern = [conj | _]
	;
	    F = [clause | _] ->
	    FirstEltPattern = [clause | _]
	;
	    F = ( _Role=[clause | _] ) ->
	    FirstEltPattern = [clause | _]
	;
	    F = ( _Role=F1) ->
	    FirstEltPattern = F1
	;
	    FirstEltPattern = F
	),
	transfer_using_complex_rule(FirstEltPattern, Module, [F | R], Context, TraceIn-TraceNext, RuleType, TransferVarsOK, RHS, RemainingR),
	!,
	transfer_representation2(RemainingR, Module, Context, TransferredRemainingR, TraceNext-TraceOut, RuleType, TransferVarsOK),
	append(RHS, TransferredRemainingR, Result).
transfer_representation2([[clause, Clause] | R], Module, Context, [[clause, Clause1] | R1], TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	add_main_clause_context_to_subordinate_clause(Context, Clause, SubordinateContext),
	transfer_representation2(Clause, Module, SubordinateContext, Clause1, TraceIn-TraceNext, RuleType, transfer_vars_ok),
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext-TraceOut, RuleType, TransferVarsOK).
transfer_representation2([LHSRole=[clause, Clause] | R], Module, Context, [RHSRole=[clause, Clause1] | R1],
			 TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	add_main_clause_context_to_subordinate_clause(Context, Clause, SubordinateContext),
	transfer_representation2(Clause, Module, SubordinateContext, Clause1, TraceIn-TraceNext1, RuleType, transfer_vars_ok),
	transfer_roles([LHSRole=[clause, Clause]], Module, Context, [RHSRole], TraceNext1-TraceNext2, RuleType),
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext2-TraceOut, RuleType, TransferVarsOK).
% So far only a very simple treatment of conjunction. This doesn't handle the case where complex rules
% need to be applied to material which is partly inside and partly outside the conjoined phrase.
transfer_representation2([[conj, Conj | Conjuncts] | R], Module, Context, [[conj, Conj | Conjuncts1] | R1], TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	transfer_representation2_list(Conjuncts, Module, Context, Conjuncts1, TraceIn-TraceNext, RuleType, TransferVarsOK),
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext-TraceOut, RuleType, TransferVarsOK).
transfer_representation2([F | R], Module, Context, [F1 | R1], [TraceElement | TraceNext1]-TraceOut, RuleType, TransferVarsOK) :-
	transfer_lexicon_entry_of_specified_type(F, Module, LHSForRoles, F1, LineInfo, RuleType),
	TraceElement = ([F]-[F1]):LineInfo,
	transfer_roles_for_item_list(LHSForRoles, Module, Context, [F1], TraceNext1-TraceNext2, RuleType),
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext2-TraceOut, RuleType, TransferVarsOK).
% For to_source_discourse, default is to translate everything as itself.
% Try to translate role if possible.
transfer_representation2([LHSRole=F | R], Module, Context, [RHSRole=F | R1], TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	( RuleType = to_source_discourse ; translate_as_identity_by_default ),
	transfer_roles([LHSRole=F], Module, Context, [RHSRole], TraceIn-TraceNext1, RuleType),
	MainTraceElement = ([F]-[F]):default,
	TraceNext1 = [MainTraceElement | TraceNext2],
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext2-TraceOut, RuleType, TransferVarsOK).
% Otherwise just leave everything unchanged.
transfer_representation2([F | R], Module, Context, [F | R1], [TraceElement | TraceNext]-TraceOut, RuleType, TransferVarsOK) :-
	( RuleType = to_source_discourse ; translate_as_identity_by_default ),
	TraceElement = ([F]-[F]):default,
	!,
	transfer_representation2(R, Module, Context, R1, TraceNext-TraceOut, RuleType, TransferVarsOK).
% For everything except to_source_discourse, fail if no translation rule.
transfer_representation2([F | R], Module, Context, [unable_to_translate:F | R1], TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	!,
	transfer_representation2(R, Module, Context, R1, TraceIn-TraceOut, RuleType, TransferVarsOK).
% This last clause probably never gets called, but just in case...
transfer_representation2(Other, _Module, _Context, Result, TraceIn-TraceIn, _RuleType, transfer_vars_ok) :-
	Result = unable_to_translate:Other,
	!.

% Should not get called...
transfer_representation2(A, B, C, D, E, F) :-
	format('~NIncorrect call:~n', []),
	prettyprint(transfer_representation2(A, B, C, D, E, F)),
	format('~N', []),
	trace.

translate_as_identity_by_default :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(translate_as_identity_by_default, yes).

% Use the complex rule with the longest LHS. Resolve any ties by using the rule order in the source file, since
% keysort/2 preserves order when possible.
transfer_using_complex_rule(FirstEltPattern, Module, ItemsIn, Context,
			    TraceIn-TraceOut, RuleType, TransferVarsOK, RHS, ItemsRemaining) :-
	findall(NegLengthLHS1-[RHS1, TraceIn1-TraceOut1, ItemsRemaining1],
		transfer_using_complex_rule1(FirstEltPattern, Module, ItemsIn, Context, TraceIn1-TraceOut1, RuleType, TransferVarsOK,
					     RHS1, ItemsRemaining1, NegLengthLHS1),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_NegGreatestLengthForLHS-[RHS, TraceIn-TraceOut, ItemsRemaining] | _Rest],
	!.

transfer_using_complex_rule1(FirstEltPattern, Module, ItemsIn, Context, TraceIn-TraceOut, RuleType, TransferVarsOK,
			     RHS, ItemsRemaining, NegLengthLHS) :-
	transfer_rule_of_specified_type(FirstEltPattern, Module, LHS, LHSForRoles, RHS, Conditions, LineInfo, RuleType),
	check_transfer_rule_context_conditions(Conditions, Context, 0-ContextCount),
	match_transfer_rule(LHS, Module, TraceLHS, Context, ItemsIn, ItemsRemaining, RuleType, TransferVarsOK),
	TraceElement = (TraceLHS-RHS):LineInfo,
	TraceIn = [TraceElement | TraceNext],
	transfer_roles_for_item_list(LHSForRoles, Module, Context, RHS, TraceNext-TraceOut, RuleType),
	length(LHS, LengthLHS),
	NegLengthLHS is -1 * ( LengthLHS + 0.01 * ContextCount ).

transfer_representation2_list([], _Module, _Context, [], TraceIn-TraceIn, _RuleType, _TransferVarsOK).
transfer_representation2_list([F | R], Module, Context, [F1 | R1], TraceIn-TraceOut, RuleType, TransferVarsOK) :-
	transfer_representation2(F, Module, Context, F1, TraceIn-TraceNext, RuleType, TransferVarsOK),
	!,
	transfer_representation2_list(R, Module, Context, R1, TraceNext-TraceOut, RuleType, TransferVarsOK).

%----------------------------------------------------------------------

transfer_roles_for_item_list(LHS, Module, Context, RHS, TraceIn-TraceOut, RuleType) :-
	get_roles_from_items(RHS, RHSRoles),
	!,
	transfer_roles(LHS, Module, Context, RHSRoles, TraceIn-TraceOut, RuleType).
transfer_roles_for_item_list(_LHS, _Module, _Context, _RHS, TraceIn-TraceIn, _RuleType).

get_roles_from_items([], []).
get_roles_from_items([Role=_ | Rest], [Role | Rest1]) :-
	get_roles_from_items(Rest, Rest1).

%----------------------------------------------------------------------

replace_explicitly_uninstantiated_roles([], []) :-
	!.
replace_explicitly_uninstantiated_roles([[conj, ConjType | Conjs] | R], [[conj, ConjType | Conjs1] | R1]) :-
	replace_explicitly_uninstantiated_roles_list(Conjs, Conjs1),
	!,
	replace_explicitly_uninstantiated_roles(R, R1).
replace_explicitly_uninstantiated_roles([Role=[clause, Clause] | R], [Role1=[clause, Clause1] | R1]) :-
	replace_explicitly_uninstantiated_role(Role, Role1),
	replace_explicitly_uninstantiated_role(Clause, Clause1),
	!,
	replace_explicitly_uninstantiated_roles(R, R1).
replace_explicitly_uninstantiated_roles([Role=Item | R], [Role1=Item | R1]) :-
	replace_explicitly_uninstantiated_role(Role, Role1),
	!,
	replace_explicitly_uninstantiated_roles(R, R1).
replace_explicitly_uninstantiated_roles([F | R], [F | R1]) :-
	!,
	replace_explicitly_uninstantiated_roles(R, R1).
replace_explicitly_uninstantiated_roles(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [replace_explicitly_uninstantiated_roles(X, Y)]),
	fail.

replace_explicitly_uninstantiated_roles_list([], []).
replace_explicitly_uninstantiated_roles_list([F | R], [F1 | R1]) :-
	replace_explicitly_uninstantiated_roles(F, F1),
	!,
	replace_explicitly_uninstantiated_roles_list(R, R1).

replace_explicitly_uninstantiated_role(R, R1) :-
	R == '*explicitly_uninstantiated*',
	R1 = _NewVar,
	!.
replace_explicitly_uninstantiated_role(R, R) :-
	!.
replace_explicitly_uninstantiated_role(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [replace_explicitly_uninstantiated_role(X, Y)]),
	fail.

/*
When we apply a transfer rule, there are several possibilities for
assigning the values of the roles on the RHS:

1. The transfer rule may explicitly assign values to all of the roles
on the RHS.

2. None of the roles on the RHS are explicitly assigned values by the
transfer_rule.  We apply role_transfer rules to all the roles on the
LHS.

2a. If all the roles produced this way have the same non-var value R',
we set the value of the roles on the RHS to R'. The most common example of
this is when we have a lexical transfer rule, which will have exactly
one pair on each side. We apply a role_transfer rules to the role R
attached to the single pair on the LHS, to get R'. This will be the
value of the role attached to the RHS.

NB: roles may be explicitly assigned an uninstantiated value. This
is why we need the *explicitly_uninstantiated* tag in the compiled
rules.
   
2b. If the roles produced by applying role_transfer rules to the roles
on the LHS do not all have the same value, then we leave the values of
the roles on the RHS uninstantiated.

3. The transfer rule assigns values to some but not all of the roles
on the RHS, and role-transfer rules do not map all the roles on the
LHS to the same value. The uninstantiated role values on the RHS stay
uninstantiated.

4. (Not completely clear) The transfer rule sets some but not all of
the roles on the RHS, and role-transfer rules maps all the roles on
the LHS to the same value R'. There are two reasonable things we can
do with the uninstantiated roles on the RHS:

4a. We keep the uninstantiated roles on the RHS uninstantiated. 

4b. Possible alternative: we set the uninstantiated roles on the RHS to
R'. This is what we are doing now, though it seems a little risky.

*/

% Case 1 
transfer_roles(_LHS, _Module, _Context, RHSRoles, TraceIn-TraceOut, _RuleType) :-
	ground(RHSRoles),
	TraceOut = TraceIn,
	!.
% Case 2
transfer_roles(LHS, Module, Context, RHSRoles, TraceIn-TraceOut, RuleType) :-
	transfer_role_list(LHS, Module, Context, TransferredLHSRoles, TraceIn0-TraceOut0, RuleType),
	(   % Case 2a
	    ( all_vars_list(RHSRoles), single_value_list(TransferredLHSRoles, Value) ) ->
	    single_value_list(RHSRoles, Value),
	    [TraceIn0, TraceOut0] = [TraceIn, TraceOut]
	;
	    % Case 2b
	    all_vars_list(RHSRoles) ->
	    TraceIn = TraceOut
	;
	    % Case 3
	    ( \+ all_vars_list(RHSRoles), \+ single_value_list(TransferredLHSRoles, Value) ) ->
	    TraceIn = TraceOut
	;
	    % Case 4b
	    single_value_list(TransferredLHSRoles, Value) ->
	    set_uninstantiated_elements_in_list_to_value(RHSRoles, Value),
	    [TraceIn0, TraceOut0] = [TraceIn, TraceOut]
	;
	    % Any other cases
	    otherwise ->
	    TraceIn = TraceOut
	),
	!.
% Cases 3 and 4
transfer_roles(_LHS, _Module, _Context, _RHSRoles, TraceIn-TraceIn, _RuleType) :-
	!.

transfer_role_list([], _Module, _Context, [], TraceIn-TraceIn, _RuleType).
transfer_role_list([F | R], Module, Context, [F1 | R1], TraceIn-TraceOut, RuleType) :-
	transfer_role(F, Module, Context, F1, TraceIn-TraceNext, RuleType),
	transfer_role_list(R, Module, Context, R1, TraceNext-TraceOut, RuleType).

% Use the complex rule with the longest conditions. Resolve any ties by using the rule order in the source file, since
% keysort/2 preserves order when possible.
transfer_role(Role=Item, Module, Context, RoleOut, TraceIn-TraceOut, RuleType) :-
	LocalContext = [role_context(Item) | Context],
	findall(NegLength-[Role1, TraceIn1-TraceOut1],
		transfer_role1(Role, Module, LocalContext, Role1, TraceIn1-TraceOut1, NegLength, RuleType),
		Pairs),
	keysort(Pairs, SortedPairs),
	SortedPairs = [_BestNegLength-[RoleOut, TraceIn-TraceOut] | _Rest],
	!.

transfer_role1(Role, _Module, _Context, Role, TraceIn-TraceIn, NegLength, _RuleType) :-
	var(Role),
	NegLength = 0,
	!.
transfer_role1(Role, Module, Context, RoleOut, TraceIn-TraceOut, NegLength, RuleType) :-
	nonvar(Role),
	role_transfer_rule_of_specified_type(Role, Module, RoleOut, Conditions, LineInfo, RuleType),
	check_transfer_rule_context_conditions(Conditions, Context, 0-ContextCount),
	TraceElement = (Role-RoleOut):LineInfo,
	TraceIn = [TraceElement | TraceOut],
	conditions_length(Conditions, Length),
	NegLength is -1 * ( Length + 0.01 * ContextCount ).
% If we are translating to source discourse, can always translate a role to itself.
% Make length 0 so that this is the least preferred translation if a real rule applies.
transfer_role1(Role, _Module, _Context, Role, TraceIn-TraceIn, NegLength, RuleType) :-
	RuleType = to_source_discourse,
	NegLength = 0.

%----------------------------------------------------------------------

transfer_rule_of_specified_type(FirstEltPattern, Module, LHS, LHSForRoles, RHS, Conditions, LineInfo, direct) :-
	current_predicate(Module:transfer_rule/6),
	Module:transfer_rule(FirstEltPattern, LHS, LHSForRoles, RHS, Conditions, LineInfo).
transfer_rule_of_specified_type(FirstEltPattern, Module, LHS, LHSForRoles, RHS, Conditions, LineInfo, to_source_discourse) :-
	current_predicate(Module:to_source_discourse_rule/6),
	Module:to_source_discourse_rule(FirstEltPattern, LHS, LHSForRoles, RHS, Conditions, LineInfo).
transfer_rule_of_specified_type(FirstEltPattern, Module, LHS, LHSForRoles, RHS, Conditions, LineInfo, to_interlingua) :-
	current_predicate(Module:to_interlingua_rule/6),
	Module:to_interlingua_rule(FirstEltPattern, LHS, LHSForRoles, RHS, Conditions, LineInfo).
transfer_rule_of_specified_type(FirstEltPattern, Module, LHS, LHSForRoles, RHS, Conditions, LineInfo, from_interlingua) :-
	current_predicate(Module:from_interlingua_rule/6),
	Module:from_interlingua_rule(FirstEltPattern, LHS, LHSForRoles, RHS, Conditions, LineInfo).

transfer_lexicon_entry_of_specified_type(LHS, Module, LHSForRoles, RHS, LineInfo, direct) :-
	current_predicate(Module:transfer_lexicon/4),
	Module:transfer_lexicon(LHS, LHSForRoles, RHS, LineInfo).
transfer_lexicon_entry_of_specified_type(LHS, Module, LHSForRoles, RHS, LineInfo, to_source_discourse) :-
	current_predicate(Module:to_source_discourse_lexicon/4),
	Module:to_source_discourse_lexicon(LHS, LHSForRoles, RHS, LineInfo).
transfer_lexicon_entry_of_specified_type(LHS, Module, LHSForRoles, RHS, LineInfo, to_interlingua) :-
	current_predicate(Module:to_interlingua_lexicon/4),
	Module:to_interlingua_lexicon(LHS, LHSForRoles, RHS, LineInfo).
transfer_lexicon_entry_of_specified_type(LHS, Module, LHSForRoles, RHS, LineInfo, from_interlingua) :-
	current_predicate(Module:from_interlingua_lexicon/4),
	Module:from_interlingua_lexicon(LHS, LHSForRoles, RHS, LineInfo).

role_transfer_rule_of_specified_type(LHS, Module, RHS, Conditions, LineInfo, direct) :-
	current_predicate(Module:role_transfer_rule/4),
	Module:role_transfer_rule(LHS, RHS, Conditions, LineInfo).
role_transfer_rule_of_specified_type(LHS, Module, RHS, Conditions, LineInfo, to_source_discourse) :-
	current_predicate(Module:to_source_discourse_role_rule/4),
	Module:to_source_discourse_role_rule(LHS, RHS, Conditions, LineInfo).
role_transfer_rule_of_specified_type(LHS, Module, RHS, Conditions, LineInfo, to_interlingua) :-
	current_predicate(Module:to_interlingua_role_rule/4),
	Module:to_interlingua_role_rule(LHS, RHS, Conditions, LineInfo).
role_transfer_rule_of_specified_type(LHS, Module, RHS, Conditions, LineInfo, from_interlingua) :-
	current_predicate(Module:from_interlingua_role_rule/4),
	Module:from_interlingua_role_rule(LHS, RHS, Conditions, LineInfo).

%----------------------------------------------------------------------

% add_main_clause_context_to_subordinate_clause(+Context, +Clause, -SubordinateContext)

add_main_clause_context_to_subordinate_clause([], Context, Context) :-
	!.
add_main_clause_context_to_subordinate_clause([F | R], Context, [above(F) | R1]) :-
	add_main_clause_context_to_subordinate_clause(R, Context, R1),
	!.
add_main_clause_context_to_subordinate_clause(Context, Clause, SubordinateContext) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_main_clause_context_to_subordinate_clause(Context, Clause, SubordinateContext)]),
	!.

%----------------------------------------------------------------------

% match_transfer_rule(+LHS, +Module, -TraceLHS, +Context, +ItemsIn, -ItemsOut, +RuleType, +TransferVarsOK)

match_transfer_rule([], _Module, [], _Context, ItemsIn, ItemsIn, _RuleType, _TransferVarsOK).
match_transfer_rule([F | R], Module, [TraceF | TraceR], Context, ItemsIn, ItemsOut, RuleType, TransferVarsOK) :-
	match_transfer_rule_element(F, Module, TraceF, Context, ItemsIn, ItemsNext, RuleType, TransferVarsOK),
	!,
	match_transfer_rule(R, Module, TraceR, Context, ItemsNext, ItemsOut, RuleType, TransferVarsOK).

match_transfer_rule_element([conj, ConjType | Conjs], _Module, [conj, ConjType | Conjs], _Context, ItemsIn, ItemsOut, _RuleType, _TransferVarsOK) :-
	!,
	remove_from_list([conj, ConjType | Conjs], ItemsIn, ItemsOut).
match_transfer_rule_element(Role=[clause, Clause], _Module, Role=[clause, Clause], _Context, ItemsIn, ItemsOut, _RuleType, _TransferVarsOK) :-
	clause_body_contains_no_compiled_transfer_variables(Clause),
	!,
	remove_from_list(Role=[clause, Clause], ItemsIn, ItemsOut).
match_transfer_rule_element([clause, Clause], _Module, [clause, Clause], _Context, ItemsIn, ItemsOut, _RuleType, _TransferVarsOK) :-
	clause_body_contains_no_compiled_transfer_variables(Clause),
	!,
	remove_from_list([clause, Clause], ItemsIn, ItemsOut).
match_transfer_rule_element(Role=[clause, Clause], Module, Role=[clause, TraceClause], _Context, ItemsIn, ItemsOut, RuleType, transfer_vars_ok) :-
	remove_from_list(Role=[clause, Clause1], ItemsIn, ItemsOut),
	match_transfer_rule(Clause, Module, TraceClause, Clause, Clause1, [], RuleType, transfer_vars_ok),
	!.
match_transfer_rule_element([clause, Clause], Module, [clause, TraceClause], _Context, ItemsIn, ItemsOut, RuleType, transfer_vars_ok) :-
	remove_from_list([clause, Clause1], ItemsIn, ItemsOut),
	match_transfer_rule(Clause, Module, TraceClause, Clause, Clause1, [], RuleType, transfer_vars_ok),
	!.
match_transfer_rule_element(Role=compiled_tr(Restrictions, Var), Module, Role=MatchingElement, Context, ItemsIn, ItemsOut, RuleType, transfer_vars_ok) :-
	remove_from_list(Role=MatchingElement, ItemsIn, ItemsOut),
	item_matches_transfer_variable_restrictions(Restrictions, MatchingElement),
	transfer_representation2([Role=MatchingElement], Module, Context, Result, _SubTrace-[], RuleType, transfer_vars_not_ok),
	strip_roles_from_expression(Result, Var),
	!.
match_transfer_rule_element(compiled_tr(Restrictions, Var), Module, MatchingElement, Context, ItemsIn, ItemsOut, RuleType, transfer_vars_ok) :-
	remove_from_list(MatchingElement, ItemsIn, ItemsOut),
	item_matches_transfer_variable_restrictions(Restrictions, MatchingElement),
	transfer_representation2([MatchingElement], Module, Context, Var, _SubTrace-[], RuleType, transfer_vars_not_ok),
	!.
match_transfer_rule_element(MatchingElement, _Module, MatchingElement, _Context, ItemsIn, ItemsOut, _RuleType, _TransferVarsOK) :-
	remove_from_list(MatchingElement, ItemsIn, ItemsOut),
	!.

strip_roles_from_expression([], []) :-
	!.
strip_roles_from_expression([_Role=F | R], [F | R1]) :-
	strip_roles_from_expression(R, R1),
	!.
strip_roles_from_expression([unable_to_translate:X | R], [unable_to_translate:X | R1]) :-
	strip_roles_from_expression(R, R1),
	!.
strip_roles_from_expression(Result, Var) :-
	format2error('~N*** Error: bad call: ~w~n', [strip_roles_from_expression(Result, Var)]),
	fail.
 
%----------------------------------------------------------------------

item_matches_transfer_variable_restrictions(( Restrictions1 ; Restrictions2 ), X) :-
	!,
	(   item_matches_transfer_variable_restrictions(Restrictions1, X) ;
	    item_matches_transfer_variable_restrictions(Restrictions2, X)
	).
item_matches_transfer_variable_restrictions(Restrictions, X) :-
	safe_subsumes_chk(Restrictions, X),
	!.
 
%----------------------------------------------------------------------

% The only way a clause body in a rule can contain variables is if it has compiled transfer variables.
clause_body_contains_no_compiled_transfer_variables(Clause) :-
	ground(Clause),
	!.

%----------------------------------------------------------------------

conditions_length(true, 0) :-
	!.
conditions_length((P, Q), Length) :-
	conditions_length(P, Length1),
	conditions_length(Q, Length2),
	Length is Length1 + Length2,
	!.
conditions_length((P ; Q), Length) :-
	conditions_length(P, Length1),
	conditions_length(Q, Length2),
	safe_max_list([Length1, Length2], Length),
	!.
conditions_length((\+ (P)), Length) :-
	conditions_length(P, Length),
	!.
conditions_length(_Other, 1).

all_vars_list([]).
all_vars_list([F | R]) :-
	var(F),
	all_vars_list(R).

single_value_list([], _Value).
single_value_list([F | R], Value) :-
	F = Value,
	nonvar(Value),
	single_value_list(R, Value).

set_uninstantiated_elements_in_list_to_value([], _Value).
set_uninstantiated_elements_in_list_to_value([F | R], Value) :-
	(   var(F) ->
	    F = Value ;
	    true
	),
	!,
	set_uninstantiated_elements_in_list_to_value(R, Value).

%----------------------------------------------------------------------

:- dynamic interlingua_tracing/0.

switch_on_interlingua_tracing :-
	interlingua_tracing,
	!.
switch_on_interlingua_tracing :-
	assertz(interlingua_tracing).

switch_off_interlingua_tracing :-
	retractall(interlingua_tracing).

%----------------------------------------------------------------------

:- dynamic interlingua_structure_debugging/0.

switch_on_interlingua_structure_debugging :-
	interlingua_structure_debugging,
	!.
switch_on_interlingua_structure_debugging :-
	assertz(interlingua_structure_debugging).

switch_off_interlingua_structure_debugging :-
	retractall(interlingua_structure_debugging).

%----------------------------------------------------------------------

:- dynamic transfer_tracing/0.
:- dynamic stored_transfer_trace/2.

switch_on_transfer_tracing :-
	transfer_tracing,
	!.
switch_on_transfer_tracing :-
	assertz(transfer_tracing).

switch_off_transfer_tracing :-
	retractall(transfer_tracing).

print_transfer_trace(RuleType, Trace) :-
	transfer_tracing,
	store_transfer_trace(RuleType, Trace),
	format('~N~nTransfer trace, "~w"~n~n', [RuleType]),
	prettyprint_transfer_trace(Trace),
	format('~N~n', []),
	!.
print_transfer_trace(_RuleType, _Trace).

zero_stored_transfer_trace :-
	retractall(stored_transfer_trace(_, _)).

store_transfer_trace(RuleType, Trace) :-
	asserta(stored_transfer_trace(RuleType, Trace)),
	!.

get_stored_transfer_trace(RuleType, Trace) :-
	stored_transfer_trace(RuleType, Trace).

%----------------------------------------------------------------------

collect_stored_transfer_trace_info_into_alist(Alist) :-
	findall(Key=String,
		stored_transfer_trace_key_and_string(Key, String),
		Alist).

stored_transfer_trace_key_and_string(Key, String) :-
	transfer_trace_rule_type_to_gui_info_key(RuleType, Key),
	stored_transfer_trace(RuleType, Trace),
	with_output_to_chars(prettyprint_transfer_trace(Trace), String).

transfer_trace_rule_type_to_gui_info_key(to_source_discourse, to_source_discourse_trace).
transfer_trace_rule_type_to_gui_info_key(to_interlingua, to_interlingua_trace).
transfer_trace_rule_type_to_gui_info_key(from_interlingua, from_interlingua_trace).

%----------------------------------------------------------------------

remove_list_from_list([], L, L).
remove_list_from_list([F | R], LIn, LOut) :-
	remove_from_list(F, LIn, LNext),
	remove_list_from_list(R, LNext, LOut).

remove_from_list(F, [F | R], R).
remove_from_list(X, [F | R], [F | R1]) :-
	remove_from_list(X, R, R1).

%----------------------------------------------------------------------

check_transfer_rule_context_conditions(true, _Context, CIn-CIn) :-
	!.
check_transfer_rule_context_conditions(context(Item), Context, CIn-COut) :-
	transfer_context_member(Item, Context),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions(context_below(Item), Context, CIn-COut) :-
	(   member([clause, Clause], Context)
	;
	    member(_Role=[clause, Clause], Context)
	),
	check_transfer_rule_context_conditions(context(Item), Clause, CIn-COut),
	!.
check_transfer_rule_context_conditions(context_above(Item), Context, CIn-COut) :-
	transfer_context_member(above(Item), Context),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions(role_context(Item), Context, CIn-COut) :-
	member(role_context(Item), Context),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions(global_context(GlobalContext), _Context, CIn-COut) :-
	check_global_context(GlobalContext),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions(number(N), _Context, CIn-COut) :-
	number_or_compositional_number(N),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions(( \+ P ), Context, CIn-COut) :-
	\+ check_transfer_rule_context_conditions(P, Context, 0-_Count),
	COut is CIn + 1,
	!.
check_transfer_rule_context_conditions((P, Q), Context, CIn-COut) :-
	check_transfer_rule_context_conditions(P, Context, CIn-CNext),
	check_transfer_rule_context_conditions(Q, Context, CNext-COut),
	!.
check_transfer_rule_context_conditions((P ; Q), Context, CIn-COut) :-
	(   check_transfer_rule_context_conditions(P, Context, CIn-COut)
	;
	    check_transfer_rule_context_conditions(Q, Context, CIn-COut)
	),
	!.

%----------------------------------------------------------------------

% We want to avoid matching (Role = Value) against (Role1 = Value1) in the case
% where Role and Value1 are both uninstantiated, or Value and Role1 are both uninstantiated.
% If we allow this, the rules are a lot harder to write.

transfer_context_member(Item, Context) :-
	(   ( Item = (Role = Value), var(Role) ) ->
	    careful_transfer_context_member(Item, Context, value_must_be_instantiated)
	;
	    ( Item = above(Role = Value), var(Role) ) ->
	    careful_transfer_context_member(Item, Context, value_must_be_instantiated)
	;
	    ( Item = (Role = Value), var(Value) ) ->
	    careful_transfer_context_member(Item, Context, role_must_be_instantiated)
	;
	    ( Item = above(Role = Value), var(Value) ) ->
	    careful_transfer_context_member(Item, Context, role_must_be_instantiated)
	;
	    otherwise ->
	    member(Item, Context)
	),
	!.

careful_transfer_context_member((Role = Value), [(Role1 = Value1) | _R], value_must_be_instantiated) :-
	nonvar(Value1),
	(Role = Value) = (Role1 = Value1),
	!.
careful_transfer_context_member(above(Role = Value), [above(Role1 = Value1) | _R], value_must_be_instantiated) :-
	nonvar(Value1),
	(Role = Value) = (Role1 = Value1),
	!.
careful_transfer_context_member((Role = Value), [(Role1 = Value1) | _R], role_must_be_instantiated) :-
	nonvar(Role1),
	(Role = Value) = (Role1 = Value1),
	!.
careful_transfer_context_member(above(Role = Value), [above(Role1 = Value1) | _R], role_must_be_instantiated) :-
	nonvar(Role1),
	(Role = Value) = (Role1 = Value1),
	!.
careful_transfer_context_member(Item, [_F | R], Constraint) :-
	!,
	careful_transfer_context_member(Item, R, Constraint).

%----------------------------------------------------------------------

check_global_context(GlobalContext) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(global_context, GlobalContext),
	!.

%----------------------------------------------------------------------

make_context_conditions_canonical(Conjunction, Representation, Result) :-
	expand_conjunction(Conjunction, ExpandedConjunction),
	make_context_conditions_canonical(ExpandedConjunction, Representation, Result),
	!.
make_context_conditions_canonical(Disjunction, Representation, Result) :-
	expand_disjunction(Disjunction, ExpandedDisjunction),
	make_context_conditions_canonical(ExpandedDisjunction, Representation, Result),
	!.
make_context_conditions_canonical(not(P), Representation, Result) :-
	make_context_conditions_canonical((\+ P), Representation, Result),
	!.
make_context_conditions_canonical((P, Q), Representation, (P1, Q1)) :-
	make_context_conditions_canonical(P, Representation, P1),
	make_context_conditions_canonical(Q, Representation, Q1),
	!.
make_context_conditions_canonical((P ; Q), Representation, (P1 ; Q1)) :-
	make_context_conditions_canonical(P, Representation, P1),
	make_context_conditions_canonical(Q, Representation, Q1),
	!.
make_context_conditions_canonical((\+ P), Representation, (\+ P1)) :-
	make_context_conditions_canonical(P, Representation, P1),
	!.
make_context_conditions_canonical(context(Role=Var), role_marked_linear, context(Role=Var)) :-
	var(Var),
	!.
make_context_conditions_canonical(context(Role=[Type, Arg]), role_marked_linear, context(Role=[Type, Arg])) :-
	!.
make_context_conditions_canonical(context([conj, ConjType | ConjsIn]), role_marked_linear, context([conj, ConjType | ConjsOut])) :-
	make_context_conditions_canonical_list(ConjsIn, role_marked_linear, ConjsOut),
	!.
make_context_conditions_canonical(context(Role=[clause, ClauseIn]), role_marked_linear, context(Role=[clause, ClauseOut])) :-
	add_uninstantiated_roles_to_list(ClauseIn, ClauseNext),
	make_transfer_representation_canonical(ClauseNext, ClauseOut),
	!.
make_context_conditions_canonical(context([clause, ClauseIn]), role_marked_linear, context(_Role=[clause, ClauseOut])) :-
	add_uninstantiated_roles_to_list(ClauseIn, ClauseNext),
	make_transfer_representation_canonical(ClauseNext, ClauseOut),
	!.
make_context_conditions_canonical(context([clause, ClauseIn]), _Representation, context([clause, ClauseOut])) :-
	make_transfer_representation_canonical(ClauseIn, ClauseOut),
	!.
make_context_conditions_canonical(context(Other), role_marked_linear, context(_Role=Other)) :-
	!.
make_context_conditions_canonical(context(Other), _Representation, context(Other)) :-
	!.
make_context_conditions_canonical(role_context([clause, ClauseIn]), role_marked_linear, role_context([clause, ClauseOut])) :-
	add_uninstantiated_roles_to_list(ClauseIn, ClauseNext),
	make_transfer_representation_canonical(ClauseNext, ClauseOut),
	!.
make_context_conditions_canonical(role_context([Type, Arg]), _Representation, role_context([Type, Arg])) :-
	!.
make_context_conditions_canonical(context_above(Role=Var), role_marked_linear, context_above(Role=Var)) :-
	var(Var),
	!.
make_context_conditions_canonical(context_above(Role=[Type, Arg]), role_marked_linear, context_above(Role=[Type, Arg])) :-
	!.
make_context_conditions_canonical(context_above(Other), role_marked_linear, context_above(_Role=Other)) :-
	!.
make_context_conditions_canonical(context_above(Other), _Representation, context_above(Other)) :-
 	!.
make_context_conditions_canonical(context_below(Role=Var), role_marked_linear, context_below(Role=Var)) :-
	var(Var),
	!.
make_context_conditions_canonical(context_below(Role=[Type, Arg]), role_marked_linear, context_below(Role=[Type, Arg])) :-
	!.
make_context_conditions_canonical(context_below(Other), role_marked_linear, context_below(_Role=Other)) :-
	!.
make_context_conditions_canonical(context_below(Other), _Representation, context_below(Other)) :-
 	!.
make_context_conditions_canonical(global_context(Other), _Representation, global_context(Other)) :-
	!.
make_context_conditions_canonical(number(N), _Representation, number(N)) :-
	!.
make_context_conditions_canonical(X, Representation, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_context_conditions_canonical(X, Representation, Y)]),
	fail,
	!.

make_context_conditions_canonical_list([], role_marked_linear, []).
make_context_conditions_canonical_list([F | R], role_marked_linear, [F1 | R1]) :-
	make_context_conditions_canonical(F, role_marked_linear, F1),
	!,
	make_context_conditions_canonical_list(R, role_marked_linear, R1).

add_uninstantiated_roles_to_list(Var, Var) :-
	var(Var),
	!.
add_uninstantiated_roles_to_list([], []) :-
	!.
add_uninstantiated_roles_to_list([F | R], [F1 | R1]) :-
	add_uninstantiated_role_to_item(F, F1),
	!,
	add_uninstantiated_roles_to_list(R, R1).

add_uninstantiated_role_to_item(Key=Value, Key=Value) :-
	!.
add_uninstantiated_role_to_item(Value, _Key=Value).

%----------------------------------------------------------------------

expand_conjunction(Conjunction, ExpandedConjunction) :-
	compound(Conjunction),
	Conjunction =.. [and | Args],
	Args \== [],
	expand_conjunction1(Args, ExpandedConjunction),
	!.

expand_conjunction1([Arg], Arg) :-
	!.
expand_conjunction1([Arg | Args], (Arg, ExpandedArgs)) :-
	expand_conjunction1(Args, ExpandedArgs).

expand_disjunction(Disjunction, ExpandedDisjunction) :-
	compound(Disjunction),
	Disjunction =.. [or | Args],
	Args \== [],
	expand_disjunction1(Args, ExpandedDisjunction),
	!.

expand_disjunction1([Arg], Arg) :-
	!.
expand_disjunction1([Arg | Args], (Arg ; ExpandedArgs)) :-
	expand_disjunction1(Args, ExpandedArgs).

%----------------------------------------------------------------------

:- dynamic undefined_interlingua_elements_allowed/0.

allow_undefined_interlingua_elements :-
	undefined_interlingua_elements_allowed,
	!.
allow_undefined_interlingua_elements :-
	assertz(undefined_interlingua_elements_allowed).

dont_allow_undefined_interlingua_elements :-
	retractall(undefined_interlingua_elements_allowed).

%----------------------------------------------------------------------

find_unknown_elements_in_interlingua_form(Representation, BadElements) :-
	check_interlingua_representation1(Representation, BadElements0-[], just_checking, no_line_info),
	sort(BadElements0, BadElements),
	!.
find_unknown_elements_in_interlingua_form(Representation, BadElements) :-
	format('~N*** Warning: bad call: ~w~n', [find_unknown_elements_in_interlingua_form(Representation, BadElements)]),
	(   var(BadElements) ->
	    BadElements = []
	;
	    otherwise ->
	    true
	).

%----------------------------------------------------------------------

check_interlingua_representation(Representation, ToOrFromInterlingua, LineInfo) :-
	check_interlingua_representation1(Representation, BadElements-[], ToOrFromInterlingua, LineInfo),
	!,
	(   BadElements = [] ->
	    true
	;
	    undefined_interlingua_elements_allowed ->
	    true
	;
	    BadElements = [BadElement] ->
	    format2error('~N*** Error: element not declared in interlingua: ~w~n', [BadElement]),
	    fail
	;
	    format2error('~N*** Error: elements not declared in interlingua: ~w~n', [BadElements]),
	    fail
	).
check_interlingua_representation(Representation, ToOrFromInterlingua, LineInfo) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [check_interlingua_representation(Representation, ToOrFromInterlingua, LineInfo)]),
	fail.
 
check_interlingua_representation1([], BadIn-BadIn, _ToOrFrom, _LineInfo).
check_interlingua_representation1([F | R], BadIn-BadOut, ToOrFrom, LineInfo) :-
	check_interlingua_representation2(F, BadIn-BadNext, ToOrFrom, LineInfo),
	!,
	check_interlingua_representation1(R, BadNext-BadOut, ToOrFrom, LineInfo).

check_interlingua_representation2([conj, _ConjType | Conjs], BadIn-BadOut, ToOrFrom, LineInfo) :-
	check_interlingua_representation1_list(Conjs, BadIn-BadOut, ToOrFrom, LineInfo),
	!.
check_interlingua_representation2([clause, Clause], BadIn-BadOut, ToOrFrom, LineInfo) :-
	check_interlingua_representation1(Clause, BadIn-BadOut, ToOrFrom, LineInfo),
	!.
check_interlingua_representation2(_Role=Item, BadIn-BadOut, ToOrFrom, LineInfo) :-
	check_interlingua_representation2(Item, BadIn-BadOut, ToOrFrom, LineInfo),
	!.
check_interlingua_representation2(TransferElement, BadIn-BadIn, _ToOrFrom, _LineInfo) :-
	compound(TransferElement),
	functor(TransferElement, tr, _),
	!.
check_interlingua_representation2([Key, Value], BadIn-BadIn, ToOrFrom, LineInfo) :-
	interlingua_constant_or_deprecated_interlingua_constant([Key, Value], LineInfo, ToOrFrom),
	mark_interlingua_constant_as_used([Key, Value], ToOrFrom),
	!.
check_interlingua_representation2(Other, [Other | BadOut]-BadOut, _ToOrFrom, _LineInfo) :-
	!.
check_interlingua_representation2(F, Bad, ToOrFrom, LineInfo) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [check_interlingua_representation2(F, Bad, ToOrFrom, LineInfo)]),
	fail.

check_interlingua_representation1_list([], BadIn-BadIn, _ToOrFrom, _LineInfo).
check_interlingua_representation1_list([F | R], BadIn-BadOut, ToOrFrom, LineInfo) :-
	check_interlingua_representation1(F, BadIn-BadNext, ToOrFrom, LineInfo),
	!,
	check_interlingua_representation1_list(R, BadNext-BadOut, ToOrFrom, LineInfo).

interlingua_constant_or_deprecated_interlingua_constant(Constant, LineInfo, ToOrFrom) :-
	current_predicate(user:deprecated_interlingua_constant/1),
	ground(Constant),
	user:deprecated_interlingua_constant(Constant),
	(   ToOrFrom = to_interlingua ->
	    
	    format2error('~N*** Warning: deprecated interlingua constant: ~w ', [Constant]),
	    inform_about_line_info(LineInfo),
	    format2error('~N~n', []) ;

	    true
	),
	!.
interlingua_constant_or_deprecated_interlingua_constant(Constant, LineInfo, ToOrFrom) :-
	current_predicate(user:deprecated_interlingua_constant/1),
	\+ ground(Constant),
	copy_term(Constant, Constant1),
	user:deprecated_interlingua_constant(Constant1),
	identical_up_to_variable_renaming(Constant, Constant1),
	(   ToOrFrom = to_interlingua ->
	    
	    format2error('~N*** Warning: deprecated interlingua constant: ~w ', [Constant]),
	    inform_about_line_info(LineInfo),
	    format2error('~N~n', []) ;

	    true
	),
	!.
interlingua_constant_or_deprecated_interlingua_constant(Constant, _LineInfo, _ToOrFrom) :-
	current_predicate(user:interlingua_constant/1),
	ground(Constant),
	user:interlingua_constant(Constant),
	!.
interlingua_constant_or_deprecated_interlingua_constant(Constant, _LineInfo, _ToOrFrom) :-
	current_predicate(user:interlingua_constant/1),
	\+ ground(Constant),
	copy_term(Constant, Constant1),
	user:interlingua_constant(Constant),
	identical_up_to_variable_renaming(Constant, Constant1),
	!.
interlingua_constant_or_deprecated_interlingua_constant(Constant, _LineInfo, _ToOrFrom) :-
	current_predicate(Module:sem_element/1),
	ground(Constant),
	Module:sem_element(Constant),
	!.

%----------------------------------------------------------------------

:- dynamic interlingua_constant_used/2.

zero_interlingua_constant_used_table :-
	retractall(interlingua_constant_used(_, _)),
	!.

mark_interlingua_constant_as_used(Const, ToOrFromInterlingua) :-
	ground(Const),
	interlingua_constant_used(Const, ToOrFromInterlingua),
	!.
mark_interlingua_constant_as_used(NonConstTerm, ToOrFromInterlingua) :-
	copy_term(NonConstTerm, NonConstTerm1),
	interlingua_constant_used(NonConstTerm1, ToOrFromInterlingua),
	identical_up_to_variable_renaming(NonConstTerm, NonConstTerm1),
	!.
mark_interlingua_constant_as_used(Const, ToOrFromInterlingua) :-
	assertz(interlingua_constant_used(Const, ToOrFromInterlingua)),
	!.

interlingua_constant_has_been_used(Const, ToOrFromInterlingua) :-
	ground(Const),
	interlingua_constant_used(Const, ToOrFromInterlingua),
	!.
interlingua_constant_has_been_used(NonConstTerm, ToOrFromInterlingua) :-
	\+ ground(NonConstTerm),
	copy_term(NonConstTerm, NonConstTerm1),
	interlingua_constant_used(NonConstTerm1, ToOrFromInterlingua),
	identical_up_to_variable_renaming(NonConstTerm, NonConstTerm1),
	!.

%----------------------------------------------------------------------

recognise_parse_transfer_and_generate_file(
	NewOrStoredRecognitionResults, _TranscriptionsFile, _RecParams, _OutFile, _WavfilesFile, _BatchrecTraceFile, 
	_PrologBatchrecFile, _PrologBatchrecFileWithTranscriptions, _TmpDirectory, _TranslationMode, _Command) :-
	\+ member(NewOrStoredRecognitionResults, [new_recognition_results, stored_recognition_results]),
	!,
	format2error('~N*** Error: bad call to recognise_parse_transfer_and_generate_file/10. First arg was ~w, must be "new_recognition_results" or "stored_recognition_results"~n', [NewOrStoredRecognitionResults]),
	fail.
recognise_parse_transfer_and_generate_file(
	NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, OutFile, WavfilesFile, BatchrecTraceFile, 
	PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, TmpDirectory, TranslationMode, Command) :-

	timed_call(recognise_parse_transfer_and_generate_file1(NewOrStoredRecognitionResults, TranscriptionsFile, RecParams, OutFile,
							       WavfilesFile, BatchrecTraceFile, 
							       PrologBatchrecFile, PrologBatchrecFileWithTranscriptions,
							       TmpDirectory, TranslationMode, Command),
		   TimeTaken),
	make_csv_version_of_translation_file(OutFile),
	format('~NProcessing finished, ~2f secs~n~n', [TimeTaken]).

recognise_parse_transfer_and_generate_file1(
	new_recognition_results, TranscriptionsFile, RecParams, OutFile, WavfilesFile, BatchrecTraceFile, 
	PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, TmpDirectory, TranslationMode, Command) :-

	safe_absolute_file_name(TranscriptionsFile, AbsTranscriptionsFile),
	format('~N# Doing batch recognition and translation on: ~w~n', [AbsTranscriptionsFile]),
	
	transcriptions_file_to_wavfiles_file(AbsTranscriptionsFile, WavfilesFile),

	set_new_current_progress_file_for_command(Command, 1),
	do_batchrec(WavfilesFile, AbsTranscriptionsFile, RecParams, BatchrecTraceFile, PrologBatchrecFile, TmpDirectory),
	add_transcriptions_to_prolog_batchrec_file(PrologBatchrecFile, TranscriptionsFile, PrologBatchrecFileWithTranscriptions),
	prolog_file_to_list(PrologBatchrecFileWithTranscriptions, PrologBatchrecListWithTranscriptions),

	set_new_current_progress_file_for_command(Command, 2),
	parse_transfer_and_generate_items(PrologBatchrecListWithTranscriptions, OutFile, TranslationMode).
recognise_parse_transfer_and_generate_file1(
	stored_recognition_results, _TranscriptionsFile, _RecParams, OutFile, _WavfilesFile, _BatchrecTraceFile, 
	_PrologBatchrecFile, PrologBatchrecFileWithTranscriptions, _TmpDirectory, TranslationMode, Command) :-

	safe_absolute_file_name(PrologBatchrecFileWithTranscriptions, AbsPrologBatchrecFileWithTranscriptions),
	format('~N--- Reading stored recognition results from ~w~n', [AbsPrologBatchrecFileWithTranscriptions]),

	set_new_current_progress_file_for_command(Command, 1),
	prolog_file_to_list(AbsPrologBatchrecFileWithTranscriptions, PrologBatchrecListWithTranscriptions),
	parse_transfer_and_generate_items(PrologBatchrecListWithTranscriptions, OutFile, TranslationMode).

%----------------------------------------------------------------------

parse_transfer_and_generate_file(File, Mode, TranslationMode) :-
	parse_transfer_and_generate_file(File, Mode, user, TranslationMode).

parse_transfer_and_generate_file(File, Mode, OutFile, TranslationMode) :-
	timed_call(parse_transfer_and_generate_file1(File, Mode, OutFile, TranslationMode),
		   TimeTaken),
	make_csv_version_of_translation_file(OutFile),
	format('~NProcessing finished, ~2f secs~n~n', [TimeTaken]).
 
parse_transfer_and_generate_file1(File, Mode, OutFile, TranslationMode) :-	
	safe_absolute_file_name(File, File1),
	format('~N~nProcessing file(s) ~w~n~n', [File1]),
	read_corpus_file(Mode, File1, Items),
	parse_transfer_and_generate_items(Items, OutFile, TranslationMode).

read_corpus_file(text, File, Items) :-
	read_file_to_atom_list(File, Items0),
	findall(sent(Atom), ( member(Atom, Items0) ), Items).
read_corpus_file(prolog, File, Items) :-
	prolog_file_or_files_to_list(File, Items).

%----------------------------------------------------------------------

parse_transfer_and_generate_items(Items, OutFile, TranslationMode) :-
	(   OutFile = user ->
	    S = user ;
	    open_regulus_file(OutFile, write, S)
	),

	empty_assoc_generic(ResultsIn),
	null_discourse_context(ContextIn),
	init_stored_nbest_translation_summary,

	zero_nbest_training_data_file_if_necessary(NBestFile),
	(   NBestFile \== no_file ->
	    format('~N~n--- Writing out N-best training data to: ~w~n~n', [NBestFile])
	;
	    true
	),

	parse_transfer_and_generate_to_stream(Items, S, ContextIn-_ContextOut,
					      ResultsIn-ResultsOut, TranslationMode, 0-_Count),

	print_stored_nbest_translation_summary(S),

	(   OutFile = user ->
	    true ;
	    close(S)
	),
	
	get_assoc_generic_or_zero(processed, ResultsOut, NProcessed),
	get_assoc_generic_or_zero(bad_recognition, ResultsOut, NBadRecognition),
	get_assoc_generic_or_zero(good, ResultsOut, NGood),
	get_assoc_generic_or_zero(ok, ResultsOut, NOK),
	get_assoc_generic_or_zero(bad, ResultsOut, NBad),
	get_assoc_generic_or_zero(unknown, ResultsOut, NUnknown),
	get_assoc_generic_or_zero(no_result, ResultsOut, NNoResult),
	get_assoc_generic_or_zero(bad_interlingua, ResultsOut, NBadInterlingua),

	get_assoc_generic_or_zero(new_bad, ResultsOut, NNewBad),
	get_assoc_generic_or_zero(new_unknown, ResultsOut, NNewUnknown),
	get_assoc_generic_or_zero(new_no_result, ResultsOut, NNewNoResult),

	get_assoc_generic_or_zero(source_words, ResultsOut, NSourceWords0),
	get_assoc_generic_or_zero(word_errors, ResultsOut, NWordErrors),
	get_assoc_generic_or_zero(oracle_word_errors, ResultsOut, NOracleWordErrors),

	get_assoc_generic_or_zero(sem_recognition_measured, ResultsOut, NSemRecognitionMeasured0),
	get_assoc_generic_or_zero(sem_recognition_ok, ResultsOut, NSemRecognitionOK),
	get_assoc_generic_or_zero(sem_recognition_maybe_ok, ResultsOut, NSemRecognitionMaybeOK),
	get_assoc_generic_or_zero(oracle_sem_recognition_ok, ResultsOut, NOracleSemRecognitionOK),
	get_assoc_generic_or_zero(oracle_sem_recognition_maybe_ok, ResultsOut, NOracleSemRecognitionMaybeOK),

	get_assoc_generic_or_zero(total_generations, ResultsOut, TotalGeneratedResults),	

	%NAll0 is NProcessed + NBadRecognition,
	NAll0 is NProcessed,
	(   NAll0 = 0 ->
	    NAll = 1
	;
	    NAll = NAll0
	),
	(   NSemRecognitionMeasured0 = 0 ->
	    NSemRecognitionMeasured = 1
	;
	    NSemRecognitionMeasured = NSemRecognitionMeasured0
	),
	(   NSourceWords0 = 0 ->
	    NSourceWords = 1
	;
	    NSourceWords = NSourceWords0
	),
	
	PCBadRecognition is 100 * NBadRecognition / NAll,
	PCGood is 100 * NGood / NAll,
	PCOK is 100 * NOK / NAll,
	PCBad is 100 * NBad / NAll,
	PCUnknown is 100 * NUnknown / NAll,
	PCNoResult is 100 * NNoResult / NAll,
	PCBadInterlingua is 100 * NBadInterlingua / NAll,
	PCProcessed is 100 * NProcessed / NAll,

	PCNewBad is 100 * NNewBad / NAll,
	PCNewUnknown is 100 * NNewUnknown / NAll,
	PCNewNoResult is 100 * NNewNoResult / NAll,

	PCSemRecognitionOK is 100 * NSemRecognitionOK / NSemRecognitionMeasured,
	PCSemRecognitionMaybeOK is 100 * NSemRecognitionMaybeOK / NSemRecognitionMeasured,

	PCOracleSemRecognitionOK is 100 * NOracleSemRecognitionOK / NSemRecognitionMeasured,
	PCOracleSemRecognitionMaybeOK is 100 * NOracleSemRecognitionMaybeOK / NSemRecognitionMeasured,
	
	WER is 100 * NWordErrors / NSourceWords,
	OracleWER is 100 * NOracleWordErrors / NSourceWords,

	NProducedResult is NAll - NNoResult,
	(   NProducedResult > 0 ->
	    AvNGeneratedResults is TotalGeneratedResults / NProducedResult
	;
	    AvNGeneratedResults = 0.0
	),

	format('~N#~n', []),
	format('~N#  Misrecognised: ~d (~1f%)~n', [NBadRecognition, PCBadRecognition]),
	format('~N#           Good: ~d (~1f%)~n', [NGood, PCGood]),
	format('~N#             OK: ~d (~1f%)~n', [NOK, PCOK]),
	format('~N#            Bad: ~d (~1f%)~n', [NBad, PCBad]),
	format('~N#        Unknown: ~d (~1f%)~n', [NUnknown, PCUnknown]),
	format('~N#      No result: ~d (~1f%)~n', [NNoResult, PCNoResult]),
	(   \+ only_translate_up_to_interlingua ->
	    format('~N#Bad interlingua: ~d (~1f%)~n', [NBadInterlingua, PCBadInterlingua])
	;
	    true
	),
	format('~N##############################~n', []),
	format('~N#      (New bad): ~d (~1f%)~n', [NNewBad, PCNewBad]),
	format('~N#  (New unknown): ~d (~1f%)~n', [NNewUnknown, PCNewUnknown]),
	format('~N#(New no result): ~d (~1f%)~n', [NNewNoResult, PCNewNoResult]),
	format('~N##############################~n', []),
	(   NSourceWords > 0 ->
	    format('~N#   Transcribed words in corpus: ~d~n', [NSourceWords]),
	    format('~N#   Word errors: ~d (WER = ~2f%)~n', [NWordErrors, WER]),
	    (   NOracleWordErrors > 0 ->
		format('~N#   Word errors (oracle): ~d (WER = ~2f%)~n', [NOracleWordErrors, OracleWER])
	    ;
		true
	    ),
	    format('~N##############################~n', [])
	;
	    true
	),
	(   ( NSemRecognitionOK > 0 ; NSemRecognitionMaybeOK > 0 ) ->
	    format('~N#   Sem Rec clearly OK: ~d (~1f%)~n', [NSemRecognitionOK, PCSemRecognitionOK]),
	    format('~N#   Sem Rec   maybe OK: ~d (~1f%)~n', [NSemRecognitionMaybeOK, PCSemRecognitionMaybeOK]),
	    (   ( NOracleSemRecognitionOK > 0 ; NOracleSemRecognitionMaybeOK > 0 ) ->
		format('~N#   Sem Rec clearly OK (oracle): ~d (~1f%)~n', [NOracleSemRecognitionOK, PCOracleSemRecognitionOK]),
		format('~N#   Sem Rec   maybe OK (oracle): ~d (~1f%)~n', [NOracleSemRecognitionMaybeOK, PCOracleSemRecognitionMaybeOK])
	    ;
		true
	    ),
	    (   ( NSemRecognitionMaybeOK > 0 ; NOracleSemRecognitionMaybeOK > 0 ) ->
		format('~N#   ("maybe OK" = "interlingua not well-formed, but same as that for reference")~n', [])
	    ;
		true
	    ),
	    format('~N##############################~n', [])
	;
	    true
	),
	format('~N#    Processed: ~d (~1f%)~n', [NProcessed, PCProcessed]),
	format('~N#~n', []),
	format('~N#Average number of generated variants: ~2f~n', [AvNGeneratedResults]),
	format('~N#~n', []),

	store_translate_corpus_summary([bad_recognition=PCBadRecognition,
					good=PCGood,
					ok=PCOK,
					bad=PCBad,
					unknown=PCUnknown,
					no_result=PCNoResult,
					sem_rec_ok=PCSemRecognitionOK]).				      

parse_transfer_and_generate_to_stream([], _S, ContextIn-ContextIn, ResultsIn-ResultsIn, _TranslationMode,
				      Count-Count).
parse_transfer_and_generate_to_stream([F | R], S, ContextIn-ContextOut, ResultsIn-ResultsOut,
				      TranslationMode, CountIn-CountOut) :-
	format(S, '~N~n%---------------------------------------', []),
	parse_transfer_and_generate_to_stream1(F, S, ContextIn-ContextNext, ResultsIn-ResultsNext, TranslationMode),
	CountNext is CountIn + 1,
	format(user, '.', []),
	(   ( 0 is CountNext mod 100, CountNext > 0 ) ->
	    
	    format(' (~d) ~n', [CountNext]),
	    flush_output(user) ;
	    
	    true
	),
	flush_output(user),
	add_progress_line(processed_translation_record),
	!,
	parse_transfer_and_generate_to_stream(R, S, ContextNext-ContextOut, ResultsNext-ResultsOut,
					      TranslationMode, CountNext-CountOut).

%----------------------------------------------------------------------

:- dynamic stored_translate_corpus_summary/3.
:- dynamic current_translate_corpus_id/1.

zero_stored_translate_corpus_summary :-
	retractall(stored_translate_corpus_summary(_, _, _)).

set_stored_translate_corpus_summary_id(Id) :-
	retractall(current_translate_corpus_id(_)),
	asserta(current_translate_corpus_id(Id)).

get_stored_translate_corpus_summary_id(Id) :-
	current_translate_corpus_id(Id),
	!.
get_stored_translate_corpus_summary_id(default).

store_translate_corpus_summary(Results) :-
	get_stored_translate_corpus_summary_id(Id),
	store_translate_corpus_summary1(Results, Id),
	!.
store_translate_corpus_summary(Results) :-
	format2error('~N*** Error: bad call: ~w~n', [store_translate_corpus_summary(Results)]),
	fail.

store_translate_corpus_summary1([], _Id).
store_translate_corpus_summary1([F | R], Id) :-
	store_translate_corpus_summary_item(F, Id),
	!,
	store_translate_corpus_summary1(R, Id).

store_translate_corpus_summary_item(Key=Value, Id) :-
	assertz(stored_translate_corpus_summary(Id, Key, Value)).

get_stored_translate_corpus_summary(Id, Results) :-
	findall(Key=Value, stored_translate_corpus_summary(Id, Key, Value), Results).

%----------------------------------------------------------------------

parse_transfer_and_generate_to_stream1(sent(Source), S, ContextIn-ContextOut, ResultsIn-ResultsOut, TranslationMode) :-
	safe_parse_transfer_and_generate(Source, Target, ContextIn-ContextOut, Stats, TranslationMode),

	(   ellipsis_processing_is_activated ->
	    
	    get_preceding_target_utterance(PrecedingUtt, ContextIn),
	    SourceInContext = Source+PrecedingUtt ;

	    SourceInContext = Source
	),

	parse_transfer_and_generate_to_stream2(SourceInContext, text_mode, text, Target, Stats, S, ResultsIn-ResultsOut),
	!.

parse_transfer_and_generate_to_stream1(interlingua_item(Source, Interlingua, _FromLanguagesInfo), S,
				       ContextIn-ContextOut, ResultsIn-ResultsOut, _TranslationMode) :-
	safe_transfer_from_interlingua_and_generate(Interlingua, Target, ContextIn-ContextOut, Stats),
	FullStats = [interlingua=Interlingua | Stats],
	parse_transfer_and_generate_to_stream2(Source, text_mode, text, Target, FullStats, S, ResultsIn-ResultsOut),
	!.

parse_transfer_and_generate_to_stream1(rec_stats(RecStatsList), _S, Context-Context, Results-Results, _TranslationMode) :-
	store_translate_corpus_summary(RecStatsList),
	!.

parse_transfer_and_generate_to_stream1(wavfile(Wavfile), S, ContextIn-ContextOut, ResultsIn-ResultsOut, TranslationMode) :-	
	safe_absolute_file_name(Wavfile, AbsWavfile),
	recognise_from_wavfile_as_defined_by_config_file(AbsWavfile, RecResult),
	(   get_transcription_from_wavfile(AbsWavfile, Transcription) ->
	    true
	;
	    Transcription = '*no_transcription*'
	),
	rec_result_and_transcription_to_batchrec_item(RecResult, AbsWavfile, Transcription, BatchrecItem),
	parse_transfer_and_generate_to_stream1(BatchrecItem, S, ContextIn-ContextOut, ResultsIn-ResultsOut, TranslationMode),
	!.

parse_transfer_and_generate_to_stream1(batchrec_item(BatchrecList), S, ContextIn-ContextOut, ResultsIn-ResultsOut, TranslationMode) :-	
	TranslationMode = interlingua,
	unpack_nbest_batchrec_list(BatchrecList, NBestBatchrecLists),

	notional_batchrec_item_for_transcription_in_nbest_list(BatchrecList, TranscriptionBatchrecList),
	safe_transfer_and_generate_batchrec_item_up_to_interlingua(batchrec_item(TranscriptionBatchrecList),
								   ContextIn-TransContextOut, TranscriptStats),
	nbest_preferences([[record=TranscriptStats, context=TransContextOut]],
			  _ChosenTransRecord,
			  TransTrace,
			  _TransRank),

	findall([record=Stats, context=ContextOut],
		(   empty_assoc_generic(DummyResults),
		    member(SingleBatchrecList, NBestBatchrecLists),
		    member(words=RecognisedWords, SingleBatchrecList),
		    member(transcription=SourceWords, SingleBatchrecList),
		    safe_transfer_and_generate_batchrec_item_up_to_interlingua(batchrec_item(SingleBatchrecList),
									       ContextIn-ContextOut, Stats1),
		    add_semantic_recognition_accuracy_info(Stats1, TranscriptStats, Stats2, DummyResults-ResultsNext),
		    add_word_error_info(Stats2, SourceWords, RecognisedWords, Stats, ResultsNext-_ResultsOut)
		),
		Tuples),
	nbest_preferences(Tuples,
			  [record=_ChosenRecord, context=_ChosenContextOut],
			  Trace,
			  RankOfChosenElement),
		
	append(TransTrace, Trace, FullTrace),
	add_semantic_recognition_accuracy_info_to_nbest_trace(FullTrace, TranscriptStats, FullTraceWithSemInfo),
	print_nbest_trace(FullTraceWithSemInfo, RankOfChosenElement, S),
	
	safe_nth(RankOfChosenElement, NBestBatchrecLists, ChosenBatchrecList),
	ChosenBatchrecList1 = [selected_hyp=RankOfChosenElement | ChosenBatchrecList],

	get_oracle_word_error_info_for_nbest_tuples(Tuples, OracleWordErrors),
	get_oracle_semantics_recognition_accuracy_info_for_nbest_tuples(Tuples, OracleSemRecognition),
	append(ChosenBatchrecList1,
	       [oracle_word_errors=OracleWordErrors,
		oracle_sem_recognition=OracleSemRecognition],
	       ChosenBatchrecList2),
	
	parse_transfer_and_generate_to_stream1(batchrec_item(ChosenBatchrecList2), S,
					       ContextIn-ContextOut, ResultsIn-ResultsOut, TranslationMode),
	!.

parse_transfer_and_generate_to_stream1(batchrec_item(BatchrecList), S, ContextIn0-ContextOut, ResultsIn-ResultsOut, TranslationMode) :-
	member(wavfile=Wavfile, BatchrecList),
	member(words=RecognisedWords, BatchrecList),
	member(transcription=SourceWords, BatchrecList),
	join_with_spaces(RecognisedWords, Recognised),
	join_with_spaces(SourceWords, Source),
	notional_batchrec_item_for_transcription_in_nbest_list(BatchrecList, TranscriptionBatchrecList),

	(   user:preceding_context(Wavfile, PrecedingContextSentence) ->
	    set_context_from_sentence(PrecedingContextSentence, S, ContextIn0-ContextIn)
	;
	    ContextIn0 = ContextIn
	),
	safe_transfer_and_generate_batchrec_item_up_to_interlingua(batchrec_item(TranscriptionBatchrecList),
								   ContextIn-_TransContextOut, TranscriptStats),

	(   ellipsis_processing_is_activated ->
	    get_preceding_target_utterance(PrecedingUtt, ContextIn),
	    SourceInContext = Source+PrecedingUtt
	;
	    SourceInContext = Source
	),

	(   ( bad_recognition(SourceInContext, Recognised) ; ignore_wavfile(Wavfile) ) ->
	    ContextOut = ContextIn,
	    Stats1 = [],
	    Target = no_translation_due_to_bad_recognition
	;
	    safe_parse_transfer_and_generate(Recognised, Target, ContextIn-ContextOut, Stats0, TranslationMode),
	    remove_all_generation_tuples_item(Stats0, Stats1)
	),

	(   ( member(selected_hyp=SelectedRank, BatchrecList), SelectedRank > 1 ) ->
	    Stats2 = [selected_hyp=SelectedRank, non_top_hyp=yes | Stats1]
	;
	    member(selected_hyp=1, BatchrecList) ->
	    Stats2 = [selected_hyp=1 | Stats1]
	;
	    otherwise ->
	    Stats2 = Stats1
	),

	add_semantic_recognition_accuracy_info(Stats2, TranscriptStats, Stats3, ResultsIn-ResultsNext1),
	add_word_error_info(Stats3, SourceWords, RecognisedWords, Stats, ResultsNext1-ResultsNext2),
	add_oracle_semantic_recognition_accuracy_info(BatchrecList, ResultsNext2-ResultsNext3),
	add_oracle_word_error_info(BatchrecList, ResultsNext3-ResultsNext4),

	parse_transfer_and_generate_to_stream2(SourceInContext, Recognised, Wavfile, Target, Stats, S, ResultsNext4-ResultsOut),
	!.

parse_transfer_and_generate_to_stream1(set_discourse_context(Source, Target, TargetRepresentation),
				       S, ContextIn-ContextOut, ResultsIn-ResultsOut, _TranslationMode) :-
	transfer_representation_to_source_discourse(TargetRepresentation,
						    SourceDiscourseRepresentation),
	set_preceding_target_utterance(Source, ContextIn, ContextNext),
	set_preceding_source_discourse(SourceDiscourseRepresentation, ContextNext, ContextOut),
	ResultsIn = ResultsOut,
	format(S, '~N%Question being used to set context: "~w"', [Source]),
	format(S, '~N%                     Translated as: "~w"', [Target]),
	format(S, '~N%             New discourse context: ~q', [SourceDiscourseRepresentation]),
	!.

parse_transfer_and_generate_to_stream1(Item, S, Context, Results, TranslationMode) :-
	format2error('~N*** Error: bad call: ~q',
		     [parse_transfer_and_generate_to_stream1(Item, S, Context, Results, TranslationMode)]),
	fail.

%---------------------------------------------------------------

add_word_error_info(StatsIn, SourceWords, RecognisedWords, StatsOut, ResultsIn-ResultsOut) :-
	length(SourceWords, NSourceWords),
	insertions_deletions_substitutions(RecognisedWords, SourceWords, Total, Insertions, Deletions, Substitutions),
	StatsOut = [word_errors=[total=Total, ins=Insertions, del=Deletions, sub=Substitutions] | StatsIn],
	inc_assoc_generic(ResultsIn, source_words, NSourceWords, ResultsNext),
	inc_assoc_generic(ResultsNext, word_errors, Total, ResultsOut),
	!.
add_word_error_info(StatsIn, SourceWords, RecognisedWords, StatsOut, ResultsIn-ResultsOut) :-
	format2error('~N*** Error: bad call: ~q',
		     [add_word_error_info(StatsIn, SourceWords, RecognisedWords, StatsOut, ResultsIn-ResultsOut)]),
	fail.

%---------------------------------------------------------------

add_oracle_word_error_info(BatchrecList, ResultsIn-ResultsOut) :-
	(   member(oracle_word_errors=WordErrors, BatchrecList) ->
	    member(total=Total, WordErrors),
	    inc_assoc_generic(ResultsIn, oracle_word_errors, Total, ResultsOut)
	;
	    otherwise ->
	    ResultsIn = ResultsOut
	),
	!.

%---------------------------------------------------------------

% Getting oracle info from the N-best list

% We want the lowest total word error
get_oracle_word_error_info_for_nbest_tuples(Tuples, OracleWordErrors) :-
	findall(Total-WordErrors,
		(   member(Tuple, Tuples),
		    member(record=Stats, Tuple),
		    member(word_errors=WordErrors, Stats),
		    member(total=Total, WordErrors)
		),
		KeyList),
	keysort(KeyList, SortedKeyList),
	SortedKeyList = [_OracleTotal-OracleWordErrors | _],
	!.

% We want the best semantic accuracy
get_oracle_semantics_recognition_accuracy_info_for_nbest_tuples(Tuples, OracleSemRecognition) :-
	findall(SemRecognition,
		(   member(Tuple, Tuples),
		    member(record=Stats, Tuple),
		    member(sem_recognition=SemRecognition, Stats)
		),
		SemRecognitionList),
	(   member(good, SemRecognitionList) ->
	    OracleSemRecognition = good
	;
	    member(unclear, SemRecognitionList) ->
	    OracleSemRecognition = unclear
	;
	    OracleSemRecognition = bad
	).

%---------------------------------------------------------------

add_semantic_recognition_accuracy_info_to_nbest_trace([], _TransStats, []).
add_semantic_recognition_accuracy_info_to_nbest_trace([F | R], TransStats, [F1 | R1]) :-
	add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransStats, F1),
	!,
	add_semantic_recognition_accuracy_info_to_nbest_trace(R, TransStats, R1).

add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransStats, F1) :-
	F = Score-[[record=Stats | Rest], PrefTrace],
	F1 = Score-[[record=Stats1 | Rest], PrefTrace],
	translation_records_semantically_equivalent(Stats, TransStats, SemRecognition),
	add_sem_recognition_item_to_stats(Stats, SemRecognition, Stats1),
	!.
add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransStats, F1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_semantic_recognition_accuracy_info_to_nbest_trace_item(F, TransStats, F1)]),
	fail.

%----------------------------------------------------------------------

add_semantic_recognition_accuracy_info(StatsIn, TranscriptStats, StatsOut, ResultsIn-ResultsOut) :-
	inc_assoc_generic(ResultsIn, sem_recognition_measured, ResultsNext),
	translation_records_semantically_equivalent(StatsIn, TranscriptStats, SemRecognition),
	(   SemRecognition = good ->
	    inc_assoc_generic(ResultsNext, sem_recognition_ok, ResultsOut)
	;
	    SemRecognition = unclear ->
	    inc_assoc_generic(ResultsNext, sem_recognition_maybe_ok, ResultsOut)
	;
	    ResultsOut = ResultsNext
	),
	add_sem_recognition_item_to_stats(StatsIn, SemRecognition, StatsOut),
	!.

% The representation is well-formed, and the gloss is the same as that for the transcription.
% Clearly good.
translation_records_semantically_equivalent(Stats, TranscriptStats, good) :-
	member(interlingua_surface=Int, Stats),
	member(interlingua_surface=Int, TranscriptStats),
	\+ bad_interlingua_surface(Int),
	!.
% The representation is not well-formed, but it is the same as that for the reference.
% Arguably good.
translation_records_semantically_equivalent(Stats, TranscriptStats, unclear) :-
	member(interlingua=Int, Stats),
	member(interlingua=Int, TranscriptStats),
	!.
% Anything else. Bad.
translation_records_semantically_equivalent(_Stats, _TranscriptStats, bad) :-
	!.

add_sem_recognition_item_to_stats([],
				  SemRecognition,
				  [sem_recognition=SemRecognition]) :-
	!.
add_sem_recognition_item_to_stats([interlingua=Int | R],
				  SemRecognition,
				  [interlingua=Int, sem_recognition=SemRecognition | R]) :-
	!.
add_sem_recognition_item_to_stats([F | R], SemRecognition, [F | R1]) :-
	!,
	add_sem_recognition_item_to_stats(R, SemRecognition, R1).
	
%---------------------------------------------------------------

add_oracle_semantic_recognition_accuracy_info(BatchrecList, ResultsIn-ResultsOut) :-
	(   member(oracle_sem_recognition=OracleSemRecognition, BatchrecList) ->
	    (   OracleSemRecognition = good ->
		inc_assoc_generic(ResultsIn, oracle_sem_recognition_ok, ResultsOut)
	    ;
		OracleSemRecognition = unclear ->
		inc_assoc_generic(ResultsIn, oracle_sem_recognition_maybe_ok, ResultsOut)
	    ;
		ResultsIn = ResultsOut
	    )
	;
	    otherwise ->
	    ResultsIn = ResultsOut
	),
	!.
add_oracle_semantic_recognition_accuracy_info(BatchrecList, Results) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_oracle_semantic_recognition_accuracy_info(BatchrecList, Results)]),
	fail.

%----------------------------------------------------------------------

safe_transfer_and_generate_batchrec_item_up_to_interlingua(batchrec_item(BatchrecList), ContextIn-ContextOut, Stats) :-
	member(rank=Rank, BatchrecList),
	member(words=RecognisedWords, BatchrecList),
	member(transcription=SourceWords, BatchrecList),
	member(wavfile=Wavfile, BatchrecList),
	member(confidence=Confidence, BatchrecList),
	join_with_spaces(RecognisedWords, Recognised),
	join_with_spaces(SourceWords, Source),
	safe_transfer_and_generate_up_to_interlingua(Recognised, _Target, ContextIn-ContextOut, Stats0),
	(   member(paraphrase=ParaphraseWords, BatchrecList) ->
	    join_with_spaces(ParaphraseWords, Paraphrase),
	    Stats = [wavfile=Wavfile, rank=Rank, confidence=Confidence, sent=Source, recognised=RecognisedWords, paraphrase=Paraphrase | Stats0]
	;
	    otherwise ->
	    Stats = [wavfile=Wavfile, rank=Rank, confidence=Confidence, sent=Source, recognised=RecognisedWords | Stats0]
	).

%----------------------------------------------------------------------

set_context_from_sentence(Source, S, ContextIn-ContextOut) :-
	user:atom_to_parse_using_current_parser(Source, '.MAIN', SourceRepresentation),
	transfer_representation_to_source_discourse(SourceRepresentation,
						    SourceDiscourseRepresentation),
	set_preceding_target_utterance(Source, ContextIn, ContextNext),
	set_preceding_source_discourse(SourceDiscourseRepresentation, ContextNext, ContextOut),
	format(S, '~N%Sentence being used to set context: "~w"', [Source]),
	format(S, '~N%             New discourse context: ~q', [SourceDiscourseRepresentation]),
	format(S, '~N~n%---------------------------------------~n~n', []),
	!.

%----------------------------------------------------------------------

safe_transfer_and_generate_lf_to_interlingua(LF, Target, ContextIn-ContextOut, Stats) :-
	safe_transfer_and_generate_lf_to_interlingua(LF, Target, ContextIn-ContextOut, user, Stats).

safe_transfer_and_generate_lf_to_interlingua(LF, Target, ContextIn-ContextOut, Module, Stats) :-
	(   user:regulus_config(only_translate_up_to_interlingua, yes) ->
	    NormallyTranslatingToInterlingua = true
	;
	    assertz(user:regulus_config(only_translate_up_to_interlingua, yes)),
	    NormallyTranslatingToInterlingua = false
	),
	safe_transfer_and_generate(LF, Target, ContextIn-ContextOut, Stats, interlingua, Module),
	(   NormallyTranslatingToInterlingua = true ->
	    true
	;
	    retractall(user:regulus_config(only_translate_up_to_interlingua, yes))
	).

safe_transfer_and_generate_up_to_interlingua(Source, Target, ContextIn-ContextOut, Stats) :-
	safe_transfer_and_generate_up_to_interlingua(Source, Target, ContextIn-ContextOut, user, Stats).

safe_transfer_and_generate_up_to_interlingua(Source, Target, ContextIn-ContextOut, Module, Stats) :-
	(   user:regulus_config(only_translate_up_to_interlingua, yes) ->
	    NormallyTranslatingToInterlingua = true
	;
	    assertz(user:regulus_config(only_translate_up_to_interlingua, yes)),
	    NormallyTranslatingToInterlingua = false
	),
	safe_parse_transfer_and_generate(Source, Target, ContextIn-ContextOut, Stats, interlingua, Module),
	(   NormallyTranslatingToInterlingua = true ->
	    true
	;
	    retractall(user:regulus_config(only_translate_up_to_interlingua, yes))
	).

safe_parse_transfer_generate_and_backtranslate(Recognised, Target, Backtranslation, ContextIn-ContextOut, TargetLanguage, Stats, BackStats) :-
	safe_transfer_and_generate_up_to_interlingua(Recognised, Target, ContextIn-ContextOut, user, FromStats),
	(   member(interlingua=Interlingua, FromStats) ->
	    transfer_from_interlingua_and_generate(Interlingua, Target, ContextIn-_BackContextOut1, ToStats, TargetLanguage),
	    append(FromStats, ToStats, Stats),
	    transfer_from_interlingua_and_generate(Interlingua, Backtranslation, ContextIn-_BackContextOut2, BackStats, backtranslate)
	;
	    otherwise ->
	    Target = error(no_interlingua),
	    Backtranslation = error(no_interlingua),
	    Stats = FromStats,
	    BackStats = []
	).

safe_transfer_generate_and_backtranslate(Parse, Target, Backtranslation, ContextIn-ContextOut, Stats, BackStats) :-
	safe_transfer_generate_and_backtranslate(Parse, user, Target, Backtranslation, ContextIn-ContextOut, Stats, BackStats).

safe_transfer_generate_and_backtranslate(Parse, TargetLanguage, Target, Backtranslation, ContextIn-ContextOut, Stats, BackStats) :-
	safe_transfer_and_generate_lf_to_interlingua(Parse, _SurfaceInt, ContextIn-ContextOut, user, FromStats),
	(   member(interlingua=Interlingua, FromStats) ->
	    transfer_from_interlingua_and_generate(Interlingua, Target, ContextIn-_BackContextOut1, ToStats, TargetLanguage),
	    append(FromStats, ToStats, Stats),
	    transfer_from_interlingua_and_generate(Interlingua, Backtranslation, ContextIn-_BackContextOut2, BackStats, backtranslate)
	;
	   otherwise ->
	    Target = error(no_interlingua),
	    Backtranslation = error(no_interlingua),
	    Stats = FromStats,
	    BackStats = []
	).

safe_transfer_generate_and_backtranslate_delayed(Parse, TargetLanguage, SurfaceInt, Target, Backtranslation, ContextIn-ContextOut) :-
	safe_transfer_and_generate_lf_to_interlingua(Parse, SurfaceInt0, ContextIn-ContextOut, user, FromStats),
	(   ( member(interlingua=Interlingua, FromStats), \+ bad_interlingua_surface(SurfaceInt0) ) ->
	    SurfaceInt = SurfaceInt0,
	    Target = transfer_and_generate_from(Interlingua, TargetLanguage),
	    Backtranslation = transfer_and_generate_from(Interlingua, backtranslate)
	;
	    otherwise ->
	    SurfaceInt = no_surface_interlingua,
	    Target = no_translation,
	    Backtranslation = no_backtranslation
	).

perform_delayed_transfer_and_generation(transfer_and_generate_from(Interlingua, TargetLanguage), Translation) :-
	null_discourse_context(ContextIn),
	transfer_from_interlingua_and_generate(Interlingua, Translation, ContextIn-_ContextOut, _ToStats, TargetLanguage),
	!.
perform_delayed_transfer_and_generation(Translation, Translation).

safe_parse_transfer_and_generate(Recognised, Target, ContextIn-ContextOut, Stats, TranslationMode) :-
	safe_parse_transfer_and_generate(Recognised, Target, ContextIn-ContextOut, Stats, TranslationMode, user).

safe_parse_transfer_and_generate(Recognised, Target, ContextIn-ContextOut, Stats, TranslationMode, Module) :-
	on_exception(
	Exception, 
	(   parse_transfer_and_generate(Recognised, Target, ContextIn-ContextOut, Stats0, TranslationMode, Module),
	    remove_all_generation_tuples_item(Stats0, Stats)
	),
	handle_parse_transfer_and_generate_exception(Exception, Recognised, Target, ContextIn-ContextOut, Stats)
    ),
	!.
safe_parse_transfer_and_generate(_Recognised, Target, ContextIn-ContextOut, Stats, _TranslationMode, _Module) :-
	Target = internal_error,
	ContextOut = ContextIn,
	Stats = [],
	!.

handle_parse_transfer_and_generate_exception(Exception, Recognised, Target, ContextIn-ContextOut, Stats) :-
	format2error('~N~n*** Error: parse_transfer_and_generate/5 threw exception while processing "~w":~n~n', [Recognised]),
	format2error('~N~q~n', [Exception]),
	Target = internal_error,
	ContextOut = ContextIn,
	Stats = [],
	!.

safe_transfer_from_interlingua_and_generate(Interlingua, Target, ContextIn-ContextOut, Stats) :-
	on_exception(Exception,
		     (   transfer_from_interlingua_and_generate(Interlingua, Target, ContextIn-ContextOut, Stats0),
			 remove_all_generation_tuples_item(Stats0, Stats)
		     ),
		     handle_transfer_from_interlingua_and_generate_exception(Exception, Interlingua, Target, ContextIn-ContextOut, Stats)
		    ),
	!.
safe_transfer_from_interlingua_and_generate(_Interlingua, Target, ContextIn-ContextOut, Stats) :-
	Target = internal_error,
	ContextOut = ContextIn,
	Stats = [],
	!.

handle_transfer_from_interlingua_and_generate_exception(Exception, Interlingua, Target, ContextIn-ContextOut, Stats) :-
	format2error('~N~n*** Error: transfer_from_interlingua_and_generate/4 threw exception while processing "~w":~n~n', [Interlingua]),
	format2error('~N~q~n', [Exception]),
	Target = internal_error,
	ContextOut = ContextIn,
	Stats = [],
	!.


%----------------------------------------------------------------------

parse_transfer_and_generate_to_stream2(_Source, _Recognised, Wavfile, _Target, _Stats, S, ResultsIn-ResultsIn) :-
	ignore_wavfile(Wavfile),
	format(S, '~N%Wavfile: ~w', Wavfile),
	format(S, '~N%*** SKIPPING WAVFILE ***', []),
	!.
%parse_transfer_and_generate_to_stream2(Source, Recognised, Wavfile, _Target, _Stats, S, ResultsIn-ResultsOut) :-
%	bad_recognition(Source, Recognised),
%	(   Wavfile = text ->
%	    true ;
%	    format(S, '~N%Wavfile: ~w', Wavfile)
%	),
%	format(S, '~N%Source: ~w', Source),
%	format(S, '~N%Recognised: ~w~n', Recognised),
%	format(S, '~N%*** JUDGED AS BAD RECOGNITION ***', []),
%	inc_assoc_generic(ResultsIn, bad_recognition, ResultsOut),
%	!. 
parse_transfer_and_generate_to_stream2(Source, Recognised, Wavfile, Target, Stats, S, ResultsIn-ResultsOut) :-
	(   Wavfile = text ->
	    true ;
	    format(S, '~N%Wavfile: ~w', Wavfile)
	),
	format(S, '~N/*~n', []),
	%*/ to keep syntax highlighting happy...
	format(S, '~NSource: ~w', Source),
	(   Recognised = text_mode ->
	    
	    StatsWithRecognisedWords = Stats ;

	    StatsWithRecognisedWords = [wavfile=Wavfile, recognised=Recognised | Stats],
	    format(S, '~NRecognised: ~w~n', Recognised)
	),    
	format(S, '~NTarget: ~w~n', Target),
	inc_assoc_generic(ResultsIn, processed, ResultsNext),
	(   bad_recognition(Source, Recognised) ->

	    inc_assoc_generic(ResultsNext, bad_recognition, ResultsNext2),
	    format(S, '~N*** JUDGED AS BAD RECOGNITION ***', []),
	    Judgement = error
	;
	    error_result(Target) ->

	    inc_assoc_generic(ResultsNext, no_result, ResultsNext1),
	    Judgement = error,
	    format(S, '~N*** NO TRANSLATION ***~n', []),
	    (   get_good_or_ok_translation_for_source(Source, GoodOrOKTranslation) ->
		inc_assoc_generic(ResultsNext1, new_no_result, ResultsNext2),
		format(S, '~N*** PREVIOUS OK TRANSLATION: "~w" *** ~n', [GoodOrOKTranslation]);
		ResultsNext1 = ResultsNext2
	    )
	;
	    (   member(interlingua_surface=InterlinguaSurface, Stats) ->
		get_translation_judgement_using_interlingua(Source, InterlinguaSurface, Target, S, Judgement)
	    ;
		get_translation_judgement(Source, Target, Judgement)
	    ),
	    inc_assoc_generic(ResultsNext, Judgement, ResultsNext1),
	    (   member(Judgement, [bad]) ->
		format(S, '~N*** BAD TRANSLATION ***~n', []),
		(   get_good_or_ok_translation_for_source(Source, GoodOrOKTranslation) ->
		    inc_assoc_generic(ResultsNext1, new_bad, ResultsNext2),
		    format(S, '~N*** PREVIOUS OK TRANSLATION: "~w" *** ~n', [GoodOrOKTranslation]);
		    ResultsNext1 = ResultsNext2
		)
	    ;
		ResultsNext1 = ResultsNext2
	    )
	),

	(   Judgement = unknown ->
	    Judgement1 = '?',
	    (   get_good_or_ok_translation_for_source(Source, GoodOrOKTranslation) ->
		inc_assoc_generic(ResultsNext2, new_unknown, ResultsNext3),
		format(S, '~N*** PREVIOUS OK TRANSLATION: "~w" *** ~n', [GoodOrOKTranslation]);
		ResultsNext3 = ResultsNext2
	    )
	;
	    Judgement1 = Judgement,
	    ResultsNext3 = ResultsNext2
	),

	(   bad_interlingua_surface_in_list(Stats) ->
	    inc_assoc_generic(ResultsNext3, bad_interlingua, ResultsNext4)
	;
	    ResultsNext3 = ResultsNext4
	),

	(   member(n_generations=NGeneration, Stats) ->
	    inc_assoc_generic(ResultsNext4, total_generations, NGeneration, ResultsOut)
	;
	    ResultsNext4 = ResultsOut
	),
	
	present_interesting_stats(S, StatsWithRecognisedWords),

	format(S, '~N*/~n', []),

	% Version 1
	%format(S, '~N~n~q.~n', [translation(Source, Target, StatsWithRecognisedWords, Judgement1)]),

	% Version 2
	format(S, '~N~ntranslation(~n', []),
	format(S, '~N    ~q,~n', [Source]),
	format(S, '~N    ~q,~n', [Target]),
	format(S, '~N    ~q,~n', [StatsWithRecognisedWords]),
	format(S, '~N    ~q~n', [Judgement1]),
	format(S, '~N  ).~n', []),

	% Version 3 (if we want to prettyprint everything... but not clearly a good idea)
	%format(S, '~N~n', []),
	%prettyprintq_to_stream(S, translation(Source, Target, StatsWithRecognisedWords, Judgement1)),
	%format(S, '.~n', []),
	store_nbest_translation_summary(Source, Recognised, Wavfile, Target, Stats, Judgement1),
	!.
parse_transfer_and_generate_to_stream2(Source, Recognised, Wavfile, Target, Stats, S, Results) :-
	format2error('~N*** Error: bad call: ~w',
		     [parse_transfer_and_generate_to_stream2(Source, Recognised, Wavfile, Target, Stats, S, Results)]),
	fail.

present_interesting_stats(S, Stats) :-
	(   member(source_representation=FirstParse, Stats) ->
	    prettyprint_term_with_intro(S, '    Source representation', FirstParse)
	;
	    true
	),
	(   member(n_parses=NParses, Stats), NParses > 1 ->
	    format(S, '~N*** WARNING *** ~d parses~n', [NParses])
	;
	    true
	),
	(   member(parse_time=ParseTimeInSeconds, Stats) ->
	    prettyprint_term_with_intro(S, '               Parse time', ParseTimeInSeconds)
	;
	    true
	),
	(   member(source_discourse=SourceDiscourse, Stats) ->
	    prettyprint_term_with_intro(S, '         Source discourse', SourceDiscourse)
	;
	    true
	),
	(   member(resolved_source_discourse=ResolvedSourceDiscourse, Stats) ->
	    prettyprint_term_with_intro(S, 'Resolved source discourse', ResolvedSourceDiscourse)
	;
	    true
	),
	(   member(resolution_processing=ResolutionProcessing, Stats) ->
	    prettyprint_term_with_intro(S, '    Resolution processing', ResolutionProcessing)
	;
	    true
	),
	(   member(uninstantiated_interlingua=UninstantiatedInterlingua, Stats),
	    prettyprint_term_with_intro(S, '      Uninst. interlingua', UninstantiatedInterlingua)
	;
	    true
	),
	(   member(interlingua=Interlingua, Stats) ->
	    prettyprint_term_with_intro(S, '              Interlingua', Interlingua)
	;
	    true
	),
	(   member(interlingua_surface=InterlinguaSurface, Stats) ->
	    prettyprint_term_with_intro(S, '      Interlingua surface', InterlinguaSurface)
	;
	    true
	),
	(   member(raw_interlingua_surface=RawInterlinguaSurface, Stats) ->
	    prettyprint_term_with_intro(S, '  Raw interlingua surface', RawInterlinguaSurface)
	;
	    true
	),
	(   member(unknown_interlingua_elements=BadElements, Stats) ->
	    prettyprint_term_with_intro(S, 'Unknown interlingua items', BadElements)
	;
	    true
	),
	(   member(interlingua_checking_time=InterlinguaCheckingTimeInSeconds, Stats) ->
	    prettyprint_term_with_intro(S, 'Interlingua checking time', InterlinguaCheckingTimeInSeconds)
	;
	    true
	),
	(   member(other_interlingua_surface=OtherInterlinguaSurface, Stats) ->
	    prettyprint_term_with_intro(S, 'Other interlingua surface', OtherInterlinguaSurface)
	;
	    true
	),
	(   member(sem_recognition=SemRecognition, Stats) ->
	    prettyprint_term_with_intro(S, '     Semantic recognition', SemRecognition)
	;
	    true
	),	
	(   member(target_representation=TransferredParse, Stats) ->
	    prettyprint_term_with_intro(S, '    Target representation', TransferredParse)
	;
	    true
	),
	(   (   member(instantiated_target=InstantiatedTarget, Stats),
		\+ safe_subsumes_chk(InstantiatedTarget, TransferredParse)
	    ) ->
	    prettyprint_term_with_intro(S, '      Instantiated target', InstantiatedTarget)
	;
	    true
	),
	(   member(original_script_translation=OriginalScript, Stats) ->
	    prettyprint_term_with_intro(S, '          Original script', OriginalScript)
	;
	    true
	),
	(   member(gloss_translation=Gloss, Stats) ->
	    prettyprint_term_with_intro(S, '                    Gloss', Gloss)
	;
	    true
	),
	(   member(n_generations=NGenerations, Stats), NGenerations > 1 ->
	    format(S, '~N*** WARNING *** ~d generated strings~n', [NGenerations])
	;
	    true
	),
	(   member(generation_time=GenerationTimeInSeconds, Stats) ->
	    prettyprint_term_with_intro(S, '          Generation time', GenerationTimeInSeconds)
	;
	    true
	),
	!.
present_interesting_stats(S, Stats) :-
	format2error('~N*** Error: bad call: ~q~n',
		     [present_interesting_stats(S, Stats)]),
	fail.
       
%----------------------------------------------------------------------

make_csv_version_of_translation_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	(   file_exists(AbsFile) ->
	    true
	;
	    otherwise ->
	    format2error('~N*** Error: unable to find file: ~w~n', [AbsFile]),
	    fail
	),
	change_extension_in_file(AbsFile, csv, AbsCSVFile),
	open(AbsFile, read, SIn),
	open_regulus_file(AbsCSVFile, write, SOut),
	make_csv_version_of_translation_file1(SIn, SOut),
	close(SIn),
	close(SOut),
	format('~N--- Written CSV version of translation file: ~w~n~n', [AbsCSVFile]),
	!.
make_csv_version_of_translation_file(File) :-
	format2error('~N*** Error: bad call: ~w~n', [make_csv_version_of_translation_file(File)]),
	fail.

make_csv_version_of_translation_file1(SIn, SOut) :-
	read(SIn, Record),
	(   Record = end_of_file ->
	    true
	;
	    otherwise ->
	    write_csv_version_of_record(Record, SOut),
	    !,
	    make_csv_version_of_translation_file1(SIn, SOut)
	).

write_csv_version_of_record(Record, SOut) :-
	Record = translation(Source, Target0, _Info, Judgement),
	separate_source_into_real_source_and_context(Source, RealSource, Context),
	(   atomic(Target0) ->
	    Target = Target0
	;
	    otherwise ->
	    Target = error
	),
	context_marker_for_csv_record(Marker),
	% Add spaces after source and target to solve weird character set problems
	% in e.g. French -> Japanese
	format(SOut, '~N~w,~w ,~w ,~s~w~n', [Judgement, RealSource, Target, Marker, Context]),
	!.
write_csv_version_of_record(Record, SOut) :-
	format2error('~N*** Error: bad call: ~w~n', [write_csv_version_of_record(Record, SOut)]),
	fail.

context_marker_for_csv_record("context: ").

separate_source_into_real_source_and_context(Source, RealSource, Context) :-
	atomic(Source),
	!,
	RealSource = Source,
	Context = no_context.
separate_source_into_real_source_and_context(Source, RealSource, Context) :-
	Source = RealSource + Context,
	atomic(RealSource),
	atomic(Context),
	!.
separate_source_into_real_source_and_context(Source, RealSource, Context) :-
	Source = RealSource + ( Context + _ ),
	atomic(RealSource),
	!.
separate_source_into_real_source_and_context(Source, RealSource, Context) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [separate_source_into_real_source_and_context(Source, RealSource, Context)]),
	fail.

%----------------------------------------------------------------------

make_translation_file_from_csv_version(InFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	(   file_exists(AbsInFile) ->
	    true
	;
	    otherwise ->
	    format2error('~N*** Error: unable to find file ~w~n', [AbsInFile]),
	    fail
	),
	change_extension_in_file(AbsInFile, pl, AbsOutFile),
	read_file_to_atom_list(AbsInFile, InList),
	format('~N--- Read CSV file ~w~n', [AbsInFile]),
	
	make_translation_list_from_csv_list(InList, OutList),

	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written normal translation file ~w~n', [AbsOutFile]),
	!.
make_translation_file_from_csv_version(InFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_translation_file_from_csv_version(InFile)]),
	fail.

make_translation_list_from_csv_list([], []).
make_translation_list_from_csv_list([F | R], [F1 | R1]) :-
	make_translation_record_from_csv_record(F, F1),
	!,
	make_translation_list_from_csv_list(R, R1).

make_translation_record_from_csv_record(CSVRecord, TranslationRecord) :-
	atomic(CSVRecord),
	atom_codes(CSVRecord, CSVString),
	remove_any_dubious_chars_from_csv_string(CSVString, CSVString1),
	split_string_into_words(CSVString1, 0',, Fields),
	Fields = [Judgement, Source0, Target, Context0],
	remove_context_marker_from_context_field(Context0, Context),
	(   Context = no_context ->
	    Source = Source0
	;
	    otherwise ->
	    Source = ( Source0 + Context )
	),
	TranslationRecord = translation(Source, Target, [], Judgement),
	!.
make_translation_record_from_csv_record(CSVRecord, TranslationRecord) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_translation_record_from_csv_record(CSVRecord, TranslationRecord)]),
	fail.

remove_any_dubious_chars_from_csv_string([], []).
remove_any_dubious_chars_from_csv_string([F | R], R1) :-
	dubious_char_in_cvs_string(F),
	!,
	remove_any_dubious_chars_from_csv_string(R, R1).
remove_any_dubious_chars_from_csv_string([F | R], [F | R1]) :-
	!,
	remove_any_dubious_chars_from_csv_string(R, R1).

% Open Office can insert quotes around the elements.
dubious_char_in_cvs_string(0'").            %" to keep Emacs happy

remove_context_marker_from_context_field(Context0, Context) :-
	atom_codes(Context0, Context0Chars),
	context_marker_for_csv_record(Marker),
	append(Marker, ContextChars, Context0Chars),
	atom_codes(Context, ContextChars),
	!.
remove_context_marker_from_context_field(Context0, Context) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_context_marker_from_context_field(Context0, Context)]),
	fail.

%----------------------------------------------------------------------

parse_and_transfer_to_all_interlingua(Atom, ContextIn, Tuples) :-
	timed_call(atom_to_all_parses(Atom, AllParses),
		   ParseTimeInSeconds),
	length(AllParses, NParses),
	(   NParses = 0 ->
	    user:report_unknown_words_in_sent_atom(Atom),
	    Tuple = [target=parsing_failed,
		     context_out=ContextIn, 
		     stats=[n_parses=0]],
	    Tuples = [Tuple]
	;
	    otherwise ->
	    ParseStats = [n_parses=NParses, parse_time=ParseTimeInSeconds],
	    transfer_and_generate_all_to_interlingua(AllParses, ParseStats, ContextIn, Tuples)
	),
	!.

transfer_and_generate_all_to_interlingua([], _ParseStats, _ContextIn, []).
transfer_and_generate_all_to_interlingua([F | R], ParseStats, ContextIn, [F1 | R1]) :-
	transfer_and_generate_to_interlingua_tuple(F, ParseStats, ContextIn, F1),
	!,
	transfer_and_generate_all_to_interlingua(R, ParseStats, ContextIn, R1).

transfer_and_generate_to_interlingua_tuple(Parse, ParseStats, ContextIn, Tuple) :-
	list_to_ord_set(Parse, ParseOS),
	append(ParseStats, [source_representation=ParseOS | OtherStats], Stats),
	safe_transfer_and_generate_lf_to_interlingua(Parse, Result, ContextIn-ContextNext, OtherStats, interlingua),
	(   error_result(Result) ->
	    ContextOut = ContextIn
	;
	    ContextOut = ContextNext
	),
	Tuple = [interlingua=Result,
		 context_out=ContextOut, 
		 stats=Stats].

%----------------------------------------------------------------------

parse_transfer_and_generate(Atom, Result, ContextIn-ContextOut, TranslationMode) :-
	parse_transfer_and_generate(Atom, Result, ContextIn-ContextOut, TranslationMode, user).

parse_transfer_and_generate(Atom, Result, ContextIn-ContextOut, TranslationMode, Module) :-
	parse_transfer_and_generate(Atom, Result, ContextIn-ContextOut, _Stats, TranslationMode, Module).

parse_transfer_and_generate(Atom, Result, ContextIn-ContextOut,
			    [n_parses=NParses,
			     parse_time=ParseTimeInSeconds,
			     source_representation=FirstParseOS |
			    OtherStats],
			    TranslationMode,
			    Module) :-
	timed_call(atom_to_all_parses(Atom, AllParses),
		   ParseTimeInSeconds),
	AllParses = [FirstParse | _Rest],
	list_to_ord_set(FirstParse, FirstParseOS),
	length(AllParses, NParses),
	!,
	transfer_and_generate(FirstParse, Result, ContextIn-ContextNext, OtherStats, TranslationMode, Module),
	(   error_result(Result) ->
	    ContextOut = ContextIn
	;
	    ContextOut = ContextNext
	).
parse_transfer_and_generate(Atom, parsing_failed, ContextIn-ContextIn, [n_parses=0], _TranslationMode, _Module) :-
	user:report_unknown_words_in_sent_atom(Atom),
	!.

safe_transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, TranslationMode) :-
	safe_transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, TranslationMode, user).

safe_transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, TranslationMode, Module) :-
	on_exception(Exception,
		     transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, TranslationMode, Module),
		     handle_transfer_and_generate_exception(Exception, Parse, Result, ContextIn-ContextIn, Stats, TranslationMode)
		    ),
	!.
handle_transfer_and_generate_exception(Exception, Parse, Target, ContextIn-ContextOut, Stats, _TranslationMode) :-
	format2error('~N~n*** Error: transfer_and_generate/5 threw exception while processing "~w":~n~n', [Parse]),
	format2error('~N~q~n', [Exception]),
	Target = internal_error,
	ContextOut = ContextIn,
	Stats = [],
	!.

transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, interlingua) :-
	transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, interlingua, user).

transfer_and_generate(Parse, Result, ContextIn-ContextIn,
		      [target_representation=TransferredParse | OtherStats], direct, Module) :-
	transfer_representation(Parse, TransferredParse),
	!,
	generate(TransferredParse, Result, ContextIn-_ContextOut, OtherStats, Module).
transfer_and_generate(Parse, transfer_failed(Parse), ContextIn-ContextIn, [], direct, _Module) :-
	!.

transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, interlingua, Module) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(translate_from_interlingua, yes),
	!,
	transfer_from_interlingua_and_generate(Parse, Result, ContextIn-ContextOut, Stats, Module).
transfer_and_generate(Parse, Result, ContextIn-ContextOut, Stats, interlingua, Module) :-
	timed_call(transfer_representation_to_source_discourse(Parse, SourceDiscourse, _Trace, Module),
		   TransferToSourceDiscourseTime),
	!,
	(   term_contains_functor(SourceDiscourse, unable_to_translate/0) ->

	    Stats = [transfer_to_source_discourse_time=TransferToSourceDiscourseTime],
	    ContextOut = ContextIn,
	    Result = transfer_to_source_discourse_failed(SourceDiscourse)
	;
	    Stats = [transfer_to_source_discourse_time=TransferToSourceDiscourseTime,
		     source_discourse=SourceDiscourse |
		    OtherStats],
	    resolve_transfer_from_source_discourse_and_generate(SourceDiscourse, Result, ContextIn-ContextOut, OtherStats, Module)
	).
transfer_and_generate(Parse, transfer_to_source_discourse_failed(Parse), ContextIn-ContextIn, [], interlingua, _Module).

resolve_transfer_from_source_discourse_and_generate(SourceDiscourse, Result, ContextIn-ContextOut, OtherStats) :-
	resolve_transfer_from_source_discourse_and_generate(SourceDiscourse, Result, ContextIn-ContextOut, OtherStats, user).

resolve_transfer_from_source_discourse_and_generate(SourceDiscourse, Result, ContextIn-ContextOut,
						    [resolved_source_discourse=ResolvedSourceDiscourse,
						     resolution_processing=ResolutionProcessing,
						     resolution_time=ResolutionTime
						    | OtherStats],
						    Module) :-
	get_preceding_source_discourse(ResolvedSourceDiscourseIn, ContextIn),
	timed_call(perform_resolution(SourceDiscourse, ResolvedSourceDiscourseIn, ResolvedSourceDiscourse, ResolutionProcessing),
		   ResolutionTime),
	set_preceding_source_discourse(ResolvedSourceDiscourse, ContextIn, ContextNext),
	!,
	transfer_from_resolved_source_discourse_and_generate(ResolvedSourceDiscourse, Result, ContextNext-ContextOut, OtherStats, Module).
resolve_transfer_from_source_discourse_and_generate(SourceDiscourse, resolution_failed(SourceDiscourse),
						    ContextIn-ContextIn, [], _Module).

transfer_from_resolved_source_discourse_and_generate(ResolvedSourceDiscourse, Result, ContextNext-ContextOut, OtherStats) :-
	transfer_from_resolved_source_discourse_and_generate(ResolvedSourceDiscourse, Result, ContextNext-ContextOut, OtherStats, user).

transfer_from_resolved_source_discourse_and_generate(ResolvedSourceDiscourse, Result,
						     ContextIn-ContextOut, Stats, Module) :-
	timed_call(transfer_representation_to_interlingua(ResolvedSourceDiscourse, ResolvedInterlingua0, _Trace, Module),
		   TransferToInterlinguaTime),
	!,
	make_transfer_representation_canonical_and_unpack_if_necessary(ResolvedInterlingua0, ResolvedInterlingua),
	Stats = [transfer_to_interlingua_time=TransferToInterlinguaTime | Stats0],
		 
	(   term_contains_functor(ResolvedInterlingua, unable_to_translate/0) ->

	    Stats0 = [],
	    ContextOut = ContextIn,
	    Result = transfer_to_interlingua_failed(ResolvedInterlingua)
	;
	    current_predicate(check_interlingua:check_interlingua/3) ->

	    copy_term(ResolvedInterlingua, UninstantiatedInterlingua),
	    timed_call(check_interlingua_structure(ResolvedInterlingua, check_interlingua, check_interlingua,
						   RawInterlinguaSurfaceForm, ResolvedInterlinguaSurfaceForm,
						   _Tree, OtherResolvedInterlinguaSurfaceForms),
		       InterlinguaCheckingTimeInSeconds),
	    
	    (   safe_subsumes_chk(ResolvedInterlingua, UninstantiatedInterlingua) ->
		Stats0 = Stats1
	    ;
		otherwise ->
		Stats0 = [uninstantiated_interlingua=UninstantiatedInterlingua | Stats1]
	    ),

	    (   ResolvedInterlinguaSurfaceForm = 'WARNING: INTERLINGUA REPRESENTATION FAILED STRUCTURE CHECK' ->
		find_unknown_elements_in_interlingua_form(ResolvedInterlingua, BadElements),
		Stats1 = [unknown_interlingua_elements=BadElements | Stats2]
	    ;
		otherwise ->
		Stats1 = Stats2
	    ),
	    
	    (   OtherResolvedInterlinguaSurfaceForms \== [] ->
		Stats2 = [interlingua=ResolvedInterlingua,
			  interlingua_surface=ResolvedInterlinguaSurfaceForm,
			  other_interlingua_surface=OtherResolvedInterlinguaSurfaceForms,
			  interlingua_checking_time=InterlinguaCheckingTimeInSeconds |
			 Stats3]
	    ;
		otherwise ->
		Stats2 = [interlingua=ResolvedInterlingua,
			  interlingua_surface=ResolvedInterlinguaSurfaceForm,
			  interlingua_checking_time=InterlinguaCheckingTimeInSeconds |
			 Stats3]
	    ),

	    (   RawInterlinguaSurfaceForm = ResolvedInterlinguaSurfaceForm ->
		Stats3 = OtherStats
	    ;
		otherwise ->
		Stats3 = [raw_interlingua_surface=RawInterlinguaSurfaceForm | OtherStats]
	    ),
	    
	    (   only_translate_up_to_interlingua ->
		Result = ResolvedInterlinguaSurfaceForm,
		set_preceding_target_utterance(Result, ContextIn, ContextOut),
		OtherStats = []
	    ;
		otherwise ->
		transfer_from_interlingua_and_generate(ResolvedInterlingua, Result, ContextIn-ContextOut, OtherStats, Module)
	    )
	;
	    otherwise ->
	    Stats0 = [interlingua=ResolvedInterlingua | OtherStats],
	    transfer_from_interlingua_and_generate(ResolvedInterlingua, Result, ContextIn-ContextOut, OtherStats, Module)
	).
transfer_from_resolved_source_discourse_and_generate(ResolvedSourceDiscourse,
						     transfer_to_interlingua_failed(ResolvedSourceDiscourse),
						     ContextIn-ContextIn, [], _Module) :-
	!.

transfer_from_interlingua_and_generate(ResolvedInterlingua, Result, ContextIn-ContextOut, OtherStats) :-
	transfer_from_interlingua_and_generate(ResolvedInterlingua, Result, ContextIn-ContextOut, OtherStats, user).

transfer_from_interlingua_and_generate(Interlingua, Result,
				       ContextIn-ContextIn, Stats, _Module) :-
	user:regulus_config(only_translate_up_to_interlingua, yes),
	!,
	% CHANGE HERE
	timed_call(check_interlingua_structure(Interlingua, Result, _Tree, OtherSurfaceForms),
		   GenerationTimeInSeconds),
	length(OtherSurfaceForms, NOther),
	NGenerations is NOther + 1,
	Stats = [n_generations=NGenerations,
		 generation_time=GenerationTimeInSeconds,
		 other_translations=OtherSurfaceForms].	    
transfer_from_interlingua_and_generate(ResolvedInterlingua, Result,
				       ContextIn-ContextOut, Stats, Module) :-
	timed_call(transfer_representation_from_interlingua(ResolvedInterlingua, TargetRepresentation, _Trace, Module),
		   FromInterlinguaTransferTime),
	!,
	(   term_contains_functor(TargetRepresentation, unable_to_translate/0) ->

	    Stats = [transfer_from_interlingua_time=FromInterlinguaTransferTime],
	    ContextOut = ContextIn,
	    Result = transfer_from_interlingua_failed(TargetRepresentation)
	;
	    Stats = [transfer_from_interlingua_time=FromInterlinguaTransferTime,
		     target_representation=TargetRepresentation |
		    OtherStats],
	    generate(TargetRepresentation, Result, ContextIn-ContextOut, OtherStats, Module)
	).	    
transfer_from_interlingua_and_generate(ResolvedInterlingua, transfer_from_interlingua_failed(ResolvedInterlingua),
				       ContextIn-ContextIn, [], _Module) :-
	!.

generate(TransferredParse, Result, ContextIn-ContextOut, FullInfo) :-
	generate(TransferredParse, Result, ContextIn-ContextOut, FullInfo, user).

generate(TransferredParse, Result, ContextIn-ContextOut, FullInfo, Module) :-
	timed_call(generate_all_surface_word_tuples(TransferredParse, AllTuples, Module),
		   GenerationTimeInSeconds),
	AllTuples = [PreferredTuple | OtherTuples],
	length(AllTuples, NGenerations),
	member(words=PreferredGeneratedWords, PreferredTuple),
	member(instantiated_representation=PreferredInstantiatedRepresentation, PreferredTuple),
	extract_word_lists_from_tuples(OtherTuples, OtherGeneratedWords),
	extract_tagged_words_from_tuples(AllTuples, AllTaggedWords),
	join_with_spaces(PreferredGeneratedWords, Result),
	set_preceding_target_utterance(Result, ContextIn, ContextOut),

	MainInfo = [instantiated_target=PreferredInstantiatedRepresentation,
		    n_generations=NGenerations,
		    generation_time=GenerationTimeInSeconds,
		    other_translations=OtherGeneratedWords,
		    tagged_translations=AllTaggedWords,
		    %generated_tree=PreferredGeneratedTree,
		    recognition_orthography_words=RecognitionOrthographyWords,
		    all_generation_tuples=AllTuples],

	member(tree=PreferredGeneratedTree, PreferredTuple),
	member(recognition_orthography_words=RecognitionOrthographyWords, PreferredTuple),
	(   generate_original_script_result(TransferredParse, PreferredGeneratedTree, OriginalScript) ->
	    Info1 = [original_script_translation=OriginalScript | MainInfo] ;
	    Info1 = MainInfo
	),
	(   generate_gloss_result(TransferredParse, PreferredGeneratedTree, Gloss) ->
	    FullInfo = [gloss_translation=Gloss | Info1] ;
	    FullInfo = Info1
	),
	!.
generate(TransferredParse, Result, ContextIn-ContextIn, [], _Module) :-
	generator_sem_constants_missing_for_sem(TransferredParse, MissingSemConstants),
	(   MissingSemConstants = [] ->
	    Result = generation_failed(TransferredParse)
	;
	    Result = generation_failed(TransferredParse, missing_generation_constants:MissingSemConstants)
	),
	!.
generate(TransferredParse, generation_failed(TransferredParse), ContextIn-ContextIn, [], _Module) :-
	!.

remove_all_generation_tuples_item([], []).
remove_all_generation_tuples_item([all_generation_tuples=_ | R], R) :-
	!.
remove_all_generation_tuples_item([F | R], [F | R1]) :-
	remove_all_generation_tuples_item(R, R1).

%----------------------------------------------------------------------

only_translate_up_to_interlingua :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(only_translate_up_to_interlingua, yes),
	!.

%----------------------------------------------------------------------

interlingua_debugging_time_limit(60).

check_interlingua_structure(Interlingua, InterlinguaSurfaceForm) :-
	check_interlingua_structure(Interlingua, check_interlingua, check_interlingua, InterlinguaSurfaceForm, _Tree, _OtherSurfaceForms).

check_alternate_interlingua_structure(Interlingua, Tag, InterlinguaSurfaceForm) :-
	get_module_for_alternate_interlingua_structure(Tag, Module),
	check_interlingua_structure(Interlingua, Tag, Module, InterlinguaSurfaceForm, _Tree, _OtherSurfaceForms).

check_interlingua_structure(Interlingua, InterlinguaSurfaceForm, Tree) :-
	check_interlingua_structure(Interlingua, check_interlingua, check_interlingua, InterlinguaSurfaceForm, Tree, _OtherSurfaceForms).

check_interlingua_structure(Interlingua, InterlinguaSurfaceForm, Tree, OtherSurfaceForms) :-
	check_interlingua_structure(Interlingua, check_interlingua, check_interlingua, InterlinguaSurfaceForm, Tree, OtherSurfaceForms).

check_interlingua_structure(Interlingua, Tag, InterlinguaSurfaceForm, Tree, OtherSurfaceForms) :-
	get_module_for_alternate_interlingua_structure(Tag, Module),
	check_interlingua_structure(Interlingua, Tag, Module, InterlinguaSurfaceForm, Tree, OtherSurfaceForms).

check_interlingua_structure(Interlingua, Tag, Module, InterlinguaSurfaceForm, Tree, OtherSurfaceForms) :-
	check_interlingua_structure(Interlingua, Tag, Module,
				    _RawInterlinguaSurfaceForm, InterlinguaSurfaceForm, Tree, OtherSurfaceForms).

check_interlingua_structure(Interlingua, Tag, Module,
			    RawInterlinguaSurfaceForm, InterlinguaSurfaceForm, Tree, OtherSurfaceForms) :-
	current_predicate(Module:check_interlingua/3),
	(   ( interlingua_structure_debugging ; nondeterministic_interlingua_grammar ) ->
	    findall([Interlingua, SomeTree, SomeRawSurfaceForm, SomeSurfaceForm],
		    (   Module:check_interlingua(Interlingua, SomeTree, SomeSurfaceWords0),
			remove_null_items_in_generated_words(SomeSurfaceWords0, SomeSurfaceWords1),
			join_with_spaces(SomeSurfaceWords1, SomeRawSurfaceForm),
			improve_interlingua_collocations(SomeSurfaceWords1, Module, SomeSurfaceWords),
			join_with_spaces(SomeSurfaceWords, SomeSurfaceForm0),
			fix_special_orthography_on_atom(SomeSurfaceForm0, Module, SomeSurfaceForm)
		    ),
		    Tuples),
	    Tuples = [[Interlingua, Tree, RawInterlinguaSurfaceForm, InterlinguaSurfaceForm] | Rest],
	    extract_interlingua_surface_forms_from_tuples(Rest, OtherSurfaceForms0),
	    safe_remove_duplicates_preserving_order(OtherSurfaceForms0, OtherSurfaceForms1),
	    delete(OtherSurfaceForms1, InterlinguaSurfaceForm, OtherSurfaceForms)
	;
	    otherwise ->
	    Module:check_interlingua(Interlingua, Tree, SurfaceWords0),
	    remove_null_items_in_generated_words(SurfaceWords0, SurfaceWords1),
	    join_with_spaces(SurfaceWords1, RawInterlinguaSurfaceForm),
	    improve_interlingua_collocations(SurfaceWords1, Tag, SurfaceWords),
	    join_with_spaces(SurfaceWords, SurfaceForm0),
	    fix_special_orthography_on_atom(SurfaceForm0, Tag, InterlinguaSurfaceForm),
	    OtherSurfaceForms = []
	),
	(   interlingua_tracing ->
	    
	    format('~N~nInterlingua gloss:~n~n~w~n~n', [InterlinguaSurfaceForm]),
	    prettyprint_parse_tree(Tree, print_line_info) ;
	    
	    true
	),
	!.
check_interlingua_structure(Interlingua, Tag, Module,
			    RawInterlinguaSurfaceForm, InterlinguaSurfaceForm, no_tree, []) :-
	current_predicate(Module:check_interlingua/3),
	interlingua_structure_debugging,
	interlingua_debugging_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	time_out(( underinstantiated_interlingua_variant(Change, Interlingua, Interlingua1),
		   Module:check_interlingua(Interlingua1, _Tree, VariantInterlinguaSurfaceWords0),
		   remove_null_items_in_generated_words(VariantInterlinguaSurfaceWords0, VariantInterlinguaSurfaceWords1),
		   improve_interlingua_collocations(VariantInterlinguaSurfaceWords1, Tag, VariantInterlinguaSurfaceWords)
		 ),
		 TimeLimitInMilliseconds,
		 Result),
	(   Result = time_out ->
	    format_to_atom('WARNING: INTERLINGUA REPRESENTATION FAILED STRUCTURE CHECK. DEBUGGING TIMED OUT AT ~d SECONDS',
			   [TimeLimit],
			   InterlinguaSurfaceForm)
	;
	    otherwise ->
	    join_with_spaces(VariantInterlinguaSurfaceWords, VariantInterlinguaSurfaceForm),
	    join_with_spaces(VariantInterlinguaSurfaceWords1, RawInterlinguaSurfaceForm),
	    format_to_atom('WARNING: INTERLINGUA REPRESENTATION FAILED STRUCTURE CHECK. APPLYING MODIFICATION (~w) GIVES "~w"',
			   [Change, VariantInterlinguaSurfaceForm],
			   InterlinguaSurfaceForm)
	),
	!.
check_interlingua_structure(_Interlingua, _Tag, Module, RawInterlinguaSurfaceForm, InterlinguaSurfaceForm, no_tree, []) :-
	current_predicate(Module:check_interlingua/3),
	InterlinguaSurfaceForm = 'WARNING: INTERLINGUA REPRESENTATION FAILED STRUCTURE CHECK',
	RawInterlinguaSurfaceForm = 'WARNING: INTERLINGUA REPRESENTATION FAILED STRUCTURE CHECK',
	!.
check_interlingua_structure(_Interlingua, _Tag, _Module, _RawInterlinguaSurfaceForm, _InterlinguaSurfaceForm, _Tree, []).

nondeterministic_interlingua_grammar :-
	user:regulus_config(nondeterministic_interlingua_grammar, yes),
	!.

get_module_for_alternate_interlingua_structure(Tag, Module) :-
	user:regulus_config(alternate_interlingua_structure(Tag), [_File, Module]),
	!.
get_module_for_alternate_interlingua_structure(Tag, Module) :-
	user:regulus_config(default_interlingua_structure, Tag),
	Module = check_interlingua,
	!.
% Obsolete but continue to support
get_module_for_alternate_interlingua_structure(Tag, Module) :-
	user:regulus_config(default_interlingua_structure_tag, Tag),
	Module = check_interlingua,
	!.
%get_module_for_alternate_interlingua_structure(Tag, Module) :-
%	format('~N*** Error: bad call: ~w~n', [get_module_for_alternate_interlingua_structure(Tag, Module)]),
%	fail.

extract_interlingua_surface_forms_from_tuples([], []).
extract_interlingua_surface_forms_from_tuples([F | R], [F1 | R1]) :-
	extract_interlingua_surface_form_from_tuple(F, F1),
	!,
	extract_interlingua_surface_forms_from_tuples(R, R1).

extract_interlingua_surface_form_from_tuple([_Interlingua, _Tree, _RawSurfaceForm, SurfaceForm], SurfaceForm) :-
	!.
extract_interlingua_surface_form_from_tuple(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [extract_interlingua_surface_form_from_tuple(X, Y)]),
	fail.

underinstantiated_interlingua_variant((X --> Y), Interlingua, [Y | Interlingua1]) :-
	remove_element_nondeterministic(Interlingua, X, Interlingua1).
underinstantiated_interlingua_variant((null --> X), Interlingua, [X | Interlingua]).
underinstantiated_interlingua_variant((X --> null), Interlingua, Interlingua1) :-
	remove_element_nondeterministic(Interlingua, X, Interlingua1).
underinstantiated_interlingua_variant(Change, Interlingua, Interlingua1) :-
	underinstantiated_interlingua_variant_clause(Change, Interlingua, Interlingua1).

underinstantiated_interlingua_variant_clause(Change, [[clause, Clause] | R], [[clause, Clause1] | R]) :-
	underinstantiated_interlingua_variant(Change, Clause, Clause1).
underinstantiated_interlingua_variant_clause(Change, [Role=[clause, Clause] | R], [Role=[clause, Clause1] | R]) :-
	underinstantiated_interlingua_variant(Change, Clause, Clause1).
underinstantiated_interlingua_variant_clause(Change, [F | R], [F | R1]) :-
	underinstantiated_interlingua_variant_clause(Change, R, R1).

remove_element_nondeterministic([X | R], X, R) :-
	\+ clause_element(X).
remove_element_nondeterministic([F | R], X, [F | R1]) :-
	remove_element_nondeterministic(R, X, R1).

clause_element([clause, _]).
clause_element(_Role=[clause, _]).

%----------------------------------------------------------------------

package_translation_corpus_results_file_for_java_gui(ResultsFile, AnswerTerm) :-
	safe_absolute_file_name(ResultsFile, AbsResultsFile),
	atom_codes(AbsResultsFile, AbsResultsFileString),
	prolog_file_to_list(AbsResultsFile, ResultsList),
	length(ResultsList, N),
	package_translation_corpus_results_list(ResultsList, ResultsTerm),
	AnswerTerm = translation_results(AbsResultsFileString, ResultsTerm),
	format('~N--- Packaged translation corpus results: ~d items, file = "~s"~n', [N, AbsResultsFileString]),
	!.
package_translation_corpus_results_file_for_java_gui(ResultsFile, AnswerTerm) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [package_translation_corpus_results_file_for_java_gui(ResultsFile, AnswerTerm)]),
	fail.

package_translation_corpus_results_list([], []).
package_translation_corpus_results_list([F | R], [F1 | R1]) :-
	package_translation_corpus_result(F, F1),
	!,
	package_translation_corpus_results_list(R, R1).

package_translation_corpus_result(Record, RecordTerm) :-
	Record = translation(Source0, Target0, _InfoAlist, Judgement),
	(   Source0 = ( Source + Context ) ->
	    
	    true ;
	    
	    Source = Source0,
	    Context = no_context
	),
	(   \+ atom(Target0) ->

	    Target = '*no_translation*' ;

	    Target = Target0
	),
	package_translation_results_for_java_gui(Source, Target, [context=Context, judgement=Judgement], RecordTerm),
	!.
package_translation_corpus_result(Record, RecordTerm) :-
	format2error('~N*** Error: bad call: ~w~n', [package_translation_corpus_result(Record, RecordTerm)]),
	fail.

%----------------------------------------------------------------------

unpack_and_store_judged_translation_output_file_from_string(ResultsFile, JudgedFileString) :-
	get_file_items_from_string(JudgedFileString, JudgedFileItems),
	absolute_file_name(ResultsFile, AbsResultsFile),
	open_regulus_file(AbsResultsFile, write, S),
	write_judged_translation_items_to_stream(JudgedFileItems, S),
	close(S).

get_file_items_from_string(JudgedFileString, JudgedFileItems) :-	
	on_exception(
		     _Exception,
		     read_from_chars(JudgedFileString, JudgedFileItems),
		     handle_bad_judged_translation_output_string(JudgedFileString)
		    ),
	!.

handle_bad_judged_translation_output_string(JudgedFileString) :-
	format2error('~*** Bad judged translation string received: "~s"~n', [JudgedFileString]),
	fail.

write_judged_translation_items_to_stream([], _S).
write_judged_translation_items_to_stream([F | R], S) :-
	write_judged_translation_item_to_stream(F, S),
	!,
	write_judged_translation_items_to_stream(R, S).

write_judged_translation_item_to_stream(Record, S) :-
	Record = translation(Source, Target, InfoAlist, Judgement),
	(   ( InfoAlist = [context=Context], Context \== no_context ) ->
	    Source1 = Source + Context ;
	    Source1 = Source
	),
	format(S, '~N~ntranslation(~n', []),
	format(S, '~N    ~q,~n', [Source1]),
	format(S, '~N    ~q,~n', [Target]),
	format(S, '~N    ~q,~n', [[]]),
	format(S, '~N    ~q~n', [Judgement]),
	format(S, '~N  ).~n', []),
	!.
write_judged_translation_item_to_stream(Record, S) :-
	format2error('~N*** Error: bad call: ~w~n', [write_judged_translation_item_to_stream(Record, S)]),
	fail.

%----------------------------------------------------------------------

package_translation_results_for_java_gui(Source, Target, InfoAlist, AnswerTerm) :-
	package_processing_alist_for_java_gui([source=Source, target=Target | InfoAlist], PackagedAlist0),
	(   user:regulus_config(original_script_encoding, Encoding) ->
	    append(PackagedAlist0, [character_encoding=Encoding], PackagedAlist)
	;
	    PackagedAlist0 = PackagedAlist
	),
	AnswerTerm =.. [translation_info | PackagedAlist],
	!.
package_translation_results_for_java_gui(_Target, _InfoAlist, translation_info(target=internal_error)).

%----------------------------------------------------------------------

ignore_wavfile(Wavfile) :-
	current_predicate(user:relevant_wavfile/1),
	\+ user:relevant_wavfile(Wavfile),
	!.

%----------------------------------------------------------------------

error_result(parsing_failed).
error_result(transfer_failed(_Parse)).
error_result(transfer_to_source_discourse_failed(_Representation)).
error_result(transfer_to_interlingua_failed(_Representation)).
error_result(resolution_failed(_Representation)).
error_result(transfer_from_interlingua_failed(_Representation)).
error_result(generation_failed(_TransferredParse)).
error_result(generation_timed_out).
error_result(InterlinguaSurface) :-
	bad_interlingua_surface(InterlinguaSurface).
	
%error_result(no_translation_due_to_bad_recognition).

%----------------------------------------------------------------------

get_translation_judgement_using_interlingua(Source, InterlinguaSurface, Target, S, Judgement) :-
	get_translation_judgement(Source, Target, user, DirectJudgement),
	get_translation_judgement(Source, InterlinguaSurface, to_interlingua_judgements, ToInterlinguaJudgement),
	get_translation_judgement(InterlinguaSurface, Target, from_interlingua_judgements, FromInterlinguaJudgement),
	combine_translation_judgements(ToInterlinguaJudgement, FromInterlinguaJudgement, IndirectJudgement),
	warn_if_translation_judgements_are_different(DirectJudgement, IndirectJudgement, S),
	(   ( DirectJudgement = unknown, IndirectJudgement \== unknown ) ->
	    Judgement = IndirectJudgement
	;
	    otherwise ->
	    Judgement = DirectJudgement
	),
	!.
get_translation_judgement_using_interlingua(Source, InterlinguaSurface, Target, S, Judgement) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [get_translation_judgement_using_interlingua(Source, InterlinguaSurface, Target, S, Judgement)]),
	fail.

combine_translation_judgements(Judgement1, Judgement2, Judgement) :-
	Judgement1 = good,
	Judgement2 = good,
	!,
	Judgement = good.
combine_translation_judgements(Judgement1, Judgement2, Judgement) :-
	member(Judgement1, [good, ok]),
	member(Judgement2, [good, ok]),
	!,
	Judgement = ok.
combine_translation_judgements(Judgement1, Judgement2, Judgement) :-
	(   Judgement1 = error
	;   Judgement2 = error
	),
	!,
	Judgement = error.
combine_translation_judgements(Judgement1, Judgement2, Judgement) :-
	(   Judgement1 = bad
	;   Judgement2 = bad
	),
	!,
	Judgement = bad.
combine_translation_judgements(_Judgement1, _Judgement2, Judgement) :-
	Judgement = unknown.

warn_if_translation_judgements_are_different(Judgement, IndirectJudgement, S) :-
	Judgement \== IndirectJudgement,
	Judgement \== unknown,
	IndirectJudgement \== unknown,
	(   seriously_incompatible_judgements(Judgement, IndirectJudgement) ->
	    format(S, '~N*** Warning: seriously incompatible translation judgements~n', [])
	;
	    format(S, '~N*** Warning: incompatible translation judgements~n', [])
	),
	format(S, '~N*** Direct translation judgement = ~w, interlingua-based judgement = ~w~n',
	       [Judgement, IndirectJudgement]),
	!.
warn_if_translation_judgements_are_different(_Judgement, _IndirectJudgement, _S) :-
	!.

seriously_incompatible_judgements(Judgement1, Judgement2) :-
	member(Judgement1, [bad, error]),
	member(Judgement2, [good, ok]),
	!.
seriously_incompatible_judgements(Judgement1, Judgement2) :-
	member(Judgement1, [good, ok]),
	member(Judgement2, [bad, error]),
	!.
	
%----------------------------------------------------------------------

get_translation_judgement(Source, Target, Judgement) :-
	get_translation_judgement(Source, Target, user, Judgement).

get_translation_judgement(Source, Target, Module, Judgement) :-
	current_predicate(judged_translation, Module:judged_translation(_, _)),
	Module:judged_translation(Source, TranslationAlist),
	member(Target-Judgement, TranslationAlist),
	!.
% If we have a translation judgement using a different context or no context, use that.
% Slightly risky, so should print out a warning here - fix this some time.
get_translation_judgement(Source+_Context, Target, Module, Judgement) :-
	current_predicate(judged_translation, Module:judged_translation(_, _)),
	(   Module:judged_translation(Source+_OtherContext, TranslationAlist) ;
	    Module:judged_translation(Source, TranslationAlist)
	),
	member(Target-Judgement, TranslationAlist),
	!.
get_translation_judgement(_Source, _Target, _Module, unknown).

%----------------------------------------------------------------------

get_good_or_ok_translation_for_source(Source, Target) :-
	current_predicate(judged_translation, user:judged_translation(_, _)),
	user:judged_translation(Source, TranslationAlist),
	member(Target-Judgement, TranslationAlist),
	member(Judgement, [good, ok]),
	!.
%This clause seems too risky - produces strange output for ellipsis corpus
%get_good_or_ok_translation_for_source(Source+_Context, Target) :-
%	current_predicate(judged_translation, user:judged_translation(_, _)),
%	(   user:judged_translation(Source+_OtherContext, TranslationAlist) ;
%	    user:judged_translation(Source, TranslationAlist)
%	),
%	member(Target-Judgement, TranslationAlist),
%	member(Judgement, [good, ok]),
%	!.	

%----------------------------------------------------------------------

atom_to_all_parses(Atom, AllParses) :-
	% We weren't able to load the timeout library, or we don't want to use timeouts
	timeouts_unavailable,
	!,
	atom_to_all_parses1(Atom, AllParses).

atom_to_all_parses(Atom, AllParses) :-
	analysis_time_limit(TimeLimit),
	TimeLimitInMilliseconds is integer( 1000 * TimeLimit ),
	
	time_out(atom_to_all_parses1(Atom, AllParses),
		 TimeLimitInMilliseconds,
		 Result),
	
	(   Result = time_out ->
	    
	    format2error('~N~n*** Error: time out (~d seconds) when trying to parse "~w"~n',
			 [TimeLimit, Atom]),
	    fail
	;

	    true
	).	    

atom_to_all_parses1(Atom, AllParses) :-
	findall(Parse, user:atom_to_parse_using_current_parser(Atom, '.MAIN', Parse), AllParses0),
	safe_remove_duplicates_preserving_order(AllParses0, AllParses),
	!.

representation_to_all_surface_words(Representation, PossWords) :-
	representation_to_all_surface_words(Representation, PossWords, _TaggedWords).

representation_to_all_surface_words(Representation, PossWords, TaggedWords) :-
	generate_surface_words(Representation, PreferredGeneratedWords, OtherGeneratedWords, TaggedWords),
	PossWords = [PreferredGeneratedWords | OtherGeneratedWords],
	!.

%----------------------------------------------------------------------

print_parse_time_report_for_translation_result_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	length(List, N),
	format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	findall(ParseTime-Source,
		(   member(translation(Source0, _Target, Info, _Judgement), List),
		    (   atom(Source0),
			Source = Source0
		    ;
			Source0 = Source+_Context
		    ),
		    member(parse_time=ParseTime, Info)
		),
		Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, SortedPairs1),
	print_parse_time_report_for_translation_result_file1(SortedPairs1, 0.0-TotalTime),
	length(SortedPairs1, N1),
	(   N1 > 0 ->
	    AverageTime is TotalTime / N1,
	    format('~N-------------------------~n', []),
	    format('~N~2f~8|(Average)~n', [AverageTime])
	;
	    otherwise ->
	    true
	).

print_parse_time_report_for_translation_result_file1([], Time-Time).
print_parse_time_report_for_translation_result_file1([Time-Source | R], In-Out) :-
	format('~N~2f~8|~w~n', [Time, Source]),
	Next is In + Time,
	!,
	print_parse_time_report_for_translation_result_file1(R, Next-Out).

%----------------------------------------------------------------------

print_timing_summary_for_translation_result_file(File) :-
	safe_absolute_file_name(File, AbsFile),
	prolog_file_to_list(AbsFile, List),
	%length(List, N),
	%format('~N--- Read file (~d records) ~w~n', [N, AbsFile]),
	format('~NAverage times for processing phases (secs):~N', []),
	print_timing_for_translation_result_list(parse_time, 'Parsing', List),
	print_timing_for_translation_result_list(transfer_to_source_discourse_time, 'To source discourse', List),
	print_timing_for_translation_result_list(resolution_time, 'Resolution', List),
	print_timing_for_translation_result_list(transfer_to_interlingua_time, 'To interlingua', List),
	print_timing_for_translation_result_list(transfer_from_interlingua_time, 'From interlingua', List),
	print_timing_for_translation_result_list(generation_time, 'Generation', List),
	format('~N~n', []),
	!.
print_timing_summary_for_translation_result_file(File) :-
	format2error('~N*** Error: bad call: ~w~n', [print_timing_summary_for_translation_result_file(File)]),
	fail.

print_timing_for_translation_result_list(Key, PrintTag, List) :-
	findall(Time,
		(   member(translation(_Source, _Target, Info, _Judgement), List),
		    member(Key=Time, Info)
		),
		Times),
	(   Times = [] ->
	    AvTime = 0.0
	;
	    otherwise ->
	    safe_sum_list(Times, TotalTime),
	    length(Times, NTimes),
	    AvTime is TotalTime / NTimes
	),
	format('~N~w~24|~2f~n', [PrintTag, AvTime]),
	!.
print_timing_for_translation_result_list(Key, PrintTag, _List) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_timing_for_translation_result_list(Key, PrintTag, '(list)')]),
	fail.

%----------------------------------------------------------------------

store_target_vocabulary_for_corpus_output(File) :-
	retractall(regulus_preds:target_vocabulary_item(_)),
	safe_absolute_file_name(File, AbsFile),
	(   safe_file_exists(AbsFile) ->
	    prolog_file_to_list(AbsFile, List),
	    length(List, N),
	    format('~N--- Storing target vocabulary on corpus output file (~d records) ~w~n', [N, AbsFile]),
	    store_target_vocabulary_for_corpus_output_list(List, 1, 0-NWords),
	    format('~N--- Stored ~d surface words~n', [NWords])
	;
	    otherwise ->
	    format('~N*** Error: unable to find file ~w~n', [AbsFile])
	),
	!.

store_target_vocabulary_for_corpus_output_list([], _I, N-N).
store_target_vocabulary_for_corpus_output_list([F | R], I, NIn-NOut) :-
	store_target_vocabulary_for_corpus_output_record(F, NIn-NNext),
	format('.', []),
	(   ( 0 is I mod 100, I > 0 ) ->
	    
	    format(' (~d) ~n', [I]),
	    flush_output(user)
	;
	    true
	),
	I1 is I + 1,
	!,
	store_target_vocabulary_for_corpus_output_list(R, I1, NNext-NOut).

store_target_vocabulary_for_corpus_output_record(Record, NIn-NOut) :-
	Record = translation(_Source, Target, _List, _Judgement),
	atom(Target),
	store_target_vocabulary_for_corpus_output_atom(Target, NIn-NOut),
	!.
store_target_vocabulary_for_corpus_output_record(_Record, NIn-NIn).

store_target_vocabulary_for_corpus_output_atom(TargetAtom, NIn-NOut) :-
	split_atom_into_words(TargetAtom, TargetWords),
	store_target_vocabulary_for_corpus_output_words(TargetWords, NIn-NOut).

store_target_vocabulary_for_corpus_output_words([], NIn-NIn).
store_target_vocabulary_for_corpus_output_words([F | R], NIn-NOut) :-
	store_target_vocabulary_for_corpus_output_word(F, NIn-NNext),
	!,
	store_target_vocabulary_for_corpus_output_words(R, NNext-NOut).

store_target_vocabulary_for_corpus_output_word(Word, NIn-NOut) :-
	(   regulus_preds:target_vocabulary_item(Word) ->
	    NOut is NIn
	;
	    assertz(regulus_preds:target_vocabulary_item(Word)),
	    NOut is NIn + 1
	).

%----------------------------------------------------------------------

check_backtranslation_on_corpus_output(File) :-
	safe_absolute_file_name(File, AbsFile),
	(   safe_file_exists(AbsFile) ->
	    prolog_file_to_list(AbsFile, List),
	    length(List, N),
	    format('~N--- Checking backtranslation on corpus output file (~d records) ~w~n', [N, AbsFile]),
	    check_backtranslation_on_corpus_output_list(List, 1)
	;
	    otherwise ->
	    format('~N*** Error: unable to find file ~w~n', [AbsFile])
	),
	!.

check_backtranslation_on_corpus_output_list([], _I).
check_backtranslation_on_corpus_output_list([F | R], I) :-
	check_backtranslation_on_corpus_output_record(F),
	format('.', []),
	(   ( 0 is I mod 100, I > 0 ) ->
	    
	    format(' (~d) ~n', [I]),
	    flush_output(user)
	;
	    true
	),
	I1 is I + 1,
	!,
	check_backtranslation_on_corpus_output_list(R, I1).

/*

translation(
    'could i have a hot chocolate please',
    'i would like a hot chocolate',
    [n_parses=1,parse_time=0.0,source_representation=[agent=[pronoun,i],null=[action,have],null=[modal,could],null=[politeness,please],null=[utterance_type,ynq],null=[voice,active],object=[drink,hot_chocolate]],transfer_to_source_discourse_time=0.0,source_discourse=[null=[utterance_type,ynq],null=[voice,active],null=[modal,could],agent=[pronoun,i],null=[action,have],object=[drink,hot_chocolate],null=[politeness,please]],resolved_source_discourse=[null=[utterance_type,ynq],null=[voice,active],null=[modal,could],agent=[pronoun,i],null=[action,have],object=[drink,hot_chocolate],null=[politeness,please]],resolution_processing=trivial,resolution_time=0.0,transfer_to_interlingua_time=0.0,interlingua=[arg2=[drink,hot_chocolate],null=[politeness,polite],null=[utterance_type,request]],interlingua_surface='POLITE ASK-FOR hot-chocolate',interlingua_checking_time=0.0,transfer_from_interlingua_time=0.0,target_representation=[_348818=[action,like],object=[drink,hot_chocolate],_348800=[modal,would],_348791=[pronoun,i],_348782=[utterance_type,dcl],_348809=[voice,active]],instantiated_target=[null=[action,like],object=[drink,hot_chocolate],null=[modal,would],agent=[pronoun,i],null=[utterance_type,dcl],null=[voice,active]],n_generations=2,generation_time=0.0,other_translations=[[i,would,like,a,hot,chocolate]],tagged_translations=[['*start*',i/np,would/v,like/v,[a,hot,chocolate]/np,'*end*'],['*start*',i/np,would/v,like/v,[a,hot,chocolate]/np,'*end*']]],
    good
  ).

*/

check_backtranslation_on_corpus_output_record(Record) :-
	Record = translation(Source, Target0, List, _Judgement),
	(   member(recognition_orthography_words=RecognitionOrthographyWords, List) ->
	    join_with_spaces(RecognitionOrthographyWords, Target)
	;
	    otherwise ->
	    Target = Target0
	),
	member(interlingua_surface=SurfaceInterlingua, List),
	check_backtranslation_on_corpus_output_record1(Target, SurfaceInterlingua, Source),
	!.
check_backtranslation_on_corpus_output_record(_Record).

check_backtranslation_on_corpus_output_record1(Target, _CorrectSurfaceInterlingua, _OriginalSource) :-
	\+ atom(Target),
	!.
check_backtranslation_on_corpus_output_record1(Target, CorrectSurfaceInterlingua, OriginalSource) :-
	(   user:atom_to_parse_using_current_parser(Target, '.MAIN', SourceLF) ->
	    null_discourse_context(ContextIn),
	    transfer_representation_to_interlingua_and_surface_interlingua_using_context(SourceLF, ContextIn, _ContextOut, _Interlingua, SurfaceInterlingua, _Tree),
	    (   SurfaceInterlingua = CorrectSurfaceInterlingua ->
		true
	    ;
		otherwise ->
		format('~N*** Error: different interlingua "~w"~n', [SurfaceInterlingua]),
		format('~N***     arising from interlingua "~w"~n', [CorrectSurfaceInterlingua]),
		format('~N***             original source: "~w"~n', [OriginalSource]),
		format('~N***           translated source: "~w"~n', [Target])
	    )
	;
	    otherwise ->
	    format('~N***       Error: unable to parse "~w"~n', [Target]),
	    format('~N***     arising from interlingua "~w"~n', [CorrectSurfaceInterlingua]),
	    format('~N***                   and source "~w"~n', [OriginalSource])
	),
	!.	    

%----------------------------------------------------------------------

qa_file_to_q_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	prolog_file_to_list(AbsInFile, InList),
	length(InList, N),
	format('~N~n--- Read QA file (~d records) ~w~n', [N, AbsInFile]),
	qa_list_to_q_list(InList, OutList),
	length(OutList, N1),
	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written Q file (~d records) ~w~n', [N1, AbsOutFile]).
qa_file_to_q_file(InFile, OutFile) :-
	format2error('~N*** Error: unable to convert QA file ~w into Q file ~w~n', [InFile, OutFile]),
	!,
	fail.

qa_list_to_q_list([], []).
qa_list_to_q_list([F | R], [F1 | R1]) :-
	qa_record_to_q_sent_record(F, F1),
	!,
	qa_list_to_q_list(R, R1).

qa_file_and_translation_output_to_a_file(QAFile, OutputFile, AFile) :-
	safe_absolute_file_name(QAFile, AbsQAFile),
	safe_absolute_file_name(OutputFile, AbsOutputFile),
	safe_absolute_file_name(AFile, AbsAFile),
	
	prolog_file_to_list(AbsQAFile, QAList),
	length(QAList, NQA),
	format('~N~n--- Read QA file (~d records) ~w~n', [NQA, AbsQAFile]),
	
	prolog_file_to_list(AbsOutputFile, OutputList),
	length(OutputList, NOut),
	format('~N~n--- Read translation output file (~d records) ~w~n', [NOut, AbsOutputFile]),

	(   NQA = NOut ->
	    
	    qa_list_and_translation_output_list_to_a_file(QAList, OutputList, AbsAFile) ;
	    
	    format2error('~N~n*** Error: different number of records in files~n', []),
	    fail
	).
qa_file_and_translation_output_to_a_file(QAFile, OutputFile, AFile) :-
	format2error('~N*** Error: unable to convert QA file ~w and translation output file ~w into A file ~w~n',
		     [QAFile, OutputFile, AFile]),
	!,
	fail.

qa_list_and_translation_output_list_to_a_file(QAList, OutputList, AbsAFile) :-
	qa_list_and_translation_output_list_to_a_list(QAList, OutputList, AList),
	length(AList, N),
	list_to_prolog_file(AList, AbsAFile),
	format('~N--- Written A file (~d records) ~w~n', [N, AbsAFile]).

% Definition changed to ignore no_answer records
qa_list_and_translation_output_list_to_a_list([], [], []).
qa_list_and_translation_output_list_to_a_list([FQA | R], [_FOut | ROut], RA) :-
	qa_record_to_a_sent_record(FQA, FQA1),
	FQA1 = sent('no_answer'),
	!,
	qa_list_and_translation_output_list_to_a_list(R, ROut, RA).
qa_list_and_translation_output_list_to_a_list([FQA | R], [FOut | ROut], [FOut1, FQA1 | RA]) :-
	qa_record_to_a_sent_record(FQA, FQA1),
	translation_output_record_to_set_discourse_record(FOut, FOut1),
	!,
	qa_list_and_translation_output_list_to_a_list(R, ROut, RA).
% End of change

qa_record_to_q_sent_record(question_and_answer(Q, _A), sent(Q)) :-
	!.
qa_record_to_q_sent_record(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [qa_record_to_q_sent_record(X, Y)]),
	fail.

qa_record_to_a_sent_record(question_and_answer(_Q, A), sent(A)) :-
	!.
qa_record_to_a_sent_record(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [qa_record_to_a_sent_record(X, Y)]),
	fail.
 
translation_output_record_to_set_discourse_record(TransRecord, SetDiscourseRecord) :-
	TransRecord = translation(Source, Target, Info, _Judgement),
	(   member(instantiated_target=TargetRepresentation, Info) ->
	    true ;
	    TargetRepresentation = []
	),
	SetDiscourseRecord = set_discourse_context(Source, Target, TargetRepresentation),
	!.

%----------------------------------------------------------------------

convert_interlingua_in_file(FileIn, FileOut, Type) :-
	(   member(Type, [judgements(source), judgements(target), interlingua_corpus, plain_text, csv(_Columns)]) ->
	    true
	;
	    format('~N*** Error: bad call: ~w: last arg must be one of [judgements(source), judgements(target), interlingua_corpus, plain_text, csv(Columns)]~n',
		   [convert_interlingua_in_file(FileIn, FileOut, Type)]),
	    fail
	),
	safe_absolute_file_name(FileIn, AbsFileIn),
	safe_absolute_file_name(FileOut, AbsFileOut),

	(   safe_file_exists(AbsFileIn) ->
	    (   ( Type = plain_text ; Type = csv(_Columns) ) ->
		read_file_to_atom_list(AbsFileIn, ListIn)
	    ;
		otherwise ->
		prolog_file_to_list(AbsFileIn, ListIn)
	    ),
		
	    length(ListIn, InN),
	    format('~N--- Read file (~d records) ~w~n', [InN, AbsFileIn])
	;
	    otherwise ->
	    ListIn = [],
	    format('~N--- Cannot find file ~w, assuming empty~n', [AbsFileIn])
	),
	
	convert_interlingua_in_list(ListIn, ListOut, Type, 1),
	
	(   ( Type = plain_text ; Type = csv(_Columns) ) ->
	    write_atom_list_to_file(ListOut, AbsFileOut)
	;
	    otherwise ->
	    list_to_prolog_file_prettyprint(ListOut, AbsFileOut)
	),
	length(ListOut, OutN),
	format('~N--- Written converted file (~d records) ~w~n~n', [OutN, AbsFileOut]),
	!.

convert_interlingua_in_list([], [], _Type, _I).
convert_interlingua_in_list([F | R], [F1 | R1], Type, I) :-
	convert_interlingua_in_record(F, F1, Type),
	format(user, '.', []),
	(   ( 0 is I mod 100, I > 0 ) ->
	    
	    format(' (~d) ~n', [I]),
	    flush_output(user)
	;
	    true
	),
	I1 is I + 1,
	!,
	convert_interlingua_in_list(R, R1, Type, I1).

convert_interlingua_in_record(Source, Source1, plain_text) :-
	convert_interlingua(Source, Source1),
	!.
convert_interlingua_in_record(CSVRecord, CSVRecord1, csv(Columns)) :-
	convert_interlingua_in_csv_record(CSVRecord, CSVRecord1, csv(Columns)),
	!.
convert_interlingua_in_record(judged_translation(Source, Judgements),
			      judged_translation(Source1, Judgements),
			      judgements(source)) :-
	convert_interlingua(Source, Source1),
	!.
convert_interlingua_in_record(judged_translation(Source, Judgements),
			      judged_translation(Source, Judgements1),
			      judgements(target)) :-
	convert_interlingua_in_judgements(Judgements, Judgements1),
	!.
convert_interlingua_in_record(interlingua_item(InterlinguaSurface, Interlingua, List),
			      interlingua_item(InterlinguaSurface1, Interlingua, List),
			      interlingua_corpus) :-
	convert_interlingua(InterlinguaSurface, InterlinguaSurface1),
	!.
convert_interlingua_in_record(X, Y, Z) :-
	 format('~N*** Error: bad call: ~w~n',
		   [convert_interlingua_in_record(X, Y, Z)]),
	 fail.

convert_interlingua_in_csv_record(CSVRecord, CSVRecord1, csv(Columns)) :-
	atom(CSVRecord),
	split_atom_into_words(CSVRecord, 0',, Components),
	convert_interlingua_in_csv_record_components(Columns, Components, Components1),
	append_atoms(Components1, 0',, CSVRecord1),
	!.
convert_interlingua_in_csv_record(CSVRecord, CSVRecord1, Columns) :-
	format('~N*** Error: bad call: ~w~n',
	       [convert_interlingua_in_csv_record(CSVRecord, CSVRecord1, Columns)]),
	fail.

convert_interlingua_in_csv_record_components([], Components, Components).
convert_interlingua_in_csv_record_components([F | R], ComponentsIn, ComponentsOut) :-
	convert_interlingua_in_csv_record_component(F, ComponentsIn, ComponentsNext),
	convert_interlingua_in_csv_record_components(R, ComponentsNext, ComponentsOut).

convert_interlingua_in_csv_record_component(1, [Interlingua | R], [Interlingua1 | R]) :-
	convert_interlingua(Interlingua, Interlingua1),
	!.
convert_interlingua_in_csv_record_component(I, [F | R], [F | R1]) :-
	I > 1,
	I1 is I - 1,
	!,
	convert_interlingua_in_csv_record_component(I1, R, R1).

convert_interlingua_in_judgements([], []).
convert_interlingua_in_judgements([Atom-Judgement | R], [Atom1-Judgement | R1]) :-
	convert_interlingua(Atom, Atom1),
	!,
	convert_interlingua_in_judgements(R, R1).

convert_interlingua(Atom, Atom1) :-
	null_discourse_context(ContextIn),
	safe_parse_transfer_and_generate(Atom, Translation, ContextIn-_ContextOut, _Stats, interlingua),
	(   bad_interlingua_surface(Translation) ->
	    Atom1 = bad_interlingua
	;
	    otherwise ->
	    Atom1 = Translation
	),
	!.

%----------------------------------------------------------------------

update_translation_judgements(ResultsFile, JudgementsFile, NNew, NChanged) :-
	create_assoc_generic_from_judgements_in_prolog_database(AssocIn),
	prolog_file_to_list(ResultsFile, ResultsList),
	update_assoc_generic_from_results_list(ResultsList, AssocIn, AssocOut, 0-NNew, 0-NChanged),
	create_new_judgements_file_from_assoc_generic(AssocOut, JudgementsFile).

create_assoc_generic_from_judgements_in_prolog_database(Assoc) :-
	current_predicate(judged_translation, user:judged_translation(_, _)),
	findall(Source-TranslationAlist, user:judged_translation(Source, TranslationAlist), KeyValList),
	list_to_assoc_generic(KeyValList, Assoc),
	!.
create_assoc_generic_from_judgements_in_prolog_database(Assoc) :-
	empty_assoc_generic(Assoc).

update_assoc_generic_from_results_list([], AssocIn, AssocIn, NewIn-NewIn, ChangedIn-ChangedIn).
update_assoc_generic_from_results_list([F | R], AssocIn, AssocOut, NewIn-NewOut, ChangedIn-ChangedOut) :-
	update_assoc_generic_from_results_list_item(F, AssocIn, AssocNext, NewIn-NewNext, ChangedIn-ChangedNext),
	!,
	update_assoc_generic_from_results_list(R, AssocNext, AssocOut, NewNext-NewOut, ChangedNext-ChangedOut).

update_assoc_generic_from_results_list_item(translation(Source, Target, _Stats, Judgement),
				    AssocIn, AssocOut, NewIn-NewOut, ChangedIn-ChangedOut) :-
	(   member(Judgement, ['?', error]) ->
	    AssocIn = AssocOut,
	    NewIn = NewOut,
	    ChangedIn = ChangedOut ;
	    
	    member(Judgement, [good, ok, bad]) ->
	    update_assoc_generic_from_results_list_item1(translation(Source, Target, Judgement),
						 AssocIn, AssocOut, NewIn-NewOut, ChangedIn-ChangedOut) ;

	    format2error('~N*** WARNING: unknown judgement discarded: ~w~n', [translation(Source, Target, Judgement)]),
	    AssocIn = AssocOut,
	    NewIn = NewOut,
	    ChangedIn = ChangedOut
	),
	!.
update_assoc_generic_from_results_list_item(Other, AssocIn, AssocOut, New, Changed) :-
	format2error('~N*** Error: bad call: ~w~n',
	       [update_assoc_generic_from_results_list_item(Other, AssocIn, AssocOut, New, Changed)]),
	fail.

update_assoc_generic_from_results_list_item1(translation(Source, Target, Judgement),
				     AssocIn, AssocOut, NewIn-NewOut, ChangedIn-ChangedOut) :-
	get_assoc_generic(Source, AssocIn, TranslationAlist),
	member(Target-Judgement, TranslationAlist),
	AssocIn = AssocOut,
	NewIn = NewOut,
	ChangedIn = ChangedOut,
	!.
update_assoc_generic_from_results_list_item1(translation(Source, Target, Judgement),
				     AssocIn, AssocOut, NewIn-NewOut, ChangedIn-ChangedOut) :-
	(   get_assoc_generic(Source, AssocIn, OldTranslationAlist) ->

	    update_translation_judgement_alist(OldTranslationAlist, Target, Judgement, NewTranslationAlist,
					       NewIn-NewOut, ChangedIn-ChangedOut) ;	    
	    
	    NewTranslationAlist = [Target-Judgement],
	    NewOut is NewIn + 1,
	    ChangedIn = ChangedOut
	),
	put_assoc_generic(Source, AssocIn, NewTranslationAlist, AssocOut),
	!.
update_assoc_generic_from_results_list_item1(translation(Source, Target, Judgement), AssocIn, AssocOut, New, Changed) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_assoc_generic_from_results_list_item1(translation(Source, Target, Judgement), AssocIn, AssocOut, New, Changed)]),
	fail.

update_translation_judgement_alist(OldAlist, Target, Judgement, NewAlist,
				   NewIn-NewOut, ChangedIn-ChangedOut) :-
	(   remove_old_judgements_for_target_from_alist(OldAlist, Target, NextAlist, n-y) ->
	    
	    NewAlist = [Target-Judgement | NextAlist],
	    ChangedOut is ChangedIn + 1,
	    NewOut = NewIn ;

	    NewAlist = [Target-Judgement | OldAlist],
	    ChangedOut = ChangedIn,
	    NewOut is NewIn + 1
	),
	!.
update_translation_judgement_alist(OldAlist, Target, Judgement, NewAlist, New, Changed) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [update_translation_judgement_alist(OldAlist, Target, Judgement, NewAlist, New, Changed)]),
	fail.

remove_old_judgements_for_target_from_alist([], _Target, [], Done-Done).
remove_old_judgements_for_target_from_alist([Target-_OldJudgement | R], Target, R1, _DoneIn-DoneOut) :-
	!,
	remove_old_judgements_for_target_from_alist(R, Target, R1, y-DoneOut).
remove_old_judgements_for_target_from_alist([F | R], Target, [F | R1], DoneIn-DoneOut) :-
	!,
	remove_old_judgements_for_target_from_alist(R, Target, R1, DoneIn-DoneOut).

create_new_judgements_file_from_assoc_generic(Assoc, JudgementsFile) :-
	assoc_generic_to_list(Assoc, KeyValList),
	open_regulus_file(JudgementsFile, write, S),
	write_judgements_keyval_list_to_stream(KeyValList, S),
	close(S).

write_judgements_keyval_list_to_stream([], _S).
write_judgements_keyval_list_to_stream([F | R], S) :-
	write_judgements_keyval_list_item_to_stream(F, S),
	!,
	write_judgements_keyval_list_to_stream(R, S).

write_judgements_keyval_list_item_to_stream(Source-TranslationAlist, S) :-
	format(S, '~N~n~q.~n', [judged_translation(Source, TranslationAlist)]),
	!.
write_judgements_keyval_list_item_to_stream(Other, S) :-
	format2error('~N*** Error: bad call: ~w~n', [write_judgements_keyval_list_item_to_stream(Other, S)]),
	fail.

%----------------------------------------------------------------------

bad_recognition(_Source, Recognised) :-
	Recognised = '<rejected>',
	!.
bad_recognition(Source, Recognised) :-
	current_predicate(user:judged_recognition/3),
	user:judged_recognition(Source, Recognised, n),
	!.
% Optionally ignore context when judging recognition - this introduces a small amount of noise,
% but it's very convenient when translating into multiple target languages.
bad_recognition(Source+_Context, Recognised) :-
	current_predicate(user:judged_recognition/3),
	user:judged_recognition(Source, Recognised, n),	
	!.
bad_recognition(Source+_Context, Recognised) :-
	current_predicate(user:judged_recognition/3),
	user:judged_recognition(Source+_OtherContext, Recognised, n),
	!.

%----------------------------------------------------------------------

bad_interlingua_surface_in_list(Stats) :-
	member(interlingua_surface=Result, Stats),
	bad_interlingua_surface(Result),
	!.

bad_interlingua_surface(Atom) :-
	atom(Atom),
	atom_codes(Atom, Chars),
	append("WARNING", _, Chars),
	!.
bad_interlingua_surface(NonAtom) :-
	\+ atom(NonAtom).

%----------------------------------------------------------------------

update_recognition_judgements(TmpRecognitionJudgementsFile, RecognitionJudgementsFile) :-
	(   file_exists(RecognitionJudgementsFile) ->
	    prolog_file_to_list(RecognitionJudgementsFile, OldJudgements) ;
	    OldJudgements = []
	),
	(   file_exists(TmpRecognitionJudgementsFile) ->
	    prolog_file_to_list(TmpRecognitionJudgementsFile, NewJudgements) ;
	    NewJudgements = []
	),
	append(OldJudgements, NewJudgements, AllJudgements),
	sort(AllJudgements, AllJudgements1),
	make_recognition_judgements_consistent(AllJudgements1, AllJudgements2),
	list_to_prolog_file(AllJudgements2, RecognitionJudgementsFile),
	!.
update_recognition_judgements(TmpRecognitionJudgementsFile, RecognitionJudgementsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [update_recognition_judgements(TmpRecognitionJudgementsFile, RecognitionJudgementsFile)]),
	fail.

make_recognition_judgements_consistent([], []) :-
	!.
make_recognition_judgements_consistent([X], [X]) :-
	!.
make_recognition_judgements_consistent([judged_recognition(_From, _To, ?) | R], R1) :-
	!,
	make_recognition_judgements_consistent(R, R1).
make_recognition_judgements_consistent([judged_recognition(From, To, n), judged_recognition(From, To, y) | R],
				       [judged_recognition(From, To, n) | R1]) :-
	format2error('~N*** Warning: inconsistent translation:~n', []),
	format2error('~N~w ->', [From]),
	format2error('~N~w', [To]),
	format2error('~Nmarked both \'y\' and \'n\'. Discarding the positive judgement.~n', []),
	!,
	make_recognition_judgements_consistent(R, R1).
make_recognition_judgements_consistent([F | R], [F | R1]) :-
	!,
	make_recognition_judgements_consistent(R, R1).

%----------------------------------------------------------------------

make_tmp_recognition_judgements_file(PrologBatchrecFileWithTranscriptions,
				     TmpRecognitionJudgementsFile) :-
	prolog_file_to_list(PrologBatchrecFileWithTranscriptions, PrologBatchrecListWithTranscriptions),
	make_tmp_recognition_judgements_list(PrologBatchrecListWithTranscriptions, TmpRecognitionJudgementsList),
	write_out_recognition_judgements_list(TmpRecognitionJudgementsList, TmpRecognitionJudgementsFile),
	!.
make_tmp_recognition_judgements_file(PrologBatchrecFileWithTranscriptions, TmpRecognitionJudgementsFile) :-
	format2error('~N*** Error: bad call: ~w~n', [make_tmp_recognition_judgements_file(PrologBatchrecFileWithTranscriptions, TmpRecognitionJudgementsFile)]),
	fail.

make_tmp_recognition_judgements_list([], []) :-
	!.
make_tmp_recognition_judgements_list([F | R], [F1 | R1]) :-
	make_tmp_recognition_judgements_item(F, F1),
	!,
	make_tmp_recognition_judgements_list(R, R1).
make_tmp_recognition_judgements_list([_F | R], R1) :-
	!,
	make_tmp_recognition_judgements_list(R, R1).

make_tmp_recognition_judgements_item(Item, Item1) :-
	Item = batchrec_item(List),
	member(transcription=RefWords, List),
	member(words=RecWords, List),
	join_with_spaces(RefWords, RefWordsAtom),
	join_with_spaces(RecWords, RecWordsAtom),
	RefWordsAtom \== RecWordsAtom,
	RecWordsAtom \== '<rejected>',
	(   current_predicate(user:judged_recognition/3) ->
	    \+ user:judged_recognition(RefWordsAtom, RecWordsAtom, _Judgement) ;
	    true
	),
	Item1 = ref_rec_pair(RefWordsAtom, RecWordsAtom),
	!.

%----------------------------------------------------------------------

write_out_recognition_judgements_list(List, File) :-
	absolute_file_name(File, AbsFile),
	open_regulus_file(AbsFile, write, S),
	print_recognition_judgements_instructions(S),
	write_out_recognition_judgements_list1(List, S),
	close(S),
	format('~NBlank recognition judgements written to ~w~n', [AbsFile]),
	!.

print_recognition_judgements_instructions(S) :-
	format(S, '~N% Recognition results to judge.~n', []),
	format(S, '~N%~n', []),	
	format(S, '~N% The format of a record is as follows:~n', []),	
	format(S, '~N%~n', []),	
	format(S, '~N% judged_recognition(~n', []),	
	format(S, '~N%     <what the person said>,~n', []),	
	format(S, '~N%     <what the system heard>,~n', []),	
	format(S, '~N%     ?~n', []),	
	format(S, '~N%   ).~n', []),	
	format(S, '~N%~n', []),	
	format(S, '~N% If you think that what the system heard clearly means the same as what the person said~n', []),	
	format(S, '~N% in the context of the translation task, replace the \'?\' with a \'y\'.~n', []),
	format(S, '~N% Otherwise, replace it with a \'n\'.~n', []),
	format(S, '~N~n~n', []),	
	!.	      

write_out_recognition_judgements_list1([], _S).
write_out_recognition_judgements_list1([F | R], S) :-
	write_out_recognition_judgements_item(F, S),
	write_out_recognition_judgements_list1(R, S).

write_out_recognition_judgements_item(ref_rec_pair(RefWordsAtom, RecWordsAtom), S) :-
	format(S, '~N~njudged_recognition(~n', []),
	format(S, '~N    ~q,~n', [RefWordsAtom]),
	format(S, '~N    ~q,~n', [RecWordsAtom]),
	format(S, '~N    ?~n', []),
	format(S, '~N  ).~n', []),
	!.
write_out_recognition_judgements_item(Item, S) :-
	format2error('~N*** Error: bad call: ~w~n', [write_out_recognition_judgements_item(Item, S)]),
	fail.

