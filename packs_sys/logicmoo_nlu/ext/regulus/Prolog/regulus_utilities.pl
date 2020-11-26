% regulus_utilities.pl

% Regulus utility predicates

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(regulus_utilities,
	[check_config_file_is_loaded/0,
	 get_config_item_or_complain/2,
	 number_or_compositional_number/1,
	 is_regulus_declaration/1,
	 is_transfer_declaration/1,
	 is_orthography_declaration/1,
	 is_collocation_declaration/1,
	 is_lf_pattern_declaration/1,
	 is_lf_rewrite_declaration/1,
	 is_generic_declaration/1,
	 is_regulus_rule/1,
	 is_transfer_entry/1,
	 is_orthography_entry/1,
	 is_collocation_entry/1,
	 is_lf_pattern_entry/1,
	 is_lf_rewrite_entry/1,
	 is_generic_entry/1,
	 is_non_lexical_regulus_rule/1,
	 is_lexical_regulus_rule/1,
	 is_non_lexical_transfer_entry/1,
	 is_lexical_transfer_entry/1,
	 flatten_transfer_representation/2,
	 make_transfer_representation_canonical/2,
	 unpack_nested_internal_representation_if_necessary/2,
	 make_transfer_representation_canonical_and_unpack_if_necessary/2,
	 order_transfer_rule_lhs_and_extract_first_element/3,
	 remove_judgements_from_record/2,
	 is_valid_feat_val_list/2,
	 is_valid_feat_val/2,
	 is_valid_sem_val/2,
	 is_feat_val_list/1,
	 is_non_feat_val_list/1,
	 instantiate_null_sem_values/1,
	 feats_and_vals_to_feat_vals/3,
	 non_ignored_synfeats_for_cat/2,
	 get_value_space_for_feat/2,
	 constraint_on_feat_allows_some_but_not_all_values/2,
	 constraint_on_feat_allows_all_values/2,
	 possible_value_of_feature_consistent_with_spec/3,
	 member_of_value_space/2,
	 possible_values_for_value_space/2,
	 feature_value_compatible_with_spec/2,
	 remove_ignored_features_from_list/2,
	 remove_ignored_feat_val_pairs_from_list/2,
	 is_semantic_component/1,
	 head_cat_and_line_info_for_rule/3,
	 summarise_rule/2,
	 get_category_internal/2,
	 regulus_error/2,
	 inform_about_regulus_exception/2,
	 inform_about_line_info/1,
	 inform_about_top_level_regulus_error/1,
	 set_regulus_switch/2,
	 set_regulus_debug_level/1,
	 regulus_debug_call/2,
	 is_list_of_atom_lists/1,
	 is_list_of_simple_pairs/1,
	 using_prolog_semantics/0,
	 using_strcat_semantics/0,
	 gsl_function/1,
	 interpret_string_as_raw_lf_input/2,
	 print_form_for_syn_feats/2,
	 print_form_for_syn_feats_grounded/2,
	 parse_tree_to_tagged_word_list/2,
	 parse_tree_to_summary/2,
	 remove_lexical_info_in_parse_tree/2,
	 remove_lexical_and_line_info_in_parse_tree/2,
	 remove_lexical_and_file_info_in_parse_tree/2,
	 prettyprint_parse_tree/2,
	 prettyprint_parse_tree/1,
	 minimal_prettyprint_parse_tree/1,
	 prettyprint_transfer_trace/1,
	 init_stored_errors/0,
	 format2error/2,
	 prettyprint2error/1,
	 prettyprintq2error/1,
	 get_stored_errors/1,
	 get_stored_errors_list/1,
	 fake_sent_record/1,
	 get_sem_constants_from_sem/2,
	 frequency_preference_information_available_in_term/1,
	 rule_frequency_preference_score/2,
	 csv_annotated_corpus_to_text_corpus/3,
	 csv_annotated_corpus_to_text_corpus/2,
	 csv_annotated_corpus_to_filtered_text_corpus/3,
	 read_commands_to_keep_file/2,
	 filter_annotated_corpus_list/3,
	 text_corpus_to_sent_corpus/2,
	 text_corpus_to_sorted_vocab_file/2
     ]
).

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(ordsets)).
:- use_module(library(system)).
'SICSTUS4ONLY'( ( :- use_module(library(file_systems)) ) ).
'SICSTUS3/4'( ( :- use_module(library(charsio)) ), ( :- use_module('$REGULUS/PrologLib/compatibility_charsio') ) ).

:- meta_predicate regulus_debug_call(+, :).

%---------------------------------------------------------------

check_config_file_is_loaded :-
	current_predicate(user:regulus_config/2),
	!.
check_config_file_is_loaded :-
	format('~N*** Warning: no config file is loaded~n', []),
	fail.

%---------------------------------------------------------------

get_config_item_or_complain(Key, Value) :-
	check_config_file_is_loaded,
	(   user:regulus_config(Key, Value) ->
	    true
	;
	    otherwise ->
	    make_ground(Key),
	    format('~N*** Error: no config file item found matching "~w"~n', [Key]),
	    fail
	).

%---------------------------------------------------------------

is_generic_declaration(macro(_, _)) :- !.

is_generic_entry(T) :-
	nonvar(T).

%---------------------------------------------------------------

is_lf_pattern_declaration(macro(_, _)) :- !.

is_lf_pattern_entry((Head :- _Body)) :-
	is_lf_pattern_head(Head),
	!.
is_lf_pattern_entry(Head) :-
	is_lf_pattern_head(Head),
	!.

is_lf_pattern_head(Head) :-
	compound(Head),
	functor(Head, lf_pattern, N),
	member(N, [2, 3]).
is_lf_pattern_head(Head) :-
	compound(Head),
	functor(Head, lf_boundary, N),
	member(N, [2, 3]).

%---------------------------------------------------------------

is_lf_rewrite_declaration(macro(_, _)) :- !.

is_lf_rewrite_entry((Head :- _Body)) :-
	is_lf_rewrite_head(Head),
	!.
is_lf_rewrite_entry(Head) :-
	is_lf_rewrite_head(Head),
	!.

is_lf_rewrite_head(lf_rewrite(From, To)) :-
	nonvar(From),
	nonvar(To).

%---------------------------------------------------------------

is_regulus_declaration(ignore_item(_)) :- !.
is_regulus_declaration(macro(_, _)) :- !.
is_regulus_declaration(default_macro(_, _)) :- !.
is_regulus_declaration(category(_, _)) :- !.
is_regulus_declaration(feature(_, _)) :- !.
is_regulus_declaration(feature_instantiation_schedule(_)) :- !.
is_regulus_declaration(feature_value_space(_, _)) :- !.
is_regulus_declaration(ignore_feature(_)) :- !.
is_regulus_declaration(specialises(_,_,_)) :- !.
is_regulus_declaration(top_level_category(_)) :- !.
is_regulus_declaration(ignore_specialises(_,_,_)) :- !.
is_regulus_declaration(feature_value_space_substitution(_,_,_)) :- !.
is_regulus_declaration(external_grammar(_,_)) :- !.
is_regulus_declaration(lexical_feature_default(_,_)) :- !.

%---------------------------------------------------------------

number_or_compositional_number(N) :-
	number(N),
	!.
number_or_compositional_number(Term) :-
	nonvar(Term),
	Term = [Operator, N1, N2],
	id_member(Operator, [plus, multiply, subtract, divide]),
	number_or_compositional_number(N1),
	number_or_compositional_number(N2),
	!.

%---------------------------------------------------------------

is_transfer_declaration(ignore_item(_)) :- !.
is_transfer_declaration(macro(_, _)) :- !.

%---------------------------------------------------------------

is_orthography_declaration(letter_class(ClassId, Letters)) :-
	atom(ClassId),
	atom_codes(ClassId, SingleLetterChars),
	length(SingleLetterChars, 1),
	
	ground(Letters),
	%is_prolog_string(Letters).
	is_list_of_non_negative_integers(Letters).

%---------------------------------------------------------------

is_collocation_declaration(collocation_macro(MacroId, Alternatives)) :-
	(  atom(MacroId) ; compound(MacroId) ),
	
	ground(Alternatives),
	is_list_of_prolog_strings(Alternatives).

is_list_of_prolog_strings([]).
is_list_of_prolog_strings([F | R]) :-
	is_prolog_string(F),
	is_list_of_prolog_strings(R).

%---------------------------------------------------------------

is_regulus_rule(Rule) :- 
	nonvar(Rule),
	Rule = (Head --> Body),
	nonvar(Head),
	(   current_predicate(user:regulus_config/2), user:regulus_config(prolog_semantics, yes)
	;
	    nonvar(Body)
	).

%---------------------------------------------------------------

% Don't do further checking at this stage, because we might have unexpanded macros
is_transfer_entry(Rule) :-
	(   safe_subsumes_chk(transfer_lexicon(_, _), Rule) ;
	    safe_subsumes_chk(reverse_transfer_lexicon(_, _), Rule) ;
	    safe_subsumes_chk(bidirectional_transfer_lexicon(_, _), Rule) ;
	    
	    safe_subsumes_chk(transfer_rule(_, _), Rule) ;
	    safe_subsumes_chk(reverse_transfer_rule(_, _), Rule) ;
	    safe_subsumes_chk(bidirectional_transfer_rule(_, _), Rule) ;

	    safe_subsumes_chk((transfer_rule(_, _) :- _), Rule) ;
	    safe_subsumes_chk((reverse_transfer_rule(_, _) :- _), Rule) ;
	    safe_subsumes_chk((bidirectional_transfer_rule(_, _) :- _), Rule) ;

	    safe_subsumes_chk(role_transfer_rule(_, _), Rule) ;
	    safe_subsumes_chk(reverse_role_transfer_rule(_, _), Rule) ;
	    safe_subsumes_chk(bidirectional_role_transfer_rule(_, _), Rule) ;

	    safe_subsumes_chk((role_transfer_rule(_, _) :- _), Rule) ;
	    safe_subsumes_chk((reverse_role_transfer_rule(_, _) :- _), Rule) ;
	    safe_subsumes_chk((bidirectional_role_transfer_rule(_, _) :- _), Rule) ;

	    safe_subsumes_chk((role_list_transfer_rule(_) :- _), Rule) 
	).

%---------------------------------------------------------------

is_orthography_entry(Rule) :-
	ground(Rule),
	Rule = orthography_rewrite(LHS, RHS),
	%is_prolog_string(LHS),
	%is_prolog_string(RHS),
	is_list_of_non_negative_integers(LHS),
	is_list_of_non_negative_integers(RHS),
	!.

%---------------------------------------------------------------

is_collocation_entry(Rule) :-
	ground(Rule),
	(   Rule = better_collocation(LHS, RHS)
	;
	    Rule = better_original_script_collocation(LHS, RHS)
	),
	is_prolog_string(LHS),
	is_prolog_string(RHS).

%---------------------------------------------------------------

is_non_lexical_regulus_rule(V) :-
	var(V),
	!,
	fail.
is_non_lexical_regulus_rule(rule(Rule, _LineInfo)) :-
	!,
	is_non_lexical_regulus_rule(Rule).
is_non_lexical_regulus_rule( (_H --> B) ) :-
	B == [],
	!.
is_non_lexical_regulus_rule( (_H --> B) ) :-
	term_contains_functor(B, ':'/2).

%---------------------------------------------------------------

is_lexical_regulus_rule(V) :-
	var(V),
	!,
	fail.
is_lexical_regulus_rule(rule(Rule, _LineInfo)) :-
	!,
	is_lexical_regulus_rule(Rule).
is_lexical_regulus_rule( (H --> B) ) :-
	\+ is_non_lexical_regulus_rule( (H --> B) ).

%---------------------------------------------------------------

is_non_lexical_transfer_entry(rule(Rule, _LineInfo)) :-
	!,
	is_non_lexical_transfer_entry(Rule).
is_non_lexical_transfer_entry((MainRule :- ContextConditions)) :-
	!,
	is_non_lexical_transfer_entry(MainRule),
	is_transfer_entry_context_conditions(ContextConditions).
is_non_lexical_transfer_entry(Rule) :-
	nonvar(Rule),
	(   Rule = transfer_rule(LHS, RHS) ;
	    Rule = reverse_transfer_rule(LHS, RHS) ;
	    Rule = bidirectional_transfer_rule(LHS, RHS) 
	),
	nonvar(LHS),
	nonvar(RHS),
	is_list(LHS),
	is_list(RHS).
is_non_lexical_transfer_entry(Rule) :-
	nonvar(Rule),
	(   Rule = role_transfer_rule(LHS, RHS) ;
	    Rule = reverse_role_transfer_rule(LHS, RHS) ;
	    Rule = bidirectional_role_transfer_rule(LHS, RHS) 
	),
	atomic(LHS),
	( atomic(RHS) ; var(RHS) ).
is_non_lexical_transfer_entry(Rule) :-
	nonvar(Rule),
	Rule = role_list_transfer_rule(Roles),
	is_list(Roles).

%---------------------------------------------------------------

is_lexical_transfer_entry(rule(Rule, _LineInfo)) :-
	!,
	is_lexical_transfer_entry(Rule).
is_lexical_transfer_entry(Rule) :-
	nonvar(Rule),
	(   Rule = transfer_lexicon(LHS, RHS) ;
	    Rule = reverse_transfer_lexicon(LHS, RHS) ;
	    Rule = bidirectional_transfer_lexicon(LHS, RHS)
	),
	nonvar(LHS),
	nonvar(RHS).

%---------------------------------------------------------------

flatten_transfer_representation([], []) :-
	!.
flatten_transfer_representation([F | R], Out) :-
	is_recursive_list_of_pairs_or_transfer_variables(F),
	flatten_transfer_representation(F, F1),
	flatten_transfer_representation(R, R1),
	append(F1, R1, Out),
	!.
flatten_transfer_representation([Role=F | R], Out) :-
	is_recursive_list_of_pairs_or_transfer_variables(F),
	flatten_transfer_representation(F, F1),
	flatten_transfer_representation(R, R1),
	add_role_to_pairs(F1, Role, F2),
	append(F2, R1, Out),
	!.
flatten_transfer_representation([F | R], [F1 | R1]) :-
	F = [clause, Clause],
	flatten_transfer_representation(Clause, Clause1),
	F1 = [clause, Clause1],
	flatten_transfer_representation(R, R1),
	!.
flatten_transfer_representation([F | R], [F1 | R1]) :-
	F = (Role=[clause, Clause]),
	atomic(Role),
	flatten_transfer_representation(Clause, Clause1),
	F1 = (Role=[clause, Clause1]),
	flatten_transfer_representation(R, R1),
	!.
flatten_transfer_representation([F | R], [F | R1]) :-
	flatten_transfer_representation(R, R1),
	!.
flatten_transfer_representation(In, Out) :-
	regulus_error('~NBad call: ~w~n', [flatten_transfer_representation(In, Out)]).

is_recursive_list_of_pairs_or_transfer_variables(List) :-
	is_list(List),
	is_recursive_list_of_pairs_or_transfer_variables1(List).

is_recursive_list_of_pairs_or_transfer_variables1([]).
is_recursive_list_of_pairs_or_transfer_variables1([F | R]) :-
	is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(F),
	!,
	is_recursive_list_of_pairs_or_transfer_variables1(R).

is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(List) :-
	is_recursive_list_of_pairs_or_transfer_variables(List),
	!.
is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(Pair) :-
	compound(Pair),
	Pair = [Key, _Value],
	atomic(Key),
	!.
is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(RoleMarkedPair) :-
	compound(RoleMarkedPair),
	RoleMarkedPair = (Role=[Key, _Value]),
	atomic(Role),
	atomic(Key),
	!.
is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(Tag:Pair) :-
	atomic(Tag),
	compound(Pair),
	Pair = [Key, _Value],
	atomic(Key),
	!.
is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(RoleMarkedPair) :-
	compound(RoleMarkedPair),
	RoleMarkedPair = (Role=TransferVar),
	atomic(Role),
	compound(TransferVar),
	functor(TransferVar, tr, _),
	!.
is_pair_or_transfer_variable_or_list_of_pairs_or_transfer_variables(TransferVar) :-
	compound(TransferVar),
	functor(TransferVar, tr, _),
	!.

add_role_to_pairs([], _Role, []).
add_role_to_pairs([F | R], Role, [F1 | R1]) :-
	add_role_to_pair(F, Role, F1),
	!,
	add_role_to_pairs(R, Role, R1).

add_role_to_pair(ExistingRole=Val, _Role, ExistingRole=Val) :-
	!.
add_role_to_pair(Val, Role, Role=Val).

%---------------------------------------------------------------

make_transfer_representation_canonical(From, To) :-
	(   user:regulus_config(nested_semantics, yes) ->
	    Mode = nested_semantics
	;
	    otherwise ->
	    Mode = normal_semantics
	),
	make_transfer_representation_canonical(From, To, Mode).

make_transfer_representation_canonical(Var, Var, _Mode) :-
	var(Var).
make_transfer_representation_canonical((Key = ValIn), (Key = ValOut), Mode) :-
	make_transfer_representation_canonical(ValIn, ValOut, Mode),
	!.
make_transfer_representation_canonical(RepresentationIn, RepresentationOut, Mode) :-
	make_transfer_representation_elements_canonical(RepresentationIn, RepresentationNext1, Mode),
	%safe_remove_duplicates(RepresentationNext1, RepresentationNext2),
	remove_duplicates_and_merge(RepresentationNext1, RepresentationNext2),
	sort_transfer_elements(RepresentationNext2, RepresentationOut),
	!.
make_transfer_representation_canonical(RepresentationIn, RepresentationIn, _Mode) :-
	!.
make_transfer_representation_canonical(RepresentationIn, RepresentationOut, Mode) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [make_transfer_representation_canonical(RepresentationIn, RepresentationOut, Mode)]),
	!.
	
make_transfer_representation_elements_canonical([], [], _Mode).
make_transfer_representation_elements_canonical([F | R], [F1 | R1], Mode) :-
	make_transfer_representation_element_canonical(F, F1, Mode),
	!,
	make_transfer_representation_elements_canonical(R, R1, Mode).

make_transfer_representation_element_canonical(Var, Var, _Mode) :-
	var(Var),
	!.
make_transfer_representation_element_canonical(A, A, _Mode) :-
	atomic(A),
	!.
make_transfer_representation_element_canonical([clause, Var], [clause, Var], _Mode) :-
	var(Var),
	!.
make_transfer_representation_element_canonical([clause, RepresentationIn], [clause, RepresentationOut], Mode) :-
	make_transfer_representation_canonical(RepresentationIn, RepresentationOut, Mode),
	!.
make_transfer_representation_element_canonical(Role=Var, Role=Var, _Mode) :-
	var(Var),
	!.
make_transfer_representation_element_canonical(Role=[clause, RepresentationIn], Role=[clause, RepresentationOut], Mode) :-
	make_transfer_representation_canonical(RepresentationIn, RepresentationOut, Mode),
	!.
make_transfer_representation_element_canonical(RepresentationIn, RepresentationIn, nested_semantics) :-
	is_simple_pair(RepresentationIn),
	!.
make_transfer_representation_element_canonical(RepresentationIn, [clause, RepresentationOut0], nested_semantics) :-
	is_list(RepresentationIn),
	make_transfer_representation_elements_canonical(RepresentationIn, RepresentationOut0, nested_semantics),
	!.
make_transfer_representation_element_canonical(RepresentationIn, RepresentationIn, _Mode).

remove_duplicates_and_merge([], []).
remove_duplicates_and_merge([F | R], R1) :-
	member(F1, R),
	mergeable_transfer_representation_elements(F, F1),
	!,
	remove_duplicates_and_merge(R, R1).
remove_duplicates_and_merge([F | R], [F | R1]) :-
	!,
	remove_duplicates_and_merge(R, R1).

mergeable_transfer_representation_elements(Role1=Elt1, Role2=Elt2) :-
	!,
	Role1 == Role2,
	Elt1 = Elt2.
mergeable_transfer_representation_elements(Elt1, Elt2) :-
	Elt1 = Elt2.

%---------------------------------------------------------------

unpack_nested_internal_representation_if_necessary(In, Out) :-
	(   user:regulus_config(nested_semantics, yes) ->
	    unpack_nested_internal_representation(In, Out)
	;
	    otherwise ->
	    In = Out
	).

unpack_nested_internal_representation(Var, Var) :-
	var(Var).
unpack_nested_internal_representation(A, A) :-
	atomic(A),
	!.
unpack_nested_internal_representation((Key=Val), (Key=Val1)) :-
	unpack_nested_internal_representation(Val, Val1),
	!.
unpack_nested_internal_representation([clause, Body], Body1) :-
	unpack_nested_internal_representation(Body, Body1),
	!.
unpack_nested_internal_representation([F | R], [F1 | R1]) :-
	unpack_nested_internal_representation(F, F1),
	!,
	unpack_nested_internal_representation(R, R1).

%----------------------------------------------------------------------

make_transfer_representation_canonical_and_unpack_if_necessary(In, Out) :-
	make_transfer_representation_canonical(In, Next),
	unpack_nested_internal_representation_if_necessary(Next, Out).

%----------------------------------------------------------------------

% order_transfer_rule_lhs_and_extract_first_element(+LIn, -LOut, -FirstElt)

order_transfer_rule_lhs_and_extract_first_element(LIn, LOut, FirstElt) :-
	pair_elements_with_patterns_and_order_clauses(LIn, LPairs),
	keysort(LPairs, SortedPairs),
	SortedPairs = [FirstElt-_ | _],
	unkey_list(SortedPairs, LOut0),
	safe_remove_duplicates_preserving_order(LOut0, LOut),
	!.
order_transfer_rule_lhs_and_extract_first_element(LIn, LOut, FirstElt) :-
	format('~N*** ERROR: bad call: ~w~n',
	       [order_transfer_rule_lhs_and_extract_first_element(LIn, LOut, FirstElt)]),
	fail.

%----------------------------------------------------------------------

sort_transfer_elements(In, Out) :-
	(   is_role_marked_form(In) ->
	    sort_role_marked_transfer_elements(In, Out)
	;
	    otherwise ->
	    list_to_ord_set(In, Out)
	).

sort_role_marked_transfer_elements(In, Out) :-
	pair_elements_with_patterns_and_order_clauses(In, Pairs),
	keysort(Pairs, SortedPairs),
	unkey_list(SortedPairs, Out).
	
is_role_marked_form(Form) :-
	term_contains_functor(Form, '='/2),
	!.

pair_elements_with_patterns_and_order_clauses([], []).
pair_elements_with_patterns_and_order_clauses([F | R], [Pattern-F1 | R1]) :-
	(   ( F = [clause, Clause], is_list(Clause) ) ->
	    order_transfer_rule_lhs_and_extract_first_element(Clause, Clause1, _FirstElt),
	    F1 = [clause, Clause1]
	;
	    ( F = (Role=[clause, Clause]), is_list(Clause) ) ->
	    order_transfer_rule_lhs_and_extract_first_element(Clause, Clause1, _FirstElt),
	    F1 = (Role=[clause, Clause1])
	;
	    F1 = F
	),
	pattern_for_element(F1, Pattern),
	!,
	pair_elements_with_patterns_and_order_clauses(R, R1).

pattern_for_element(Var, Var) :-
	var(Var),
	!.
pattern_for_element(_Role=Element, Pattern) :-
	!,
	pattern_for_element(Element, Pattern).
pattern_for_element(compiled_tr(Restrictions, _Var), Restrictions) :-
	!.
pattern_for_element([clause, _Body], [clause, _]) :-
	!.
pattern_for_element(Other, Other).	

%---------------------------------------------------------------

is_transfer_entry_context_conditions(ContextConditions) :-
	var(ContextConditions),
	!,
	fail.
is_transfer_entry_context_conditions(context(_Item)).
is_transfer_entry_context_conditions(context_above(_Item)).
is_transfer_entry_context_conditions(context_below(_Item)).
is_transfer_entry_context_conditions(global_context(_Item)).
is_transfer_entry_context_conditions(role_context(_Item)).
is_transfer_entry_context_conditions(number(_Item)).
is_transfer_entry_context_conditions(not(ContextConditions)) :-
	is_transfer_entry_context_conditions(( \+ ContextConditions )).
is_transfer_entry_context_conditions(( \+ ContextConditions )) :-
	is_transfer_entry_context_conditions(ContextConditions).
is_transfer_entry_context_conditions(Conjunction) :-
	compound(Conjunction),
	Conjunction =.. [AndOrOr | List],
	member(AndOrOr, [and, or]),
	is_transfer_entry_context_conditions_list(List).
is_transfer_entry_context_conditions(( ContextConditions1, ContextConditions2 )) :-
	is_transfer_entry_context_conditions(ContextConditions1),
	is_transfer_entry_context_conditions(ContextConditions2).
is_transfer_entry_context_conditions(( ContextConditions1 ; ContextConditions2 )) :-
	is_transfer_entry_context_conditions(ContextConditions1),
	is_transfer_entry_context_conditions(ContextConditions2).

is_transfer_entry_context_conditions_list([Condition]) :-
	!,
	is_transfer_entry_context_conditions(Condition).
is_transfer_entry_context_conditions_list([F | R]) :-
	is_transfer_entry_context_conditions(F),
	!,
	is_transfer_entry_context_conditions_list(R).

%---------------------------------------------------------------

remove_judgements_from_record(Var, Var) :-
	var(Var),
	!.
remove_judgements_from_record(Atom, Atom) :-
	atomic(Atom),
	!.
remove_judgements_from_record(Key=[Value, Judgement], Key=Value) :-
	atom(Judgement),
	judgement_or_similar(Judgement),
	!.
remove_judgements_from_record(Key=[Value1, Value2, Judgement], Key=[Value1, Value2]) :-
	atom(Judgement),
	judgement_or_similar(Judgement),
	!.
remove_judgements_from_record(T, T1) :-
	compound(T),
	functor(T, F, N),
	functor(T1, F, N),
	remove_judgements_from_record(N, T, T1).

remove_judgements_from_record(0, _T, _T1).
remove_judgements_from_record(I, T, T1)	:-
	I > 0,
	arg(I, T, Arg),
	arg(I, T1, Arg1),
	remove_judgements_from_record(Arg, Arg1),
	I1 is I - 1,
	!,
	remove_judgements_from_record(I1, T, T1).

judgement_or_similar(no_result).
judgement_or_similar(good).
judgement_or_similar(bad).
judgement_or_similar(ok).
judgement_or_similar('?').
judgement_or_similar('?paraphrase').
judgement_or_similar('?im').
judgement_or_similar('?dm').
judgement_or_similar('?om').
judgement_or_similar('*dont_edit_this_line*').

%---------------------------------------------------------------

% is_valid_feat_val_list(+FeatsWithVals, +AllFeats)

is_valid_feat_val_list(FeatsWithVals, AllFeats) :-
	is_list(FeatsWithVals),
	is_valid_feat_val_list1(FeatsWithVals, AllFeats).

is_valid_feat_val_list1([], _AllFeats).
is_valid_feat_val_list1([First | Rest], AllFeats) :-
	is_valid_feat_val_pair(First, AllFeats),
	!,
	is_valid_feat_val_list1(Rest, AllFeats).

is_valid_feat_val_pair(Pair, AllFeats) :-
	nonvar(Pair),
	Pair = (F = V),
	atom(F),
	is_valid_feat(F, AllFeats),
	is_valid_feat_val(F, V).

is_valid_feat(F, AllFeats) :-
	member(F, AllFeats).

is_semantic_component(Sem) :-
	nonvar(Sem),
	functor(Sem, +, 2).

%---------------------------------------------------------------

is_valid_feat_val(_F, V) :-
	var(V),
	!.
is_valid_feat_val(F, V) :-
	( F = sem ; F = gsem ),
	is_valid_sem_val(V),
	!.
is_valid_feat_val(_F, V) :-
	F \== sem,
	F \== gsem,
	functor(V, bv, _),
	!.
is_valid_feat_val(F, V) :-
	F \== sem,
	F \== gsem,
	get_value_space_for_feat(F, ValueSpace),
	is_valid_val_for_space(V, ValueSpace),
	!.
is_valid_feat_val(F, V) :-
	regulus_error('~NBad feature-value pair: ~q=~q~n', [F, V]).

is_valid_sem_val(Expr) :-
	is_valid_sem_val(Expr, list_ok).

is_valid_sem_val(V, _ListOK) :-
	var(V),
	!.
is_valid_sem_val(A, _ListOK) :-
	atomic(A),
	!.
is_valid_sem_val(FuncExpr, _ListOK) :-
	functor(FuncExpr, F, N),
	gsl_function(F/N),
	is_valid_sem_val_args(N, FuncExpr),
	!.
is_valid_sem_val(List, list_ok) :-
	is_feat_val_list(List),
	is_valid_sem_feat_val_list(List),
	!.
is_valid_sem_val(List, list_ok) :-
	is_non_feat_val_list(List),
	is_valid_sem_non_feat_val_list(List),
	!.
%is_valid_sem_val((F, R), list_ok) :-
%	is_valid_sem_val(F),
%	is_valid_sem_val(R).
is_valid_sem_val(Other, _ListOK) :-
	regulus_error('~NBad semantic value: ~q~n', [Other]).

is_valid_sem_feat_val_list([]).
is_valid_sem_feat_val_list([(_F=V) | R]) :-
	is_valid_sem_val(V, list_ok),
	!,
	is_valid_sem_feat_val_list(R).

is_valid_sem_non_feat_val_list([]).
is_valid_sem_non_feat_val_list([F | R]) :-
	\+ is_feat_val_pair(F),
	is_valid_sem_val(F, list_ok),
	!,
	is_valid_sem_non_feat_val_list(R).

is_valid_sem_val_args(0, _FuncExpr).
is_valid_sem_val_args(I, FuncExpr) :-
	I > 0,
	arg(I, FuncExpr, Arg),
	is_valid_sem_val(Arg, list_ok),
	I1 is I - 1,
	!,
	is_valid_sem_val_args(I1, FuncExpr).

is_valid_val_for_space(V, _ValueSpace) :-
	var(V),
	!.
is_valid_val_for_space(Atom, ValueSpace) :-
	atomic(Atom),
	!,
	member(Component, ValueSpace),
	member(Atom, Component).
is_valid_val_for_space((P/\Q), ValueSpace) :-
	is_valid_val_for_space(P, ValueSpace),
	is_valid_val_for_space(Q, ValueSpace).
is_valid_val_for_space((P\/Q), ValueSpace) :-
	is_valid_val_for_space(P, ValueSpace),
	is_valid_val_for_space(Q, ValueSpace).
is_valid_val_for_space((\(P)), ValueSpace) :-
	is_valid_val_for_space(P, ValueSpace).

%---------------------------------------------------------------

% If we are using Prolog semantics, we allow anonymous variables in the LHS.
instantiate_null_sem_values(_Rule) :-
	using_prolog_semantics,
	!.
instantiate_null_sem_values((Head --> Body)) :-
        Head = cat(_, _, Sem),
	term_variables(Sem, HeadSemVars),
	term_variables(Body, BodyVars), 
	instantiate_null_sem_values1(HeadSemVars, BodyVars). 

instantiate_null_sem_values1([], _).
instantiate_null_sem_values1([HeadSemVar | HeadSemVars], BodyVars) :-
	(   id_member(HeadSemVar, BodyVars) -> true
	;
	    HeadSemVar = '*null_value*'
	),
	instantiate_null_sem_values1(HeadSemVars, BodyVars).

%---------------------------------------------------------------

feats_and_vals_to_feat_vals([], [], []) :-
	!.
feats_and_vals_to_feat_vals([F | R], [V | R1], [(F = V) | R2]) :-
	feats_and_vals_to_feat_vals(R, R1, R2),
	!.
feats_and_vals_to_feat_vals(X, Y, Z) :-
	format2error('~NError: bad call: ~w~n', [feats_and_vals_to_feat_vals(X, Y, Z)]),
	fail.

%---------------------------------------------------------------

non_ignored_synfeats_for_cat(CatName, NonIgnoredSynFeats) :-
	category_internal(CatName, Feats),
	findall(IgnoredFeat, ignore_feature(IgnoredFeat), IgnoredFeats),
	append([gsem, sem], IgnoredFeats, IgnoredAndSemFeats),
	list_to_ord_set(IgnoredAndSemFeats, IgnoredAndSemFeatsOS),
	ord_subtract(Feats, IgnoredAndSemFeatsOS, NonIgnoredSynFeats),
	!.
non_ignored_synfeats_for_cat(CatName, NonIgnoredSynFeats) :-
	format2error('~NError: bad call: ~w~n', [non_ignored_synfeats_for_cat(CatName, NonIgnoredSynFeats)]),
	fail.

%---------------------------------------------------------------

% get_value_space_for_feat(+Feat, -ValueSpace)

get_value_space_for_feat(Feat, ValueSpace) :-
	feature(Feat, ValueSpaceName),
	feature_value_space(ValueSpaceName, ValueSpace),
	!.
get_value_space_for_feat(Feat, _ValueSpace) :-
	regulus_error('~NCouldn\'t find value space for feature: ~w~n', [Feat]).

%---------------------------------------------------------------

possible_value_of_feature_consistent_with_spec(_Feat, Spec, Val) :-
	Spec == 'ANY',
	!,
	Val = 'ANY'.
possible_value_of_feature_consistent_with_spec(Feat, Spec, Val) :-
	nonvar(Spec),
	Spec = 'ANY'(Constraint),
	concise_name_for_constraint(Feat, Constraint, ConstraintName),	
	!,
	join_with_underscore(['ANY', ConstraintName], Val).
possible_value_of_feature_consistent_with_spec(Feat, Spec, Val) :-
	get_value_space_for_feat(Feat, ValueSpace),
	member_of_value_space(ValueSpace, Val),
	feature_value_compatible_with_spec(Val, Spec).

%---------------------------------------------------------------

% constraint_on_feat_allows_some_but_not_all_values(Constraint, Feat)

constraint_on_feat_allows_some_but_not_all_values(Constraint, Feat) :-
	get_value_space_for_feat(Feat, ValueSpace),
	possible_values_for_value_space(ValueSpace, AllPossibleVals),
	feature_values_in_list_compatible_with_spec(AllPossibleVals, Constraint, CompatibleVals),
	length(AllPossibleVals, NVals),
	length(CompatibleVals, NCompatibleVals),
	1 < NCompatibleVals, NCompatibleVals < NVals.

% constraint_on_feat_allows_all_values(Constraint, Feat)

constraint_on_feat_allows_all_values(Constraint, Feat) :-
	get_value_space_for_feat(Feat, ValueSpace),
	possible_values_for_value_space(ValueSpace, AllPossibleVals),
	feature_values_in_list_compatible_with_spec(AllPossibleVals, Constraint, CompatibleVals),
	length(AllPossibleVals, NVals),
	length(CompatibleVals, NCompatibleVals),
	NCompatibleVals = NVals.

feature_values_in_list_compatible_with_spec([], _Constraint, []).
feature_values_in_list_compatible_with_spec([F | R], Constraint, Result) :-
	(   feature_value_compatible_with_spec(F, Constraint) ->
	    Result = [F | Result1] ;
	    Result = Result1
	),
	feature_values_in_list_compatible_with_spec(R, Constraint, Result1).

%---------------------------------------------------------------

% concise_name_for_constraint(+Feat, +Constraint, -ConstraintName)

% Name is constructed by matching against space of all possible values, 
% and marking a '1' for each possible val, a '0' for each impossible val.

concise_name_for_constraint(Feat, Constraint, ConstraintName) :-
	get_value_space_for_feat(Feat, ValueSpace),
	possible_values_for_value_space(ValueSpace, AllPossibleVals),
	concise_name_string_for_constraint(AllPossibleVals, Constraint, ConstraintNameString-[]),
	atom_codes(ConstraintName, ConstraintNameString),
	!.
concise_name_for_constraint(Feat, Constraint, ConstraintName) :-
	regulus_error('~NBad call: ~w~n', [concise_name_for_constraint(Feat, Constraint, ConstraintName)]).

concise_name_string_for_constraint([], _Constraint, String-String).
concise_name_string_for_constraint([F | R], Constraint, [F1 | R1]-Out) :-
	(   feature_value_compatible_with_spec(F, Constraint) ->
	    F1 = 0'1 ;
	    F1 = 0'0
	),
	!,
	concise_name_string_for_constraint(R, Constraint, R1-Out).

% possible_values_for_value_space(ValueSpace, AllPossibleVals)

possible_values_for_value_space(ValueSpace, AllPossibleVals) :-
	findall(Val, member_of_value_space(ValueSpace, Val), AllPossibleVals).

% member_of_value_space(+ValueSpace, ?Val)

member_of_value_space([], []).
member_of_value_space([Choices | RChoices], [Choice | RChoice]) :-
	member(Choice, Choices),
	member_of_value_space(RChoices, RChoice).

% feature_value_compatible_with_spec(Val, Spec).

% Uninstantiated feature value.
feature_value_compatible_with_spec(_Val, Spec) :-
	var(Spec),
	!.
% Base case for following. Note that [] is atomic, so this clause
% need to go before the one for atomic Spec.
feature_value_compatible_with_spec(_Val, []) :-
	!.
% The feature has already been instantiated somewhere else.
feature_value_compatible_with_spec(Val, [Spec1 | Spec2]) :-
	!,
	feature_value_compatible_with_spec(Val, Spec1),
	feature_value_compatible_with_spec(Val, Spec2).
% Atomic feature value specification. It can only belong to one
% dimension of the feature value space, so we just check them in
% turn using member to get a match.
feature_value_compatible_with_spec(Val, Spec) :-
	atomic(Spec),
	!,
	member(Spec, Val).
% Conjunction in the obvious way.
feature_value_compatible_with_spec(Val, (Spec1/\Spec2)) :-
	!,
	feature_value_compatible_with_spec(Val, Spec1),
	feature_value_compatible_with_spec(Val, Spec2).
% Disjunction in the obvious way.
feature_value_compatible_with_spec(Val, (Spec1\/Spec2)) :-
	!,
	(   feature_value_compatible_with_spec(Val, Spec1) ;
	    feature_value_compatible_with_spec(Val, Spec2)
	).
% Negation in the obvious way.
feature_value_compatible_with_spec(Val, (\(Spec))) :-
	!,
	\+ feature_value_compatible_with_spec(Val, Spec).

%---------------------------------------------------------------

is_feat_val_list([]).
is_feat_val_list(List) :-
	nonvar(List),
	List = [F | R],
	is_feat_val_pair(F),
	!,
	is_feat_val_list(R).

is_non_feat_val_list([]).
is_non_feat_val_list(List) :-
	nonvar(List),
	List = [F | R],
	\+is_feat_val_pair(F),
	!,
	is_non_feat_val_list(R).

is_feat_val_pair(T) :-
	nonvar(T),
	T = ( Feat = _Val),
	atomic(Feat).	

%---------------------------------------------------------------

remove_ignored_features_from_list([], []).
remove_ignored_features_from_list([F | R], R1) :-
	ignore_feature(F),
	!,
	remove_ignored_features_from_list(R, R1).
remove_ignored_features_from_list([F | R], [F | R1]) :-
	!,
	remove_ignored_features_from_list(R, R1).

remove_ignored_feat_val_pairs_from_list([], []).
remove_ignored_feat_val_pairs_from_list([F=_Val | R], R1) :-
	ignore_feature(F),
	!,
	remove_ignored_feat_val_pairs_from_list(R, R1).
remove_ignored_feat_val_pairs_from_list([F | R], [F | R1]) :-
	!,
	remove_ignored_feat_val_pairs_from_list(R, R1).

%---------------------------------------------------------------

% head_cat_and_line_info_for_rule(+Rule, ?Cat, ?LineInfo) 

head_cat_and_line_info_for_rule(rule( (cat(HeadCat, _SynFeatsWithVals, _Value) --> _Body), LineInfo), HeadCat, LineInfo).

%---------------------------------------------------------------

summarise_rule((H --> B), (H1 --> B1)) :-
	summarise_cat(H, H1),
	summarise_rule_body(B, B1),
	!.
summarise_rule(X, Y) :-
	format2error('~N*** Error: bad call: ~q.~n', [summarise_rule(X, Y)]),
	fail.

summarise_cat(Atom, Summary) :-
	atomic(Atom),
	Summary = Atom,
	!.
summarise_cat(CatName:_Feats, Summary) :-
	atomic(CatName),
	Summary = CatName,
	!.
summarise_cat(X, Y) :-
	format2error('~N*** Error: bad call: ~q.~n', [summarise_cat(X, Y)]),
	fail.

summarise_rule_body((F, R), (F1, R1)) :-
	!,
	summarise_rule_body(F, F1),
	summarise_rule_body(R, R1).
summarise_rule_body(Cat, Cat1) :-
	summarise_cat(Cat, Cat1).

%---------------------------------------------------------------

get_category_internal(null_sem_to_expand, [sem]) :-
	!.
get_category_internal(CatName, AllFeats) :-
	category_internal(CatName, AllFeats),
	!.
get_category_internal(CatName, _AllFeats) :-
	regulus_error('~NUndeclared category: ~w~n', [CatName]).

%---------------------------------------------------------------

regulus_error(Format, Args) :-
	raise_exception(regulus_exception(Format, Args)).

inform_about_regulus_exception(Exception, LineInfo) :-
	format2error('~N*** REGULUS COMPILATION ERROR ', []),
	inform_about_line_info(LineInfo),
	inform_about_regulus_exception1(Exception).

inform_about_line_info(line_info(_ItemNumber, _OtherInfo, Lines, File)) :-
	inform_about_line_info(line_info(_ItemNumber, Lines, File)),
	!.
inform_about_line_info(line_info(_ItemNumber, LineFrom-LineTo, File)) :-
	(   LineFrom = LineTo ->
	    format2error('on line ~d~n in ~w', [LineFrom, File]) ;
	    format2error('between lines ~d and ~d in ~w~n', [LineFrom, LineTo, File])
	),
	!.
inform_about_line_info(_Other).

inform_about_regulus_exception1(regulus_exception(Format, Args)) :-
	format2error(Format, Args),
	!.
inform_about_regulus_exception1(Exception) :-
	format2error('~N~w~n', [Exception]).

inform_about_top_level_regulus_error(failed) :-
	!,
	format2error('~N~nProcessing aborted due to internal error.~n', []).
inform_about_top_level_regulus_error(Atom) :-
	atomic(Atom),
	!,
	format2error('~N~nProcessing aborted due to internal error: ~w~n', [Atom]).
inform_about_top_level_regulus_error(regulus_exception(Format, Args)) :-
	!,
	format2error('~N~n*** REGULUS COMPILATION ERROR (no particular line)~n', []),
	format2error(Format, Args),
	format2error('~N~nProcessing aborted.~n', []).
inform_about_top_level_regulus_error(Exception) :-
	!,
	inform_about_non_regulus_exception(Exception).

inform_about_non_regulus_exception(Exception) :-
	with_output_to_chars(inform_about_non_regulus_exception1(Exception),
			     String),
	format2error('~s', [String]).

inform_about_non_regulus_exception1(Exception) :-
	format('~N~n*** Processing aborted due to internal error ***~n~n', []),
	safe_print_message(error, Exception),
	format('~n', []).

%---------------------------------------------------------------

set_regulus_switch(SwitchName, _Value) :-
	\+ user:declared_regulus_switch(SwitchName, _PossibleValues),
	!,
	findall(S, user:declared_regulus_switch(S, _), Ss),
	format('~NWarning: switch "~w" not declared.~nDeclared switches: ~w~n', [SwitchName, Ss]).
set_regulus_switch(SwitchName, Value) :-
	user:declared_regulus_switch(SwitchName, PossibleValues),
	\+ member(Value, PossibleValues),
	!,
	format('~NWarning: "~w" not a declared possible value for switch "~w". Declared values: ~w~n', [Value, SwitchName, PossibleValues]).
set_regulus_switch(SwitchName, Value) :-
	retractall(regulus_switch(SwitchName, _)),
	asserta(regulus_switch(SwitchName, Value)),
	!.

set_regulus_debug_level(Level) :-
	integer(Level),
	Level >= 0,
	!,
	retractall(regulus_debug_level(_)),
	assert(regulus_debug_level(Level)).
set_regulus_debug_level(_Level) :-
	format2error('~NError: set_regulus_debug_level should be called with a non-negative integer argument~n', []),
	fail.

regulus_debug_call(RequiredLevel, Goal) :-
	regulus_debug_level(Level),
	Level >= RequiredLevel,
	!,
	call(Goal).
regulus_debug_call(_RequiredLevel, _Goal).
	
%---------------------------------------------------------------

is_list_of_atom_lists([]).
is_list_of_atom_lists([F | R]) :-
	is_atom_list(F),
	!,
	is_list_of_atom_lists(R).

is_atom_list([]).
is_atom_list([F | R]) :-
	atomic(F),
	!,
	is_atom_list(R).

%---------------------------------------------------------------

is_list_of_simple_pairs([]).
is_list_of_simple_pairs([F | R]) :-
	is_simple_pair(F),
	!,
	is_list_of_simple_pairs(R).

is_simple_pair(Pair) :-
	nonvar(Pair),
	Pair = [Key, _Value],
	atomic(Key),
	!.

%---------------------------------------------------------------

interpret_string_as_raw_lf_input(Chars, LF) :-
	strip_off_initial_lf_tag(Chars, Chars1),
	append(Chars1, ".", Chars2),
	on_exception(_Exception,
		     read_from_chars(Chars2, LF),
		     ( format2error('~N*** Error: unable to treat "~s" as Prolog term~n', [Chars2]), fail )
		    ),
	%format('~N--- Setting LF = "~q" and skipping parsing phase~n', [LF]),
	!.

strip_off_initial_lf_tag(CharsIn, CharsOut) :-
	split_string_into_words(CharsIn, 0':, ColonChunks),
	ColonChunks = [FirstColonChunk | RestColonChunks],
	split_atom_into_words(FirstColonChunk, FirstColonChunkComponents),
	FirstColonChunkComponents = ['LF'],
	append_atoms(RestColonChunks, 0':, RestAtom),
	atom_codes(RestAtom, CharsOut),
	!.

%---------------------------------------------------------------

using_prolog_semantics :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(prolog_semantics, yes),
	!.

%---------------------------------------------------------------

using_strcat_semantics :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(strcat_semantics, yes),
	!.

%---------------------------------------------------------------

gsl_function(_F/_N) :-
	using_prolog_semantics,
	!.

gsl_function(add/2).
gsl_function(sub/2).
gsl_function(mul/2).
gsl_function(div/2).
gsl_function(neg/1).

gsl_function(strcat/2).

gsl_function(insert_begin/2).
gsl_function(insert_end/2).
gsl_function(concat/2).
gsl_function(first/1).
gsl_function(last/1).
gsl_function(rest/1).

%---------------------------------------------------------------

print_form_for_syn_feats_grounded(SynFeats, PrintForm) :-
	copy_term(SynFeats, SynFeats1),
	print_form_for_syn_feats(SynFeats1, PrintForm),
	make_ground(PrintForm).

print_form_for_syn_feats(SynFeats, PrintSynFeats) :-
	print_form_for_syn_feats1(SynFeats, PrintSynFeats).

print_form_for_syn_feats1([], []).
print_form_for_syn_feats1([F | R], [F1 | R1]) :-
	print_form_for_feat_val_pair(F, F1),
	print_form_for_syn_feats1(R, R1).

print_form_for_feat_val_pair((Feat = Val), (Feat = Val1)) :-
	print_form_for_feat_val(Val, Feat, Val1).

print_form_for_feat_val(X, _Feat, X) :-
	var(X),
	!.
print_form_for_feat_val(A, _Feat, A) :-
	atomic(A),
	!.
print_form_for_feat_val(Val, Feat, Val1) :-
	functor(Val, bv, NBV),
	get_value_space_for_feat(Feat, ValueSpace),
	possible_values_for_value_space(ValueSpace, AllPossibleVals),
	length(AllPossibleVals, NPossVals),
	NPossVals1 is NPossVals + 1,
	(   NPossVals1 = NBV ->
	    print_form_for_feat_val1(Val, AllPossibleVals, NPossVals, Val1)
	;
	    format2error('~N*** Error: encoded value ~w for feature ~w not compatible with current definition', [Val, Feat]),
	    fail
	),
	!.
print_form_for_feat_val(Val, Feat, Val1) :-
	functor(Val, bv, _NBV),
	format2error('~N*** Error: bad call: ~w~n',
		     [print_form_for_feat_val(Val, Feat, Val1)]),
	!,
	fail.
print_form_for_feat_val(Val, _Feat, Val).
	
print_form_for_feat_val1(Val, AllPossibleVals, NPossVals, ValOut) :-
	print_form_for_feat_val2(1, AllPossibleVals, NPossVals, Val, PositiveVals),
	make_disjunctive_or_negated_disjunctive_form_for_feat_val(PositiveVals, AllPossibleVals, ValOut),
	!.
print_form_for_feat_val1(Val, AllPossibleVals, NPossVals, ValOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_form_for_feat_val1(Val, AllPossibleVals, NPossVals, ValOut)]),
	fail.

print_form_for_feat_val2(I, _AllPossibleVals, NPossVals, _Val, []) :-
	I > NPossVals,
	!.
print_form_for_feat_val2(I, [PossVal | PossVals], NPossVals, Val, Result) :-
	I1 is I + 1,
	arg(I, Val, BVI),
	arg(I1, Val, BVI1),
	(   BVI == BVI1 ->
	    Result = Val1 ;
	    Result = [PossVal | Val1]
	),
	!,
	print_form_for_feat_val2(I1, PossVals,  NPossVals, Val, Val1).

make_disjunctive_or_negated_disjunctive_form_for_feat_val(PositiveVals, AllPossibleVals, ValOut) :-
	length(AllPossibleVals, NAllPossibleVals),
	HalfNAllPossibleVals is NAllPossibleVals / 2,
	length(PositiveVals, NPositiveVals),
	(   ( NPositiveVals =< HalfNAllPossibleVals ) ->
	    
	    make_disjunctive_form_for_feat_val(PositiveVals, DisjunctionOfPositives),
	    ValOut = DisjunctionOfPositives ;
	    
	    list_to_ord_set(PositiveVals, PositiveValsOS),
	    list_to_ord_set(AllPossibleVals, AllPossibleValsOS),
	    ord_subtract(AllPossibleValsOS, PositiveValsOS, NegativeValsOS),
	    (   NegativeValsOS = [] ->
		ValOut = _Any ;
		make_disjunctive_form_for_feat_val(NegativeValsOS, DisjunctionOfNegatives),
		ValOut = (\(DisjunctionOfNegatives))
	    )
	).
	
make_disjunctive_form_for_feat_val([Val], Val1) :-
	!,
	make_conjunctive_form_for_feat_val(Val, Val1).
make_disjunctive_form_for_feat_val([F | R], (F1 \/R1)) :-
	make_conjunctive_form_for_feat_val(F, F1),
	!,
	make_disjunctive_form_for_feat_val(R, R1).

make_conjunctive_form_for_feat_val([Val], Val) :-
	!.
make_conjunctive_form_for_feat_val([F | R], (F/\R1)) :-
	make_conjunctive_form_for_feat_val(R, R1).

%---------------------------------------------------------------

remove_lexical_info_in_parse_tree(Tree, Tree1) :-
	on_exception(
	_Exception, 
	remove_lexical_info_in_parse_tree1(Tree, Tree1, '*root*'),
	handle_remove_lexical_info_in_parse_tree_error(Tree)
    ),
	!.
remove_lexical_info_in_parse_tree(Tree, _Tree) :-
	handle_remove_lexical_info_in_parse_tree_error(Tree).

handle_remove_lexical_info_in_parse_tree_error(_Tree) :-
	format('~N*** Warning: call to remove_lexical_info_in_parse_tree/2 failed.~n', []),
	fail.

% Empty production
% Empty productions may become non-empty
%remove_lexical_info_in_parse_tree1(empty_constituent, Result, Above) :-
%	(   (   current_predicate(user:regulus_config/2),
%		user:regulus_config(constituent_is_optionally_empty_depending_on_encoding, Above)
%	    ) ->
%	    Result = _LexOrEmpty
%	;
%	    otherwise ->
%	    Result = empty_constituent
%	),
%	!.
remove_lexical_info_in_parse_tree1(phrase(Mother, _LineInfo, empty_constituent),
				   phrase(Mother, _UnspecifiedLineInfo, _UnspecifiedDaughter),
				   _Above) :-
	current_predicate(user:regulus_config/2),
	user:regulus_config(constituent_is_optionally_empty_depending_on_encoding, Mother),
	!.
remove_lexical_info_in_parse_tree1(empty_constituent, empty_constituent, _Above) :-
	!.
% Lex
remove_lexical_info_in_parse_tree1(lex(_Word), lex(_), _Above) :-
	!.
% Not lexical or empty production
remove_lexical_info_in_parse_tree1(phrase(Mother, LineInfo, Daughters),
				   phrase(Mother, LineInfo, Daughters1),
				   _Above) :-
	remove_lexical_info_in_parse_tree_daughters(Daughters, Daughters1, Mother),
	!.
remove_lexical_info_in_parse_tree1(Other, Other1, Above) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_lexical_info_in_parse_tree1(Other, Other1, Above)]),
	fail.

remove_lexical_info_in_parse_tree_daughters((F, R), (F1, R1), Above) :-
	!,
	remove_lexical_info_in_parse_tree_daughters(F, F1, Above),
	remove_lexical_info_in_parse_tree_daughters(R, R1, Above).
remove_lexical_info_in_parse_tree_daughters(Other, Other1, Above) :-
	remove_lexical_info_in_parse_tree1(Other, Other1, Above).	

%---------------------------------------------------------------

remove_lexical_and_line_info_in_parse_tree(Tree, Tree1) :-
	on_exception(
	_Exception, 
	remove_lexical_and_line_info_in_parse_tree1(Tree, Tree1),
	handle_remove_lexical_and_line_info_in_parse_tree_error(Tree)
    ),
	!.
remove_lexical_and_line_info_in_parse_tree(Tree, _Tree) :-
	handle_remove_lexical_and_line_info_in_parse_tree_error(Tree).

handle_remove_lexical_and_line_info_in_parse_tree_error(_Tree) :-
	format('~N*** Warning: call to remove_lexical_and_line_info_in_parse_tree/2 failed.~n', []),
	fail.

% Empty production
remove_lexical_and_line_info_in_parse_tree1(empty_constituent, empty_constituent) :-
	!.
% Lex
remove_lexical_and_line_info_in_parse_tree1(lex(_Word), lex(_)) :-
	!.
% Not lexical or empty production
remove_lexical_and_line_info_in_parse_tree1(phrase(Mother, _AnyLineInfo, Daughters),
				   phrase(Mother, _UninstantiatedLineInfo, Daughters1)) :-
	remove_lexical_and_line_info_in_parse_tree_daughters(Daughters, Daughters1),
	!.
remove_lexical_and_line_info_in_parse_tree1(Other, Other1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_lexical_and_line_info_in_parse_tree1(Other, Other1)]),
	fail.

remove_lexical_and_line_info_in_parse_tree_daughters((F, R), (F1, R1)) :-
	!,
	remove_lexical_and_line_info_in_parse_tree_daughters(F, F1),
	remove_lexical_and_line_info_in_parse_tree_daughters(R, R1).
remove_lexical_and_line_info_in_parse_tree_daughters(Other, Other1) :-
	remove_lexical_and_line_info_in_parse_tree1(Other, Other1).	

%---------------------------------------------------------------

remove_lexical_and_file_info_in_parse_tree(Tree, Tree1) :-
	on_exception(
	_Exception, 
	remove_lexical_and_file_info_in_parse_tree1(Tree, Tree1, '*root*'),
	handle_remove_lexical_and_file_info_in_parse_tree_error(Tree)
    ),
	!.
remove_lexical_and_file_info_in_parse_tree(Tree, _Tree, _Above) :-
	handle_remove_lexical_and_file_info_in_parse_tree_error(Tree).

handle_remove_lexical_and_file_info_in_parse_tree_error(_Tree) :-
	format('~N*** Warning: call to remove_lexical_and_file_info_in_parse_tree/2 failed.~n', []),
	fail.

% Empty production
%remove_lexical_and_file_info_in_parse_tree1(empty_constituent, empty_constituent) :-
%	!.
% Empty productions may become lex
remove_lexical_and_file_info_in_parse_tree1(empty_constituent, Result, Above) :-
	(   (   current_predicate(user:regulus_config/2),
		user:regulus_config(constituent_is_optionally_empty_depending_on_encoding, Above)
	    ) ->
	    Result = _LexOrEmpty
	;
	    otherwise ->
	    Result = empty_constituent
	),
	!.
% Lex
remove_lexical_and_file_info_in_parse_tree1(lex(_Word), lex(_), _Above) :-
	!.
% Not lexical or empty production
remove_lexical_and_file_info_in_parse_tree1(phrase(Mother, LineInfo, Daughters),
					    phrase(Mother, LineInfoWithoutFile, Daughters1),
					    _Above) :-
	remove_file_from_line_info(LineInfo, LineInfoWithoutFile),
	remove_lexical_and_file_info_in_parse_tree_daughters(Daughters, Daughters1, Mother),
	!.
remove_lexical_and_file_info_in_parse_tree1(Other, Other1, Above) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_lexical_and_file_info_in_parse_tree1(Other, Other1, Above)]),
	fail.

remove_lexical_and_file_info_in_parse_tree_daughters((F, R), (F1, R1), Above) :-
	!,
	remove_lexical_and_file_info_in_parse_tree_daughters(F, F1, Above),
	remove_lexical_and_file_info_in_parse_tree_daughters(R, R1, Above).
remove_lexical_and_file_info_in_parse_tree_daughters(Other, Other1, Above) :-
	remove_lexical_and_file_info_in_parse_tree1(Other, Other1, Above).

%remove_file_from_line_info(line_info(FreqOrId, Lines, _File),
%			   line_info(FreqOrId, Lines, _)) :-
%	!.
remove_file_from_line_info(line_info(FreqOrId, _Lines, _File),
			   line_info(FreqOrId, _OtherLines, _)) :-
	!.
remove_file_from_line_info(LineInfo, LineInfoWithoutFile) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_file_from_line_info(LineInfo, LineInfoWithoutFile)]),
	fail.


%---------------------------------------------------------------
	
parse_tree_to_tagged_word_list(Tree, List) :-
	on_exception(
	_Exception, 
	parse_tree_to_tagged_word_list1(Tree, List-[]),
	handle_parse_tree_to_tagged_word_list_error(Tree)
    ),
	!.
parse_tree_to_tagged_word_list(Tree, _List) :-
	handle_parse_tree_to_tagged_word_list_error(Tree).

handle_parse_tree_to_tagged_word_list_error(_Tree) :-
	format('~N*** Warning: call to parse_tree_to_tagged_word_list/2 failed.~n', []).

% Empty production
parse_tree_to_tagged_word_list1(empty_constituent, In-In) :-
	!.
% Syncategorematic lex
parse_tree_to_tagged_word_list1(lex(Word), [Word/null | Out]-Out) :-
	!.
% Normal lex
parse_tree_to_tagged_word_list1(LexNode, In-Out) :-
	lex_node(LexNode),
	cat_and_words_for_lex_node(LexNode, Cat, Words),
	(   Words = [] ->
	    In = Out ;
	    
	    Words = [SingleWord] ->
	    In = [SingleWord/Cat | Out] ;
	    
	    In = [Words/Cat | Out]
	),
	!.
% Not lexical or empty production
parse_tree_to_tagged_word_list1(phrase(_Mother, _LineInfo, Daughters), In-Out) :-
	parse_tree_to_tagged_word_list_daughters(Daughters, In-Out),
	!.
parse_tree_to_tagged_word_list1(Other, InOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [parse_tree_to_tagged_word_list1(Other, InOut)]),
	fail.

parse_tree_to_tagged_word_list_daughters((F, R), In-Out) :-
	!,
	parse_tree_to_tagged_word_list_daughters(F, In-Next),
	parse_tree_to_tagged_word_list_daughters(R, Next-Out).
parse_tree_to_tagged_word_list_daughters(Other, In-Out) :-
	parse_tree_to_tagged_word_list1(Other, In-Out).

%---------------------------------------------------------------

parse_tree_to_summary(Tree, Tree1) :-
	on_exception(
	_Exception, 
	parse_tree_to_summary1(Tree, Tree1),
	handle_parse_tree_to_summary_error(Tree)
    ),
	!.
parse_tree_to_summary(Tree, _Tree1) :-
	handle_parse_tree_to_summary_error(Tree).

handle_parse_tree_to_summary_error(_Tree) :-
	format('~N*** Warning: call to parse_tree_to_summary/2 failed.~n', []).

% Empty tree
parse_tree_to_summary1(no_tree, no_tree) :-
	!.
% Empty production
parse_tree_to_summary1(empty_constituent, empty_constituent) :-
	!.
% Syncategorematic lex
parse_tree_to_summary1(lex(Word), lex(Word)) :-
	!.
% Comma list of items
parse_tree_to_summary1(CommaList, Summary) :-
	is_comma_list(CommaList),
	comma_list_to_list(CommaList, List),
	parse_tree_to_summary_list(List, Summary),
	!.
% Normal lex
parse_tree_to_summary1(LexNode, ( Cat < Result )) :-
	lex_node(LexNode),
	cat_and_words_for_lex_node(LexNode, Cat, Words),
	(   Words = [] ->
	    Result = null ;
	    
	    Words = [SingleWord] ->
	    Result = lex(SingleWord) ;
	    
	    Result = lex(Words)
	),
	!.
% Not lexical or empty production
parse_tree_to_summary1(phrase(Cat, _LineInfo, Daughters), ( Cat < Daughters1 )) :-
	comma_list_to_list(Daughters, DaughtersList),
	parse_tree_to_summary_list(DaughtersList, Daughters1),
	!.
parse_tree_to_summary1(Other, Res) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [parse_tree_to_summary1(Other, Res)]),
	fail.

parse_tree_to_summary_list([], []).
parse_tree_to_summary_list([F | R], [F1 | R1]) :-
	!,
	parse_tree_to_summary1(F, F1),
	parse_tree_to_summary_list(R, R1).

%---------------------------------------------------------------

prettyprint_transfer_trace([]) :-
	format('~N(no rules)~n', []),
	!.
prettyprint_transfer_trace(TraceInfo) :-
	empty_assoc_generic(AssocIn),
	prettyprint_transfer_trace1(TraceInfo, AssocIn-AssocOut),
	print_file_assoc_generic(AssocOut),
	!.

prettyprint_transfer_trace1([], AssocIn-AssocIn).
prettyprint_transfer_trace1([F | R], AssocIn-AssocOut) :-
	print_transfer_trace_element(F, AssocIn-AssocNext),
	!,
	prettyprint_transfer_trace1(R, AssocNext-AssocOut).

print_transfer_trace_element((LHS-RHS):LineInfo, AssocIn-AssocOut) :-
	format('~N~q --> ~q  ', [LHS, RHS]),
	prettyprint_line_info(LineInfo, print_line_info, AssocIn-AssocOut),
	!.
print_transfer_trace_element(Other, AssocIn-AssocIn) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [print_transfer_trace_element(Other)]),
	!.

%---------------------------------------------------------------

prettyprint_parse_tree(Tree) :-
	prettyprint_parse_tree(Tree, dont_print_line_info).

prettyprint_parse_tree(Tree, PrintLineInfo) :-
	on_exception(
	_Exception, 
	prettyprint_parse_tree_top(Tree, PrintLineInfo),
	handle_prettyprint_parse_tree_error(Tree)
    ),
	!.
prettyprint_parse_tree(Tree, _PrintLineInfo) :-
	handle_prettyprint_parse_tree_error(Tree).

handle_prettyprint_parse_tree_error(Tree) :-
	format2error('~N*** Error: call to prettyprint_parse_tree/2 failed. Printing using simple version.~n~n', []),
	simple_prettyprint_parse_tree(Tree).

prettyprint_parse_tree_top(no_tree, _PrintLineInfo) :-
	format('no_tree', []),
	!.
prettyprint_parse_tree_top(Tree, PrintLineInfo) :-
	member(PrintLineInfo, [print_line_info, print_line_info_and_numbers]),
	empty_assoc_generic(AssocIn),
	prettyprint_parse_tree_main(Tree, '', PrintLineInfo, AssocIn-AssocOut),
	print_file_assoc_generic(AssocOut),
	!.
prettyprint_parse_tree_top(Tree, PrintLineInfo) :-
	\+ member(PrintLineInfo, [print_line_info, print_line_info_and_numbers]),
	empty_assoc_generic(AssocIn),
	prettyprint_parse_tree_main(Tree, '', PrintLineInfo, AssocIn-_AssocOut),
	!.

prettyprint_parse_tree_main(Tree, Indent, PrintLineInfo, Assoc) :-
	copy_term(Tree, Tree1),
	make_ground(Tree1),
	with_output_to_chars(prettyprint_parse_tree(Tree1, Indent, PrintLineInfo, Assoc),
			     RawString),
	clean_up_and_print_parse_tree(RawString).

prettyprint_parse_tree(LexNode, _Indent, PrintLineInfo, AssocIn-AssocOut) :-
	lex_node(LexNode),
	prettyprint_lex_node(LexNode, PrintLineInfo, AssocIn-AssocOut),
	!.
prettyprint_parse_tree(phrase(Mother, LineInfo, _Daughters), _Indent, PrintLineInfo, AssocIn-AssocOut) :-
	placeholder_line_info(LineInfo),
	format('~w', [Mother]),
	prettyprint_line_info(LineInfo, PrintLineInfo, AssocIn-AssocOut),
	format(' *cut*', []),
	!.
prettyprint_parse_tree(phrase(Mother, LineInfo, Daughters), Indent, PrintLineInfo, AssocIn-AssocOut) :-
	format('~w', [Mother]),
	prettyprint_line_info(LineInfo, PrintLineInfo, AssocIn-AssocNext),
	(   Daughters = (_, _) ->
	    format_to_atom('~w|  ', [Indent], Indent1) ;
	    format_to_atom('~w   ', [Indent], Indent1)
	),
	prettyprint_daughters(Daughters, Indent1, PrintLineInfo, AssocNext-AssocOut),
	!.
%prettyprint_parse_tree(Other, _Indent, _PrintLineInfo, _Assoc) :-
%	regulus_error('Couldn''t prettyprint tree: ~w~n', [Other]).
prettyprint_parse_tree(Other, _Indent, _PrintLineInfo, AssocIn-AssocIn) :-
	format('~w ', [Other]),
	!.

prettyprint_lex_node(LexNode, PrintLineInfo, AssocIn-AssocOut) :-
	prettyprint_line(LexNode, PrintLineInfo, AssocIn-AssocOut).

prettyprint_line(phrase(Cat, LineInfo, Daughters), PrintLineInfo, AssocIn-AssocOut) :-
	!,
	format('~w ', [Cat]),
	prettyprint_line(Daughters, PrintLineInfo, AssocIn-AssocNext),
	prettyprint_line_info(LineInfo, PrintLineInfo, AssocNext-AssocOut).
prettyprint_line((F, R), PrintLineInfo, AssocIn-AssocOut) :-
	!,
	(   F = empty_constituent ->
	    format('~w ', [null]) ;
	    format('~w ', [F])
	),
	prettyprint_line(R, PrintLineInfo, AssocIn-AssocOut).
prettyprint_line(empty_constituent, _PrintLineInfo, AssocIn-AssocIn) :-
	format('null', []),
	!.
prettyprint_line(Other, _PrintLineInfo, AssocIn-AssocIn) :-
	format('~w', [Other]),
	!.

prettyprint_daughters(LexNode, Indent, PrintLineInfo, AssocIn-AssocOut) :-
	lex_node(LexNode),
	!,
	prettyprint_indent(Indent),
	prettyprint_line(LexNode, PrintLineInfo, AssocIn-AssocOut).
prettyprint_daughters((F, R), Indent, PrintLineInfo, AssocIn-AssocOut) :-
	!,
	prettyprint_indent(Indent),
	prettyprint_parse_tree(F, Indent, PrintLineInfo, AssocIn-AssocNext),
	prettyprint_daughters(R, Indent, PrintLineInfo, AssocNext-AssocOut).
prettyprint_daughters(LastDaughter, Indent, PrintLineInfo, AssocIn-AssocOut) :-
	prettyprint_indent(Indent),
	prettyprint_parse_tree(LastDaughter, Indent, PrintLineInfo, AssocIn-AssocOut),
	!.
 
prettyprint_line_info(LineInfo, print_line_info_and_numbers, AssocIn-AssocIn) :-
	placeholder_line_info(LineInfo),
	LineInfo = line_info(ItemNumber, _, _),
	format(' (node ~d)', [ItemNumber]),
	!.
prettyprint_line_info(LineInfo, PrintLineInfo, AssocIn-AssocOut) :-
	member(PrintLineInfo, [print_line_info, print_line_info_and_numbers]),
	(   LineInfo = line_info(ItemNumber, FromLine-ToLine, File), Cut = no_cut
	;
	    LineInfo = line_info(ItemNumber, Cut, FromLine-ToLine, File)
	),
	(   Cut = cut ->
	    CutText = ', CUT'
	;
	    CutText = ''
	),
	file_to_short_form(File, ShortFile),
	(   ( PrintLineInfo = print_line_info_and_numbers, number(ItemNumber) ) ->
	    format(' (node ~d~w)', [ItemNumber, CutText]) ;
	    ( PrintLineInfo = print_line_info, ItemNumber = frequency(Freq), number(Freq) ) ->
	    format(' (freq ~d)', [Freq]) ;
	    true
	),
	format(' [~w:~d-~d]', [ShortFile, FromLine, ToLine]),
	put_assoc_generic(ShortFile, AssocIn, File, AssocOut),
	!.
prettyprint_line_info(_LineInfo, _PrintLineInfo, AssocIn-AssocIn).

prettyprint_indent(Indent) :-
	format('~N~w', [Indent]).

file_to_short_form(File, ShortFile) :-
	split_atom_into_words(File, 0'., [Main, _Extension]),
	split_atom_into_words(Main, 0'/, Components),
	last(Components, ShortFile0),
	uppercase_atom(ShortFile0, ShortFile),
	!.
file_to_short_form(File, File).

placeholder_line_info(LineInfo) :-
	\+ real_line_info(LineInfo).

real_line_info(LineInfo) :-
	ground(LineInfo),
	(   LineInfo = line_info(Label, From-To, File)
	;
	    LineInfo = line_info(Label, _OtherInfo, From-To, File)
	),
	(   atomic(Label)
	;
	    ( nonvar(Label), Label = frequency(_) )
	),
	number(From),
	number(To),
	atom(File),
	!.

/*

We clean up the parse tree by rewriting vertical sequences of bar characters (|).
The first bar in the sequence is turned into a /, and the last one is
turned into a \. So e.g.

   |             /
   |    ---->    |
   |             \

*/

clean_up_and_print_parse_tree(String0) :-
	split_string_into_words(String0, 0'\n, List0),
	clean_up_parse_tree_lines(List0, '', List),
	write_cleaned_parse_tree(List).

clean_up_parse_tree_lines([], _Previous, []).
clean_up_parse_tree_lines([F | R], Previous, [F1 | R1]) :-
	(   R = [Next | _] ->
	    true ;
	    Next = ''
	),
	clean_up_parse_tree_line(F, Previous, Next, F1),
	!,
	clean_up_parse_tree_lines(R, F, R1).

clean_up_parse_tree_line(LineAtom0, Previous, Next, LineAtom) :-
	atom_codes(LineAtom0, Chars0),
	atom_codes(Previous, PreviousChars),
	atom_codes(Next, NextChars),
	clean_up_parse_tree_line1(Chars0, PreviousChars, NextChars, Chars),
	atom_codes(LineAtom, Chars).

clean_up_parse_tree_line1([], _PreviousChars, _NextChars, []).
clean_up_parse_tree_line1([F | R], PreviousChars, NextChars, [F1 | R1]) :-
	first_and_rest_or_null(PreviousChars, FPrevious, RPrevious),
	first_and_rest_or_null(NextChars, FNext, RNext),
	clean_up_parse_tree_line_char(F, FPrevious, FNext, F1),
	!,
	clean_up_parse_tree_line1(R, RPrevious, RNext, R1).

% Keep bar unchanged if bars above and below.
clean_up_parse_tree_line_char(0'|, 0'|, 0'|, 0'|) :-
	!.
% If bar above but not below, change to \
clean_up_parse_tree_line_char(0'|, 0'|, _, 0'\\) :-
	!.
% If bar below not not above, change to /
clean_up_parse_tree_line_char(0'|, _, 0'|, 0'/) :-
	!.
% Otherwise keep character unchanged
clean_up_parse_tree_line_char(Other, _, _, Other).

first_and_rest_or_null([F | R], F, R) :-
	!.
first_and_rest_or_null(_Other, null, null).

write_cleaned_parse_tree([]).
write_cleaned_parse_tree([F | R]) :-
	format('~N~w~n', [F]),
	!,
	write_cleaned_parse_tree(R).

print_file_assoc_generic(Assoc) :-
	format('~N~n------------------------------- FILES -------------------------------~n~n', []),
	assoc_generic_to_list(Assoc, List),
	length_of_longest_key_in_alist_list(List, 0-LongestLength),
	TabOffset is LongestLength + 2,
	print_file_assoc_generic_list(List, TabOffset),
	!.
print_file_assoc_generic(_Assoc).

length_of_longest_key_in_alist_list([], Length-Length).
length_of_longest_key_in_alist_list([Key-_Val | R], LengthIn-LengthOut) :-
	format_to_atom('~w', [Key], KeyAtom),
	atom_codes(KeyAtom, KeyChars),
	length(KeyChars, Length),
	(   Length > LengthIn ->
	    LengthNext = Length ;
	    LengthNext = LengthIn
	),
	!,
	length_of_longest_key_in_alist_list(R, LengthNext-LengthOut).	    

print_file_assoc_generic_list([], _TabOffset).
print_file_assoc_generic_list([F | R], TabOffset) :-
	print_file_assoc_generic_item(F, TabOffset),
	!,
	print_file_assoc_generic_list(R, TabOffset).

print_file_assoc_generic_item(Short-Long, TabOffset) :-
	format('~N~w:~*|~w', [Short, TabOffset, Long]),
	!.

%---------------------------------------------------------------

simple_prettyprint_parse_tree(no_tree) :-
	format('no_tree', []),
	!.
simple_prettyprint_parse_tree(Tree) :-
	simple_prettyprint_parse_tree(Tree, 0).

simple_prettyprint_parse_tree(LexNode, _Indent) :-
	lex_node(LexNode),
	simple_prettyprint_lex_node(LexNode),
	!.
simple_prettyprint_parse_tree(phrase(Mother, _LineInfo, Daughters), Indent) :-
	format('~w', [Mother]),
	Indent1 is Indent + 1,
	simple_prettyprint_daughters(Daughters, Indent1),
	!.
simple_prettyprint_parse_tree(Other, _Indent) :-
	regulus_error('Couldn''t simple_prettyprint tree: ~w~n', [Other]).

simple_prettyprint_lex_node(LexNode) :-
	simple_prettyprint_line(LexNode).

simple_prettyprint_line(phrase(Cat, _LineInfo, Daughters)) :-
	!,
	format('~w ', [Cat]),
	simple_prettyprint_line(Daughters).	
simple_prettyprint_line((F, R)) :-
	!,
	format('~w ', [F]),
	simple_prettyprint_line(R).
simple_prettyprint_line(Other) :-
	format('~w', [Other]),
	!.

simple_prettyprint_daughters(LexNode, Indent) :-
	lex_node(LexNode),
	!,
	nl, tab(2*Indent),
	simple_prettyprint_line(LexNode).
simple_prettyprint_daughters((F, R), Indent) :-
	!,
	nl, tab(2*Indent),
	simple_prettyprint_parse_tree(F, Indent),
	simple_prettyprint_daughters(R, Indent).
simple_prettyprint_daughters(LastDaughter, Indent) :-
	nl, tab(2*Indent),
	simple_prettyprint_parse_tree(LastDaughter, Indent),
	!.

%---------------------------------------------------------------

minimal_prettyprint_parse_tree(no_tree) :-
	format('no_tree', []),
	!.
minimal_prettyprint_parse_tree(phrase(Mother, _LineInfo, Daughters)) :-
	format('( ~w ', [Mother]),
	minimal_prettyprint_daughters(Daughters),
	format(' )', []),
	!.
minimal_prettyprint_parse_tree(lex(Lex)) :-
	format('~w', [Lex]),
	!.
minimal_prettyprint_parse_tree(Other) :-
	format('( ~w )', [Other]),
	!.

minimal_prettyprint_daughters((F, R)) :-
	!,
	minimal_prettyprint_parse_tree(F),
	format(' ', []),
	minimal_prettyprint_daughters(R).
minimal_prettyprint_daughters(LastDaughter) :-
	minimal_prettyprint_parse_tree(LastDaughter),
	!.

%---------------------------------------------------------------

lex_node(phrase(_Mother, _File, Daughters)) :-
	lex_daughters(Daughters).
lex_node(Daughters) :-
	lex_daughters(Daughters).

cat_and_words_for_lex_node(phrase(Cat, _File, Daughters), Cat, Words) :-
	words_for_lex_daughters(Daughters, Words-[]).

words_for_lex_daughters((F, R), In-Out) :-
	!,
	words_for_lex_daughters(F, In-Next),
	words_for_lex_daughters(R, Next-Out).
words_for_lex_daughters(empty_constituent, In-In) :-
	!.
words_for_lex_daughters(lex(Word), [Word | Out]-Out).

lex_daughters(Daughter) :-
	lex_daughter(Daughter).
lex_daughters((F, R)) :-
	lex_daughter(F),
	lex_daughters(R).

lex_daughter(empty_constituent).
lex_daughter(lex(_Item)).

%----------------------------------------------------------------------

:- dynamic stored_error/1.
:- dynamic error_storing_enabled/0.

init_stored_errors :-
	retractall(stored_error(_)),
	retractall(error_storing_enabled),
	assertz(error_storing_enabled).

format2error(Format, Args) :-
	format(user_error, Format, Args),
	flush_output(user_error),
	save_error_message_if_appropriate(Format, Args),
	!.

prettyprint2error(Term) :-
	prettyprint(user_error, Term).

prettyprintq2error(Term) :-
	prettyprintq(user_error, Term).

save_error_message_if_appropriate(Format, Args) :-
	should_store_errors,
	format_to_atom(Format, Args, Atom),
	assertz(stored_error(Atom)),
	!.
save_error_message_if_appropriate(_Format, _Args).

should_store_errors :-
	current_predicate(user:using_java_gui/0),
	!.
should_store_errors :-
	error_storing_enabled.

% We need the string version for the Java GUI, since Sicstus 3's PrologBeans
% runs into problems with empty strings.
get_stored_errors(ErrorString) :-
	findall(Atom, stored_error(Atom), Atoms),
	Atoms \== [],
	append_atoms(Atoms, 0'\n, JoinedAtom),
	atom_codes(JoinedAtom, ErrorString),
	retractall(error_storing_enabled),
	!.
get_stored_errors(ErrorString) :-
	ErrorString = "NO ERRORS".

get_stored_errors_list(Atoms) :-
	findall(Atom, stored_error(Atom), Atoms),
	retractall(error_storing_enabled),
	!.


%fake_sent_record(init_dialogue).
%fake_sent_record(set_notional_time(_)).
%fake_sent_record(set_notional_speaker(_)).

fake_sent_record(Record) :-
	\+ real_sent_record(Record).

real_sent_record(Record) :-
	compound(Record),
	functor(Record, sent, _N),
	arg(1, Record, Atom),
	atom(Atom),
	Atom \== '',
	atom_codes(Atom, Chars),
	\+ interpret_string_as_raw_lf_input(Chars, _LF).

get_sem_constants_from_sem(Var, In-In) :-
	var(Var),
	!.
get_sem_constants_from_sem(Atom, [Atom | Out]-Out) :-
	atomic(Atom),
	!.
get_sem_constants_from_sem(Term, [F | Next]-Out) :-
	compound(Term),
	functor(Term, F, N),
	get_sem_constants_from_sem_args(N, Term, Next-Out),
	!.
get_sem_constants_from_sem(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [get_sem_constants_from_sem(X, Y)]),
	fail.

get_sem_constants_from_sem_args(I, _Term, In-In) :-
	I =< 0,
	!.
get_sem_constants_from_sem_args(I, Term, In-Out) :-
	I > 0,
	arg(I, Term, Arg),
	get_sem_constants_from_sem(Arg, In-Next),
	I1 is I - 1,
	!,
	get_sem_constants_from_sem_args(I1, Term, Next-Out).

%---------------------------------------------------------------

frequency_preference_information_available_in_term(Term) :-
	term_contains_subterm(Term, frequency(_)),
	!.
	
%---------------------------------------------------------------

rule_frequency_preference_score(Tree, Score) :-
	rule_frequencies_in_tree(Tree, Frequencies-[]),
	geometric_mean_of_list(Frequencies, Score).

rule_frequencies_in_tree(V, In-In) :-
	( var(V) ; atomic(V) ),
	!.
rule_frequencies_in_tree(frequency(F), [F | Out]-Out) :-
	!.
rule_frequencies_in_tree(Term, In-Out) :-
	functor(Term, _F, N),
	!,
	rule_frequencies_in_tree_args(N, Term, In-Out).

rule_frequencies_in_tree_args(I, _Term, In-In) :-
	I =< 0,
	!.
rule_frequencies_in_tree_args(I, Term, In-Out) :-
	arg(I, Term, Arg),
	rule_frequencies_in_tree(Arg, In-Next),
	I1 is I - 1,
	!,
	rule_frequencies_in_tree_args(I1, Term, Next-Out).

%---------------------------------------------------------------

geometric_mean_of_list([], 1) :-
	!.
geometric_mean_of_list(List, Mean) :-
	length(List, N),
	sum_of_logs(List, 0-Sum),
	Mean is exp(Sum / N).

sum_of_logs([], In-In).
sum_of_logs([F | R], In-Out) :-
	Next is log(F) + In,
	!,
	sum_of_logs(R, Next-Out).

%---------------------------------------------------------------

csv_annotated_corpus_to_filtered_text_corpus(InFile, FilterAtom, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	csv_file_to_list_of_lists(InFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d records) ~w~n', [NIn, AbsInFile]),

	filter_annotated_corpus_list_using_atom(InList, FilterAtom, FilteredInList),
	length(FilteredInList, NFiltered),
	format('~N--- ~d records left after filtering~n', [NFiltered]),
	
	get_first_elements_of_lists(FilteredInList, OutList),

	length(OutList, NOut),
	write_atom_list_to_file(OutList, AbsOutFile),
	format('~N--- Written file (~d records) ~w~n', [NOut, AbsOutFile]),
	!.

filter_annotated_corpus_list_using_atom([], _FilterAtom, []).
filter_annotated_corpus_list_using_atom([F | R], FilterAtom, [F | R1]) :-
	keep_annotated_corpus_record_using_atom(F, FilterAtom),
	!,
	filter_annotated_corpus_list_using_atom(R, FilterAtom, R1).	
filter_annotated_corpus_list_using_atom([_F | R], FilterAtom, R1) :-
	!,
	filter_annotated_corpus_list_using_atom(R, FilterAtom, R1).

keep_annotated_corpus_record_using_atom([_Sent, _SemanticInterpretation | OtherInfo], FilterAtom) :-
	csv_fields_contain_word_ignoring_case(OtherInfo, FilterAtom),
	!.

%---------------------------------------------------------------

csv_annotated_corpus_to_text_corpus(InFile, CommandsToKeepFile, OutFile) :-
	read_commands_to_keep_file(CommandsToKeepFile, CommandsToKeepList),
	csv_annotated_corpus_to_text_corpus1(InFile, CommandsToKeepList, OutFile),
	!.

read_commands_to_keep_file(CommandsToKeepFile, CommandsToKeepList) :-
	safe_absolute_file_name(CommandsToKeepFile, AbsCommandsToKeepFile),
	csv_file_to_list_of_lists(AbsCommandsToKeepFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d records) ~w~n', [NIn, AbsCommandsToKeepFile]),
	get_first_elements_of_lists(InList, CommandsToKeepList),
	!.
read_commands_to_keep_file(CommandsToKeepFile, CommandsToKeepList) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [read_commands_to_keep_file(CommandsToKeepFile, CommandsToKeepList)]),
	fail.

csv_annotated_corpus_to_text_corpus(InFile, OutFile) :-
	csv_annotated_corpus_to_text_corpus1(InFile, any, OutFile).

csv_annotated_corpus_to_text_corpus1(InFile, CommandsToKeepList, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),

	csv_file_to_list_of_lists(InFile, InList),
	length(InList, NIn),
	format('~N--- Read file (~d records) ~w~n', [NIn, AbsInFile]),

	filter_annotated_corpus_list(InList, CommandsToKeepList, FilteredInList),
	length(FilteredInList, NFiltered),
	format('~N--- ~d records left after filtering~n', [NFiltered]),
	
	get_first_elements_of_lists(FilteredInList, OutList),

	length(OutList, NOut),
	write_atom_list_to_file(OutList, AbsOutFile),
	format('~N--- Written file (~d records) ~w~n', [NOut, AbsOutFile]),
	!.

filter_annotated_corpus_list([], _CommandsToKeepList, []).
filter_annotated_corpus_list([F | R], CommandsToKeepList, [F | R1]) :-
	keep_annotated_corpus_record(F, CommandsToKeepList),
	!,
	filter_annotated_corpus_list(R, CommandsToKeepList, R1).	
filter_annotated_corpus_list([_F | R], CommandsToKeepList, R1) :-
	!,
	filter_annotated_corpus_list(R, CommandsToKeepList, R1).

keep_annotated_corpus_record([_Sent, SemanticInterpretation | OtherInfo], CommandsToKeepList) :-
	\+ csv_fields_contain_word_ignoring_case(OtherInfo, ignore),
	(   ( is_list(CommandsToKeepList), CommandsToKeepList \== [] ) ->
	    split_atom_into_words(SemanticInterpretation, SemanticAtoms),
	    all_atoms_are_in_list(SemanticAtoms, CommandsToKeepList)
	;
	    otherwise ->
	    true
	),
	!.

all_atoms_are_in_list([], _List).
all_atoms_are_in_list([F | R], List) :-
	member(F, List),
	!,
	all_atoms_are_in_list(R, List).

get_first_elements_of_lists([], []).
get_first_elements_of_lists([[F | _OtherInfo] | R], [F | R1]) :-
	F \= '',
	!,
	get_first_elements_of_lists(R, R1).
get_first_elements_of_lists([_ | R], R1) :-
	!,
	get_first_elements_of_lists(R, R1).

%---------------------------------------------------------------

text_corpus_to_sent_corpus(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_file_to_atom_list(InFile, InList),
	length(InList, InN),
	sort(InList, UniquedInList),
	length(UniquedInList, UniquedInN),
	format('~N--- Read text file (~d records; ~d unique) ~w~n', [InN, UniquedInN, AbsInFile]),

	atom_list_to_sent_list(InList, OutList),
	length(OutList, OutN),

	list_to_prolog_file(OutList, AbsOutFile),
	format('~N--- Written Prolog file (~d records) ~w~n', [OutN, AbsOutFile]),
	!.
text_corpus_to_sent_corpus(InFile, OutFile) :-
	format2error('~N *** Error: bad call: ~w', [text_corpus_to_sent_corpus(InFile, OutFile)]),
	fail.

atom_list_to_sent_list([], []).
atom_list_to_sent_list([F | R], [sent(F) | R1]) :-
	!,
	atom_list_to_sent_list(R, R1).

%---------------------------------------------------------------

text_corpus_to_sorted_vocab_file(InFile, OutFile) :-
	safe_absolute_file_name(InFile, AbsInFile),
	safe_absolute_file_name(OutFile, AbsOutFile),
	
	read_file_to_atom_list(InFile, InList),
	length(InList, InN),
	format('~N--- Read text file (~d records) ~w~n', [InN, AbsInFile]),

	atom_list_to_sorted_vocab_list(InList, OutList),
	length(OutList, OutN),

	write_atom_list_to_file(OutList, AbsOutFile),
	format('~N--- Written text file (~d records) ~w~n', [OutN, AbsOutFile]),
	!.
text_corpus_to_sorted_vocab_file(InFile, OutFile) :-
	format2error('~N *** Error: bad call: ~w', [text_corpus_to_sorted_vocab_file(InFile, OutFile)]),
	fail.

atom_list_to_sorted_vocab_list(InList, OutList) :-
	findall(Word,
		(   member(Atom, InList),
		    split_atom_into_words(Atom, Words),
		    member(Word, Words)
		),
		AllWords),
	sort(AllWords, OutList),
	!.
