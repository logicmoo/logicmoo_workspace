/*
I tried to separate out 3 functions from the G2R compiler that contain source code that RIACS owns,
and can be contributed to Regulus.   I may not have gotten all the predicates necessary, or there
may be some that are necessary in Gemini, but not in Regulus.

Attached are mjohnson_lrec.pl, kiefer.pl, and compaction.pl.
*/

mark_johnson_transform_proper :-
	clear_dynamic_predicates([new_cfg_rule/2]),
	compute_left_rec_table, % compute this again
	find_retained_nonterminals,
	mark_johnson_rule1,
	cfg_rule(NT, Body),
	apply_transform_list(Body, NT),
	fail.

mark_johnson_transform:-
	mark_johnson_transform_proper,
	fail.
mark_johnson_transform:-
	mj_remove_empties,
	mj_remove_useless_nonterminals,
	num_clauses(new_cfg_rule(_,_), N),
	format('Found ~d rules after applying Left-recursion elimination~n', [N]),
	copy_to_cfg_rule.

copy_to_cfg_rule :-
	clear_dynamic_predicates([cfg_rule/2]),
	atomic_cat(_Cat,AtomicCat),
	mj_helper(AtomicCat,RHSList),
	\+ RHSList = [],
	sort(RHSList, FinalRHS),
	assert(cfg_rule(AtomicCat,FinalRHS)),
	fail.
copy_to_cfg_rule.

new_mj_nt(lt(A), lt(X), AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(A, lt(X), AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(lt(A), X, AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).
new_mj_nt(A, X, AX):-
	atomic(A),
	atomic(X),
	!,
	concat_atom([A,'-',X], AX),
	assert_once(atomic_cat(introduced_by_mj, AX)).


mj_helper(AtomicCat,[FirstRHS|RestRHSs]) :-
	retract(new_cfg_rule(AtomicCat,FirstRHS)),
	!,
	mj_helper(AtomicCat,RestRHSs).
mj_helper(_AtomicCat,[]).


mark_johnson_rule1 :-
	left_corner(A, X),
	left_recursive(A),
	retained_nonterminal(A),
	(X = lt(_) -> true ; \+left_recursive(X)),
	new_mj_nt(A, X, AX),
	assert(new_cfg_rule(A, [X, AX])),
	fail.
mark_johnson_rule1.


apply_transform_list([], _NT).
apply_transform_list([Body|RestList], NT):-
	apply_transform(Body, NT),
	apply_transform_list(RestList, NT).

/*
% rules for the original Mark Johnson Transform
apply_transform([X|Beta], B):-
	left_corner(A,B),
	new_mj_nt(A, X, AX),
	new_mj_nt(A, B, AB),
	append(Beta, [AB], NewBody),
	assert(new_cfg_rule(AX,NewBody)),
	fail.
apply_transform([X|Beta], A):-
	left_corner(A, X),
	new_mj_nt(A, X, AX),
	assert(new_cfg_rule(AX,Beta)),
	fail.
apply_transform(_, _).
*/

% rule 2
apply_transform([X|Beta], B):-
	left_recursive(B),
	left_corner(A, B),
	left_recursive(A),
	retained_nonterminal(A),
	new_mj_nt(A, X, AX),
	new_mj_nt(A, B, AB),
	append(Beta, [AB], NewBody),
	assert(new_cfg_rule(AX,NewBody)),
	fail.

% rule 3
apply_transform([X|Beta], A):-
	left_recursive(A),
	retained_nonterminal(A),
	left_corner(A, X),
	new_mj_nt(A, X, AX),
	assert(new_cfg_rule(AX,Beta)),
	fail.

% rule 4
apply_transform(Beta, A):-
	\+ left_recursive(A),
	assert(new_cfg_rule(A,Beta)),
	fail.

apply_transform(_, _).

mj_remove_empties:-
	clear_dynamic_predicates([mj_empty/1]),
	format('Entering empty elimination~n', []),
	ctr_set(5,0),
	retract(new_cfg_rule(AtomicCat, [])),
	assert(mj_empty(AtomicCat)),
	format(' found new empty cat ~a~n', [AtomicCat]),
	fail.
mj_remove_empties:-
	mj_remove_empties_iterate.

mj_remove_empties_iterate:-
	new_cfg_rule(AtomicCat, Body),
	mj_remove_empty_nonterminals(Body, RestBody),
	\+ Body = RestBody,
	(RestBody = [] ->
assert_unique(mj_empty(AtomicCat)),
	     format(' found new empty cat ~a~n', [AtomicCat]),
	     ctr_set(5, 1)
	;    assert_unique(new_cfg_rule(AtomicCat, RestBody))),
	fail.
mj_remove_empties_iterate:-
	ctr_is(5,1),
	!,
	ctr_set(5,0),
	num_clauses(new_cfg_rule(_,_), Rules),
	format('Found ~d derived rules during empty elimination~n', [Rules]),
	mj_remove_empties_iterate.
mj_remove_empties_iterate:-
	mj_eliminate_useless_rules,
	num_clauses(new_cfg_rule(_,_), Rules),
	format('Found ~d rules after empty elimination~n', [Rules]).

mj_eliminate_useless_rules:-
	new_cfg_rule(AtomicCat, Body),
	mj_useless_body(Body),
	retract(new_cfg_rule(AtomicCat, Body)),
	fail.
mj_eliminate_useless_rules.

mj_useless_body([lt(_Head)|Tail]):-
	!,
	mj_useless_body(Tail).
mj_useless_body([Head|_Tail]):-
	\+ new_cfg_rule(Head, _),
	!.
mj_useless_body([_Head|Tail]):-
	mj_useless_body(Tail).

mj_remove_empty_nonterminals([], []).
mj_remove_empty_nonterminals([Empty|Rest], Result):-
	mj_empty(Empty),
	!,
	mj_remove_empty_nonterminals(Rest, Result).
mj_remove_empty_nonterminals([Head|Rest], [Head|Result]):-
	mj_remove_empty_nonterminals(Rest, Result).


mj_remove_useless_nonterminals:-
	retract(new_cfg_rule(X, [X])),
	fail.
mj_remove_useless_nonterminals:-
	clear_dynamic_predicates([reachable_nt/1]),
	mj_top_down(sigma),
	mj_remove_nonreachables,
	num_clauses(new_cfg_rule(_,_), Rules),
	format('Found ~d rules after removing useless productions~n', [Rules]).

mj_top_down(NT):-
	reachable_nt(NT),
	!.
mj_top_down(NT):-
	assert(reachable_nt(NT)),
	new_cfg_rule(NT, Body),
	mj_top_down_body(Body),
	fail.
mj_top_down(_).

mj_top_down_body([]).
mj_top_down_body([Head|Body]):-
	mj_top_down(Head),
	mj_top_down_body(Body).

mj_remove_nonreachables:-
	new_cfg_rule(NT, Body),
	\+ reachable_nt(NT),
	retract(new_cfg_rule(NT, Body)),
	fail.
mj_remove_nonreachables.







kiefer :-
	clear_dynamic_predicates([kiefer_category/1]),
	preprocess_kiefer_lexicon,
	preprocess_kiefer_rules,
	ctr_set(5, 0),
	iterate_bottom_up(1),
	instantiate_kiefer_cfg_rules.


preprocess_kiefer_lexicon :-
	clear_dynamic_predicates([kiefer_category/1,kiefer_lex/2]),
	switch_value(rename_feats,RenameFeats),
	switch_value(canonical_only,CanonicalOnly),
	dynamic_prefix(Prefix),
	cat(Word,FullCat),
	filter_features(FullCat, Cat),
	(CanonicalOnly = yes ->
check_canonical(Cat)
	;    true),
	process_dynamic_lex_entry(Word,Prefix,Cat,NewWord),
	( RenameFeats = true ->
instantiate_rename_feat(Cat)
	;    true),
%	mark_vals_cat(Cat, NewCat),
%	mark_singletons(Cat, true),
	assert_general(kiefer_lex(Cat, [NewWord])),
	assert_general(kiefer_category(Cat)),
	fail.
preprocess_kiefer_lexicon :-
	% This filters out single word "multiwords", which are
	% used only with typed input (typically numerals)
	multi_word_cat(LastWord,[PrevWord|RestWords],FullCat),
	filter_features(FullCat, Cat),
	(switch_value(canonical_only,yes) ->
check_canonical(Cat)
	;    true),
	(switch_value(rename_feats,true) ->
instantiate_rename_feat(Cat)
	;    true),
	rev([LastWord,PrevWord|RestWords],MultiWordList),
%	mark_vals_cat(Cat, NewCat),
%	mark_singletons(Cat, true),
	assert_general(kiefer_lex(Cat,MultiWordList)),
	assert_general(kiefer_category(Cat)),
	fail.
preprocess_kiefer_lexicon:-
	num_clauses(kiefer_category(_), Cat),
	num_clauses(kiefer_lex(_,_), U),
	format('Found ~d unique categories for a lexicon of size ~d~n', 
	       [Cat, U]).

preprocess_kiefer_rules:-
	clear_dynamic_predicates([restricted_rule/2]),
	((top_down_rule(FullMother,FullDaughters,RuleName),
	  (start_cat(FullMother), if_exists(application_sigma_rule(_,_,_,_,_)) ->
application_sigma_rule(_,_,RuleName,yes,_)  % only use sigma rules that have recognition flag = yes
	  ;    true))
	; rule(FullMother,FullDaughters,RuleName)),
	(ctr_set(4, 0) ;
	 (ctr_is(4, ThisRuleCount),
	  (ThisRuleCount > 1 ->
format('Rule ~a derived ~d rules~n', [RuleName, ThisRuleCount]),
	       fail
	  ; fail))),
	filter_rules([FullMother|FullDaughters], [Mother|Daughters]),
%	mark_vals_cat(Mother,MarkedMother),
%	mark_vals_daus(Daughters,MarkedDaughters),
%	mark_singletons(Mother, Daughters),
	ctr_inc(4),
	assert_unique(restricted_rule(Mother, Daughters)),
	fail.
preprocess_kiefer_rules:-
	num_clauses(restricted_rule(_,_), NumDerivedRules),
	num_clauses(top_down_rule(_,_,_,_), T),
	num_clauses(rule(_,_,_,_), R),
	TotalRule is T + R,
	assert(gemini_rule_count(TotalRule)),
	format('Found ~d derived rules from a grammar of size ~d~n', 
	       [NumDerivedRules, TotalRule]).

iterate_bottom_up(_) :-
	clear_dynamic_predicates([new_kiefer_category/1]),
	ctr_set(5, 0), % setting a arbitary counter to 0
	restricted_rule(Mother, Daughters),
	kiefer_match_daughters(Daughters),
	assert_general(new_kiefer_category(Mother)),
	fail.
iterate_bottom_up(_):-
	retract(new_kiefer_category(Cat)),
	assert_general(kiefer_category(Cat)),
	ctr_set(5, 1),
	fail.
iterate_bottom_up(N):-
	NextN is N + 1,
	num_clauses(kiefer_category(_), C),
	format('Found ~d categories after ~d iterations~n', [C,N]),
	(ctr_is(5, 1) ->
iterate_bottom_up(NextN)
	;    true).

kiefer_match_daughters([]).
kiefer_match_daughters([Dau|RestDaus]):-
	kiefer_category(Dau),
	kiefer_match_daughters(RestDaus).

kiefer_cfg_term(sigma, sigma):-
	assert_unique(atomic_cat(sigma, sigma)),
	!.
kiefer_cfg_term(Cat, AtomicCat):-
	copy_term(Cat, Copy),
	numbervars(Copy, 0, _),
	clause(atomic_cat(Copy, AtomicCat), _, Ref),
	instance(Ref, (atomic_cat(FreshCopy, AtomicCat):-true)),
	numbervars(FreshCopy, 0, _),
	FreshCopy = Copy,
	!.
kiefer_cfg_term(Cat, AtomicCat):-	
	functor(Cat, Functor, _),
	gensym(Functor, AtomicCat),
	assert(atomic_cat(Cat, AtomicCat)).


subsuming_mother(Cat, AtomicCat):-
	copy_term(Cat, Copy),
	clause(atomic_cat(Copy, AtomicCat), _, Ref),
	instance(Ref, (atomic_cat(FreshCopy, AtomicCat):-true)),
	numbervars(Copy, 0, _),
	FreshCopy = Copy.
/*
subsuming_mother(Cat, AtomicCat):-
	copy_term(Cat, Copy),
	clause(atomic_cat(Copy, AtomicCat), _, Ref),
	instance(Ref, (atomic_cat(FreshCopy, AtomicCat):-true)),
	numbervars(FreshCopy, 0, _),
	FreshCopy = Copy.
subsuming_mother(Cat, AtomicCat):-
	atomic_cat(Cat, AtomicCat).
*/

instantiate_kiefer_cfg_rules_helper :-
	kiefer_category(Cat),
%	find_singleton_daughter_vars(false,[Cat],SingletonList),
%	instantiate_singletons(SingletonList),
	kiefer_cfg_term(Cat, _AtomicCat),
	fail.

instantiate_kiefer_cfg_rules:-
	clear_dynamic_predicates([atomic_cat/2]),
	instantiate_kiefer_cfg_rules_helper.

instantiate_kiefer_cfg_rules :-
	clear_dynamic_predicates([atomic_cat_rule/2]),
	add_kiefer_lexical_rules,
	restricted_rule(Mother, Daughters),
	instantiate_daughters(Daughters, AtomicDaughters),
%	atomic_cat(Mother, AtomicMother),
	subsuming_mother(Mother, AtomicMother),
	assert_unique(atomic_cat_rule(AtomicMother, AtomicDaughters)),
	fail.
instantiate_kiefer_cfg_rules :-
	num_clauses(atomic_cat_rule(_,_), Rules),
	format('Found ~d total rules~n', [Rules]).



instantiate_daughters([],[]).
instantiate_daughters([Dau|RestDaus], [AtomicDau|RestAtomicDaus]):-
	atomic_cat(Dau, AtomicDau),
	instantiate_daughters(RestDaus, RestAtomicDaus).

add_kiefer_lexical_rules:-
	kiefer_lex(Cat, Words),
	subsuming_mother(Cat, AtomicCat),
	add_lt_wrapper(Words, LtWords),
	assert(atomic_cat_rule(AtomicCat, LtWords)),
	fail.
add_kiefer_lexical_rules.

add_lt_wrapper([], []).
add_lt_wrapper([Word|RestWords], [lt(Word)|RestResult]):-
	add_lt_wrapper(RestWords, RestResult).

kiefer_remove_empties:-
	clear_dynamic_predicates([kiefer_empty/1]),
	format('Entering empty elimination~n', []),
	ctr_set(5,0),
	retract(atomic_cat_rule(AtomicCat, [])),
	assert(kiefer_empty(AtomicCat)),
	format(' found new empty cat ~a~n', [AtomicCat]),
	fail.
kiefer_remove_empties:-
	kiefer_remove_empties_iterate.

kiefer_remove_empties_iterate:-
	atomic_cat_rule(AtomicCat, Body),
	remove_empty_nonterminals(Body, RestBody),
	\+ Body = RestBody,
	(RestBody = [] ->
assert_unique(kiefer_empty(AtomicCat)),
	     format(' found new empty cat ~a~n', [AtomicCat]),
	     ctr_set(5, 1)
	;    assert_unique(atomic_cat_rule(AtomicCat, RestBody))),
	fail.
kiefer_remove_empties_iterate:-
	ctr_is(5,1),
	!,
	ctr_set(5,0),
	num_clauses(atomic_cat_rule(_,_), Rules),
	format('Found ~d derived rules during empty elimination~n', [Rules]),
	kiefer_remove_empties_iterate.
kiefer_remove_empties_iterate:-
	kiefer_eliminate_useless_rules,
	num_clauses(atomic_cat_rule(_,_), Rules),
	format('Found ~d rules after empty elimination~n', [Rules]).

kiefer_eliminate_useless_rules:-
	atomic_cat_rule(AtomicCat, Body),
	kiefer_useless_body(Body),
	retract(atomic_cat_rule(AtomicCat, Body)),
	fail.
kiefer_eliminate_useless_rules.

kiefer_useless_body([lt(_Head)|Tail]):-
	!,
	kiefer_useless_body(Tail).
kiefer_useless_body([Head|_Tail]):-
	\+ atomic_cat_rule(Head, _),
	!.
kiefer_useless_body([_Head|Tail]):-
	kiefer_useless_body(Tail).

remove_empty_nonterminals([], []).
remove_empty_nonterminals([Empty|Rest], Result):-
	kiefer_empty(Empty),
	!,
	remove_empty_nonterminals(Rest, Result).
remove_empty_nonterminals([Head|Rest], [Head|Result]):-
	remove_empty_nonterminals(Rest, Result).


kiefer_remove_useless_nonterminals:-
	retract(atomic_cat_rule(X, [X])),
	fail.
kiefer_remove_useless_nonterminals:-
	clear_dynamic_predicates([reachable_nt/1]),
	kiefer_top_down(sigma),
	kiefer_remove_nonreachables,
	num_clauses(atomic_cat_rule(_,_), Rules),
	format('Found ~d rules after removing useless productions~n', [Rules]).

kiefer_top_down(NT):-
	reachable_nt(NT),
	!.
kiefer_top_down(NT):-
	assert(reachable_nt(NT)),
	atomic_cat_rule(NT, Body),
	kiefer_top_down_body(Body),
	fail.
kiefer_top_down(_).

kiefer_top_down_body([]).
kiefer_top_down_body([Head|Body]):-
	kiefer_top_down(Head),
	kiefer_top_down_body(Body).

kiefer_remove_nonreachables:-
	atomic_cat_rule(NT, Body),
	\+ reachable_nt(NT),
	retract(atomic_cat_rule(NT, Body)),
	fail.
kiefer_remove_nonreachables.



remove_redundant_rules:-
	num_clauses(cfg_rule(_,_), NumRules),
	format('removing redundant nonterminals: ~d total rules~n', [NumRules]),
	clear_dynamic_predicates([rewrite_nt/2]),
	ctr_set(5, 0),
	cfg_rule(NonTerminal, RHS),
	cfg_rule(OtherNonTerminal, RHS),

	\+ NonTerminal = OtherNonTerminal,
	\+ rewrite_nt(NonTerminal, OtherNonTerminal),
	\+ rewrite_nt(_, OtherNonTerminal),
	\+ rewrite_nt(OtherNonTerminal, _),
	\+ rewrite_nt(_, NonTerminal),
	\+ rewrite_nt(NonTerminal, _),
	retract(cfg_rule(OtherNonTerminal, RHS)),
	assert(rewrite_nt(OtherNonTerminal, NonTerminal)),
	ctr_set(5,1),
	fail.
remove_redundant_rules:-
	cfg_rule(NonTerminal, [[OtherNonTerminal]]),
	NonTerminal \== sigma,
	\+ NonTerminal = OtherNonTerminal,
	\+ rewrite_nt(NonTerminal, OtherNonTerminal),
	\+ rewrite_nt(OtherNonTerminal, NonTerminal),
	\+ rewrite_nt(_, NonTerminal),
	\+ rewrite_nt(NonTerminal, _),
	\+ rewrite_nt(_, OtherNonTerminal),
	\+ rewrite_nt(OtherNonTerminal, _),
	retract(cfg_rule(NonTerminal, [[OtherNonTerminal]])),
	assert(rewrite_nt(NonTerminal, OtherNonTerminal)),
	ctr_set(5,1),
	fail.
remove_redundant_rules:-
	update_rule_set,
	fail.
remove_redundant_rules:-
	retract(cfg_rule(NT, RHS)),
	clean_up_rhs(RHS, NT, NewRHS),
	(RHS \== NewRHS ->
ctr_set(5,1)
%	     format('  Removing redundant rule option for ~p~n     ~p~n', 
%		    [(NT->RHS),(NT->NewRHS)])
	;    true),
	sort(NewRHS, FinalRHS),
	asserta(cfg_rule(NT, FinalRHS)),
	fail.
remove_redundant_rules:-
	(ctr_is(5,1) ->
remove_redundant_rules
	;    num_clauses(cfg_rule(_,_), NumRules),
	     format('CFG rules after duplicate NT removal: ~d~n', [NumRules])).

update_rule_set:-
	retract(cfg_rule(NT, RHS)),
	substitute_in_rhs(RHS, NewRHS),
	asserta(cfg_rule(NT, NewRHS)),
	fail.
update_rule_set.




% eliminate productions that are duplicated in the rule
clean_up_rhs([], _NT, []).
clean_up_rhs([Head|Tail], NT, Result):-
	member(Head,Tail),
	!,
	clean_up_rhs(Tail, NT, Result).
clean_up_rhs([Head|Tail], NT, [Head|Result]):-
	clean_up_rhs(Tail, NT, Result).

substitute_in_rhs([], []):-
	!.
substitute_in_rhs([Head|Tail], [NewHead|NewTail]):-
	!,
	substitute_in_rhs(Head, NewHead),
	substitute_in_rhs(Tail, NewTail).
substitute_in_rhs(Symbol, NewSymbol):-
	atomic(Symbol),
	!,
	(rewrite_nt(Symbol, NewSymbol) -> true
	;    Symbol = NewSymbol).
substitute_in_rhs(Symbol, Symbol).


destructively_build_rule_list(AtomicCat,[FirstRHS|RestRHSs]) :-
	retract(atomic_cat_rule(AtomicCat,FirstRHS)),
	!,
	destructively_build_rule_list(AtomicCat,RestRHSs).
destructively_build_rule_list(_AtomicCat,[]).

