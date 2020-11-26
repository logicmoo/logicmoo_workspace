% ebl_include_lex.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

/*

- Add declarations saying which lexicon entries are to be imported
  - Specify list of subdomains
    - No specification = [default]
  - Specify category + optional surface or sem
  - Surface form specified exactly
  - Sem matches some part of sem
  - Can also allow negative declarations
    - Format: dont_include_lex/1 or /2
    - Negative take precedence over positive

- Extract entries from "reflective DCG" format after doing rest of EBL training
  - Expand disjunctive surface forms non-deterministically
  - "Context" is "lexicon"
  - "Tags" (last arg of raw EBL training result) from ebl_include_lex decl

Sample declarations:

include_lex(v:[]).
include_lex(n:[]).

include_lex(v:[words=start]).
include_lex(n:[words=(high, blood, pressure)]).

include_lex(v:[sem=start_happening]).
include_lex(n:[sem=high_blood_pressure]).

include_lex(v:[words=start], [default]).
include_lex(v:[sem=high_blood_pressure], [chest_pain, abdominal_pain]).

dont_include_lex(n:[sem=high_blood_pressure]).
dont_include_lex(n:[words=(high, blood, pressure)]).

Reflective DCG rule with disjunctive lexical elt:

dcg_clause(d(phrase(d,A),[agr=bv(0,B,B,C,C,1,1),article=bv(0,1,1),can_be_np=bv(0,0,1),def=bv(0,0,1),det_type=bv(0,0,0,1,1,1,1,1),prenumber=bv(0,0,1),syn_type=bv(0,0,0,0,0,0,1,1,1,1),wh=bv(0,0,1,1)],a,_,D,E), ('C'(D,a,E),A=lex(a);'C'(D,an,E),A=lex(an))).

Raw EBL training result:

rule((p(phrase(p,lex(on)),[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],[[prep,on]],_,A,B):-'C'(A,on,B)), (p(_,[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],_,_,_,_):-'C'(_,_,_)), [switch,on,the,light], [on], [default]).

*/

%---------------------------------------------------------------

:- module(ebl_include_lex,
	  [create_ebl_included_lex_entries/4]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(terms)).

%---------------------------------------------------------------

create_ebl_included_lex_entries(EBLIncludeLexFiles, StoredLexData, IgnoredSubdomains, LexEntries) :-
	internalise_include_files(EBLIncludeLexFiles, IgnoredSubdomains),
	create_lex_includes_from_conditional_lex_includes(StoredLexData, IgnoredSubdomains),
	create_ebl_included_lex_entries1(LexEntries),
	!.

%---------------------------------------------------------------

:- dynamic ebl_include_lex_decl/2, ebl_dont_include_lex_decl/2, ebl_conditional_include_lex_decl/4.

internalise_include_files(Files, IgnoredSubdomains) :-
	retractall(ebl_include_lex_decl(_, _)),
	retractall(ebl_dont_include_lex_decl(_, _)),
	retractall(ebl_conditional_include_lex_decl(_, _, _, _)),
	internalise_include_files1(Files, IgnoredSubdomains),
	!.
internalise_include_files(Files, IgnoredSubdomains) :-
	format2error('~N*** Error: bad call: ~w~n', [internalise_include_files(Files, IgnoredSubdomains)]),
	fail.
  
internalise_include_files1([], _IgnoredSubdomains) :-
	!.
internalise_include_files1(File, IgnoredSubdomains) :-
	\+ is_list(File),
	!,
	internalise_include_files1([File], IgnoredSubdomains).
internalise_include_files1([F | R], IgnoredSubdomains) :-
	internalise_include_file(F, IgnoredSubdomains),
	!,
	internalise_include_files1(R, IgnoredSubdomains).

internalise_include_file(File, IgnoredSubdomains) :-
	prolog_file_to_list(File, List),
	internalise_include_list(List, IgnoredSubdomains),
	!.

internalise_include_list([], _IgnoredSubdomains).
internalise_include_list([F | R], IgnoredSubdomains) :-
	internalise_include_item(F, IgnoredSubdomains),
	!,
	internalise_include_list(R, IgnoredSubdomains).

internalise_include_item((Head :- Body), _IgnoredSubdomains) :-
	parse_conditional_lex_include(Head, Body, Cat, Sem, Tags),
	assertz(ebl_conditional_include_lex_decl(Head, Cat, Sem, Tags)),
	!.
internalise_include_item(include_lex(Spec), IgnoredSubdomains) :-
	%assertz(ebl_include_lex_decl(Spec, [default])),
	internalise_include_item(include_lex(Spec, [default]), IgnoredSubdomains),
	!.
internalise_include_item(include_lex(Spec, Tags), IgnoredSubdomains) :-
	remove_ignored_subdomains_from_tags(Tags, IgnoredSubdomains, Tags1),
	(   Tags1 = [] ->
	    true ;
	    
	    ebl_include_lex_decl(Spec, Tags1) ->
	    true;
	    
	    assertz(ebl_include_lex_decl(Spec, Tags1))
	),
	!.
internalise_include_item(dont_include_lex(Spec), IgnoredSubdomains) :-
	%assertz(ebl_dont_include_lex_decl(Spec, [default])),
	internalise_include_item(dont_include_lex(Spec, [default]), IgnoredSubdomains),
	!.
internalise_include_item(dont_include_lex(Spec, Tags), IgnoredSubdomains) :-
	remove_ignored_subdomains_from_tags(Tags, IgnoredSubdomains, Tags1),
	(   Tags1 = [] ->
	    true ;

	    ebl_dont_include_lex_decl(Spec, Tags1) ->
	    true ;
	    
	    assertz(ebl_dont_include_lex_decl(Spec, Tags1))
	),
	!.
internalise_include_item(Item, IgnoredSubdomains) :-
	format2error('~N*** Error: bad call: ~w~n', [internalise_include_item(Item, IgnoredSubdomains)]),
	fail.

%---------------------------------------------------------------
/*

  include_lex(v:[sem=[Type, Value]], Tags) :-
	rule_exists(v:[sem=[[Type, Value]]], Tags).

  include_lex(v:[sem=[Type, Value]], Tags) :-
	rule_exists(v:[sem=[[tense, Tense], [Type, Value]]], Tags).

*/

parse_conditional_lex_include(Head, Body, Cat, BodySem, BodyTags) :-
	Head = include_lex(Cat:[sem=_HeadSem], _HeadTags),
	Body = rule_exists(Cat:[sem=BodySem], BodyTags),
	all_vars_in_head_are_in_body(Head, Body),
	!.

all_vars_in_head_are_in_body(Head, Body) :-
	term_variables(Head, HeadVars),
	term_variables(Body, BodyVars),
	all_vars_in_first_arg_are_in_second_arg(HeadVars, BodyVars).

all_vars_in_first_arg_are_in_second_arg(HeadVars, BodyVars) :-
	\+ var_is_in_first_arg_but_not_in_second(_Var, HeadVars, BodyVars).

var_is_in_first_arg_but_not_in_second(Var, HeadVars, BodyVars) :-
	member(Var, HeadVars),
	\+ id_member(Var, BodyVars).

%---------------------------------------------------------------

create_lex_includes_from_conditional_lex_includes(StoredLexData, IgnoredSubdomains) :-
	get_all_stored_conditional_lex_includes(ConditionalLexIncludes),
	create_lex_includes_from_conditionals(ConditionalLexIncludes, StoredLexData, IgnoredSubdomains),
	!.
create_lex_includes_from_conditional_lex_includes(StoredLexData, IgnoredSubdomains) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [create_lex_includes_from_conditional_lex_includes(StoredLexData, IgnoredSubdomains)]),
	fail.

get_all_stored_conditional_lex_includes(ConditionalLexIncludes) :-
	findall([Rule, Cat, Sem, Tags],
		ebl_conditional_include_lex_decl(Rule, Cat, Sem, Tags),
		ConditionalLexIncludes).

create_lex_includes_from_conditionals([], _StoredLexData, _IgnoredSubdomains).
create_lex_includes_from_conditionals([F | R], StoredLexData, IgnoredSubdomains) :-
	create_lex_includes_from_conditional(F, StoredLexData, IgnoredSubdomains),
	!,
	create_lex_includes_from_conditionals(R, StoredLexData, IgnoredSubdomains).

create_lex_includes_from_conditional(_Record, [], _IgnoredSubdomains).
create_lex_includes_from_conditional(Record, [F | R], IgnoredSubdomains) :-
	create_lex_include_from_conditional_and_lex_item(Record, F, IgnoredSubdomains),
	!,
	create_lex_includes_from_conditional(Record, R, IgnoredSubdomains).

create_lex_include_from_conditional_and_lex_item(CondRecord, LexRecord, IgnoredSubdomains) :-
	copy_term(CondRecord, CondRecord1),
	CondRecord1 = [Rule, Cat, Sem, Tags],
	LexRecord = stored_lex_data(Cat, Sem, Tags),
	internalise_include_item(Rule, IgnoredSubdomains),
	%format('~N~nSuccessful call: ~w~n',
	%       create_lex_include_from_conditional_and_lex_item(CondRecord, LexRecord, IgnoredSubdomains)),
	!.
create_lex_include_from_conditional_and_lex_item(_CondRecord, _LexRecord, _IgnoredSubdomains).

%---------------------------------------------------------------


remove_ignored_subdomains_from_tags(Tags, _IgnoredSubdomains, Tags1) :-
	\+ is_list(Tags),
	Tags = Tags1,
	!.
remove_ignored_subdomains_from_tags(Tags, IgnoredSubdomains, Tags1) :-
	is_list(Tags),
	list_to_ord_set(Tags, TagsOS),
	ord_subtract(TagsOS, IgnoredSubdomains, Tags1),
	!.
remove_ignored_subdomains_from_tags(Tags, IgnoredSubdomains, Tags1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [remove_ignored_subdomains_from_tags(Tags, IgnoredSubdomains, Tags1)]),
	fail.

%---------------------------------------------------------------

create_ebl_included_lex_entries1(LexEntries) :-
	findall(LexEntry, ebl_included_lex_entry(LexEntry), LexEntries),
	!.

ebl_included_lex_entry(LexEntry) :-
	expanded_reflective_dcg_lex_entry(Head, Body),
	words_from_body(Body, Words),
	sem_and_cat_from_head(Head, Sem, Cat),
	entry_matches_decl(Cat, Words, Sem, Tags),
	filter_tags_using_negative_entries(Tags, Cat, Words, Sem, Tags1),
	%format('~N~n~w~n', [filter_tags_using_negative_entries(Tags, Cat, Words, Sem, Tags1)]),
	Tags1 \== [],
	format_lex_entry_as_raw_ebl_rule(Head, Body, Tags, LexEntry).

%---------------------------------------------------------------

filter_tags_using_negative_entries(TagsIn, Cat, Words, Sem, TagsOut) :-
	findall(BadTags, entry_matches_negative_decl(Cat, Words, Sem, BadTags), AllBadTags),
	append_list(AllBadTags, AllBadTagsList),
	findall(Tag,
		(   member(Tag, TagsIn),
		    \+ member(Tag, AllBadTagsList)
		),
		TagsOut),
	!.
filter_tags_using_negative_entries(TagsIn, _Cat, _Words, _Sem, TagsIn) :-
	!.
filter_tags_using_negative_entries(Tags, Cat, Words, Sem, Tags1) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [filter_tags_using_negative_entries(Tags, Cat, Words, Sem, Tags1)]),
	fail.

%---------------------------------------------------------------

/*

Reflective DCG rule with disjunctive lexical elt:

dcg_clause(d(phrase(d,A),[agr=bv(0,B,B,C,C,1,1),article=bv(0,1,1),can_be_np=bv(0,0,1),def=bv(0,0,1),det_type=bv(0,0,0,1,1,1,1,1),prenumber=bv(0,0,1),syn_type=bv(0,0,0,0,0,0,1,1,1,1),wh=bv(0,0,1,1)],a,_,D,E), ('C'(D,a,E),A=lex(a);'C'(D,an,E),A=lex(an))).

*/

expanded_reflective_dcg_lex_entry(Head, Body) :-
	user:dcg_clause(Head0, Body0),
	(   normalise_prolog_dcg_clause_to_c_version((Head0 :- Body0), (Head1 :- Body1)) ->
	    true
	;
	    (Head0 :- Body0) = (Head1 :- Body1)
	),
	Head = Head1,
	expand_lexical_body(Body1, Body2),
	remove_trivial_conjuncts(Body2, Body).

expand_lexical_body((P ; Q), Expanded) :-
	!,
	(   expand_lexical_body(P, Expanded) ;
	    expand_lexical_body(Q, Expanded)
	).
expand_lexical_body((P, Q), (P1, Q1)) :-
	!,
	expand_lexical_body(P, P1),
	expand_lexical_body(Q, Q1).
expand_lexical_body(X = Y, true) :-
	X = Y,
	!.
expand_lexical_body('C'(In, Word, Out), 'C'(In, Word, Out)) :-
	!.

remove_trivial_conjuncts((P, true), P1) :-
	!,
	remove_trivial_conjuncts(P, P1).
remove_trivial_conjuncts((true, P), P1) :-
	!,
	remove_trivial_conjuncts(P, P1).
remove_trivial_conjuncts((P, Q), Result) :-
	!,
	remove_trivial_conjuncts(P, P1),
	remove_trivial_conjuncts(Q, Q1),
	(   ( P == P1, Q == Q1 ) ->
	    Result = (P1, Q1)
	;
	    otherwise ->
	    remove_trivial_conjuncts((P1, Q1), Result)
	).
remove_trivial_conjuncts(P, P).

%---------------------------------------------------------------

/*

Reflective DCG rule with disjunctive lexical elt:

dcg_clause(d(phrase(d,A),[agr=bv(0,B,B,C,C,1,1),article=bv(0,1,1),can_be_np=bv(0,0,1),def=bv(0,0,1),det_type=bv(0,0,0,1,1,1,1,1),prenumber=bv(0,0,1),syn_type=bv(0,0,0,0,0,0,1,1,1,1),wh=bv(0,0,1,1)],a,_,D,E), ('C'(D,a,E),A=lex(a);'C'(D,an,E),A=lex(an))).

*/

words_from_body((B1, B2), (W1, W2)) :-
	!,
	words_from_body(B1, W1),
	words_from_body(B2, W2).
words_from_body('C'(_From, Word, _To), Word) :-
	!.

sem_and_cat_from_head(Head, Sem, Cat) :-
	functor(Head, Cat, 6),
	arg(3, Head, Sem),
	!.
sem_and_cat_from_head(Head, Sem, Cat) :-
	format2error('~N*** Error: bad call: ~w~n', [sem_and_cat_from_head(Head, Sem, Cat)]),
	fail.

%---------------------------------------------------------------
	
entry_matches_decl(Cat, Words, Sem, Tags) :-
	ebl_include_lex_decl(Decl, Tags),
	\+ cat_fails_to_match_decl(Cat, Decl),
	\+ words_fail_to_match_decl(Words, Decl),
	\+ sem_fails_to_match_decl(Sem, Decl).
	%format('~N~nSuccessful call: (decl = ~q): ~q~n',
	%       [ebl_include_lex_decl(Decl, Tags), entry_matches_decl(Cat, Words, Sem, Tags)]).

entry_matches_negative_decl(Cat, Words, Sem, Tags) :-
	ebl_dont_include_lex_decl(Cat:Body, Tags),
	(   Body = [words=Words1] ->
	    Words = Words1
	;
	    Body = [sem=Sem1] ->
	    term_contains_subterm(Sem, Sem1)
	),
	!.

cat_fails_to_match_decl(Cat, Decl) :-
	Decl = Cat1:_Rest,
	Cat \== Cat1,
	!.

words_fail_to_match_decl(Words, Decl) :-
	Decl = _Cat:[words=Words1],
	Words \== Words1,
	!.

sem_fails_to_match_decl(Sem, Decl) :-
	Decl = _Cat:[sem=Sem1],
	\+ term_contains_subterm(Sem, Sem1),
	!.

%---------------------------------------------------------------

/*

Raw EBL training result:

rule((p(phrase(p,lex(on)),[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],[[prep,on]],_,A,B):-'C'(A,on,B)), (p(_,[def=_,obj_sem_n_type=bv(0,0,0,0,0,0,0,1,1,1,1,1,1),postposition=bv(0,0,1),sem_p_type=bv(0,1,1,1,1),sem_pp_type=bv(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)],_,_,_,_):-'C'(_,_,_)), [switch,on,the,light], [on], [default]).

*/

format_lex_entry_as_raw_ebl_rule(Head0, Body0, Tags, LexEntry) :-
	change_format_of_lexical_dcg_rule_if_sicstus4((Head0 :- Body0), (Head :- Body)),
	LexEntry = rule((Head :- Body),
			(Head :- Body),
			[lexicon],
			[lexicon],
			Tags),
	!.
format_lex_entry_as_raw_ebl_rule(Head, Body, Tags, LexEntry) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [format_lex_entry_as_raw_ebl_rule(Head, Body, Tags, LexEntry)]),
	fail.

/*
This ugly hack is caused by the fact that Sicstus 3 and Sicstus 4 represent DCGs
differently. If we're in Sicstus 4, we need to force the lexical rule into
the Sicstus 4 DCG format.
*/

change_format_of_lexical_dcg_rule_if_sicstus4(Rule, Rule) :-
	\+ user:sicstus_version(4),
	!.
change_format_of_lexical_dcg_rule_if_sicstus4((Head0 :- Body0), (Head :- true)) :-
	copy_term((Head0 :- Body0), (Head :- Body)),
	evaluate_dcg_body(Body),
	!.
change_format_of_lexical_dcg_rule_if_sicstus4(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [change_format_of_lexical_dcg_rule_if_sicstus4(X, Y)]),
	fail.

evaluate_dcg_body('C'(In, Word, Out)) :-
	In = [Word | Out],
	!.
evaluate_dcg_body((P, Q)) :-
	evaluate_dcg_body(P),
	evaluate_dcg_body(Q),
	!.
evaluate_dcg_body(true) :-
	!.
evaluate_dcg_body(Other) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [evaluate_dcg_body(Other)]),
	fail.
