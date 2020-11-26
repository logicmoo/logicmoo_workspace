:- ensure_loaded('$REGULUS/PrologLib/compatibility').
 
%---------------------------------------------------------------
 
:- module(regulus_check_recursion,
	  [check_rules_for_recursion/1]
      ).

%---------------------------------------------------------------

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
 
%---------------------------------------------------------------

:- dynamic mother/1, mother_daughter/2, cat_has_been_checked/1.

check_rules_for_recursion(Rules) :-
	format('~N--- Checking rules for recursion~n', []),
	init_check_rules_for_recursion(Rules),
	all_mothers(Mothers),
	check_rules_for_recursion1(Mothers, RecursiveP),
	(   RecursiveP = recursive(Chain) ->
	    format('~N--- Rules are recursive through chain:~n~n', []),
	    prettyprint(Chain),
	    nl, nl
	;
	    otherwise ->
	    format('~N--- Rules are not recursive~n', [])
	),
	!.
check_rules_for_recursion(_Rules) :-
	regulus_error('~NChecking for recursiveness of CFG rules failed~n', []).

%---------------------------------------------------------------

init_check_rules_for_recursion(Rules) :-
	retractall(mother(_)),
	retractall(mother_daughter(_, _)),
	retractall(cat_has_been_checked(_)),
	store_mothers_and_daughters(Rules).

store_mothers_and_daughters([]).
store_mothers_and_daughters([F | R]) :-
	store_mothers_and_daughters1(F),
	store_mothers_and_daughters(R).

store_mothers_and_daughters1(Mother-RuleList) :-
	store_mother(Mother),
	store_mothers_and_daughters2(RuleList, Mother).

store_mothers_and_daughters2([], _Mother).
store_mothers_and_daughters2([F | R], Mother) :-
	store_mothers_and_daughters_for_rule(F, Mother),
	!,
	store_mothers_and_daughters2(R, Mother).

store_mothers_and_daughters_for_rule(Rule, Mother) :-
	Rule = rule(( _Head --> Body ), _LabelInfo),
	store_mothers_and_daughters_for_rule_body(Body, Mother),
	!.
store_mothers_and_daughters_for_rule(Rule, Mother) :-
	format('~N*** Error: bad call: ~w~n', [store_mothers_and_daughters_for_rule(Rule, Mother)]),
	fail.

store_mothers_and_daughters_for_rule_body((P, Q), Mother) :-
	store_mothers_and_daughters_for_rule_body(P, Mother),
	store_mothers_and_daughters_for_rule_body(Q, Mother),
	!.
store_mothers_and_daughters_for_rule_body((?(P)), Mother) :-
	store_mothers_and_daughters_for_rule_body(P, Mother),
	!.
store_mothers_and_daughters_for_rule_body(cat(Daughter, _SynFeatsWithVals, _Value), Mother) :-
	store_mother_and_daughter(Mother, Daughter),
	!.
store_mothers_and_daughters_for_rule_body(_Other, _Mother) :-
	!.

store_mother(Mother) :-
	(   mother(Mother) ->
	    true
	;
	    otherwise ->
	    assertz(mother(Mother))
	).

store_mother_and_daughter(Mother, Daughter) :-
	(   mother_daughter(Mother, Daughter) ->
	    true
	;
	    otherwise ->
	    assertz(mother_daughter(Mother, Daughter))
	).

all_mothers(Mothers) :-
	findall(Mother, mother(Mother), Mothers).

%---------------------------------------------------------------

check_rules_for_recursion1([], not_recursive) :-
	!.
check_rules_for_recursion1([F | _R], recursive(Chain)) :-
	find_recursion(F, []-Chain0),
	format_recursion_chain_for_presentation(Chain0, Chain),
	!.
check_rules_for_recursion1([_F | R], RecursiveP) :-
	!,
	check_rules_for_recursion1(R, RecursiveP).

% We have already visiting this cat, and it's in the current chain. Recursion.
find_recursion(Cat, ChainIn-ChainOut) :-
	cat_has_been_checked(Cat),
	member(Cat, ChainIn),
	ChainOut = [Cat | ChainIn],
	!.
% We have visited this cat in other circumstances, but found no recursion. No point looking further.
find_recursion(Cat, _ChainIn-_ChainOut) :-
	cat_has_been_checked(Cat),
	!,
	fail.
% Mark cat as checked, find a possible daughter, and carry on looking
find_recursion(Cat, ChainIn-ChainOut) :-
	assertz(cat_has_been_checked(Cat)),
	ChainNext = [Cat | ChainIn],
	!,
	mother_daughter(Cat, Daughter),
	find_recursion(Daughter, ChainNext-ChainOut).

%---------------------------------------------------------------

format_recursion_chain_for_presentation(Chain0, Chain) :-	
	Chain0 = [RecursedCat | RestCats],
	truncate_after_first_occurrence(RestCats, RecursedCat, RestCatsTruncated),
	reverse([RecursedCat | RestCatsTruncated], Chain),
	!.
format_recursion_chain_for_presentation(Chain0, Chain) :-	
	format('~N*** Error: bad call: ~w~n', [format_recursion_chain_for_presentation(Chain0, Chain)]),
	fail.

truncate_after_first_occurrence([X | _R], X, [X]) :-
	!.
truncate_after_first_occurrence([F | R], X, [F | Truncated]) :-
	!,
	truncate_after_first_occurrence(R, X, Truncated).

