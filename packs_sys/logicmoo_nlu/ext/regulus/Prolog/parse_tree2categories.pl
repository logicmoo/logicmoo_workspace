% parse_tree2categories.pl

%---------------------------------------------------------------

:- module(parse_tree2categories,
	  [parse_tree2categories/2,
	   parse_tree2categories/3]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/dcg2regulus').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_eval').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).

%---------------------------------------------------------------

parse_tree2categories(Tree, Categories) :-
	parse_tree2categories(Tree, user, Categories).

parse_tree2categories(Tree, Module, Categories) :-
	Tree = phrase(GrammarAtom, _LineInfo, _Daughters),
	Goal =.. [GrammarAtom, Tree, _SynFeats, _Local, _Global, _WordList, []],
	parse_tree2categories1(Goal, Module, Categories0-[]),
	sort(Categories0, Categories1),
	prolog_categories_to_regulus(Categories1, Categories),
	!.
parse_tree2categories(Tree, Module, Categories) :-
	format('~N*** Error: bad call: ~w~n',
	       [parse_tree2categories(Tree, Module, Categories)]),
	fail.

parse_tree2categories1(true, _Module, CatsIn-CatsIn) :-
	!.
parse_tree2categories1((X = Y), _Module, CatsIn-CatsIn) :-
	X = Y,
	!.
parse_tree2categories1((P, Q), Module, CatsIn-CatsOut) :-
	!,
	parse_tree2categories1(P, Module, CatsIn-CatsNext),
	parse_tree2categories1(Q, Module, CatsNext-CatsOut).
parse_tree2categories1((P ; Q), Module, CatsIn-CatsOut) :-
	!,
	(     parse_tree2categories1(P, Module, CatsIn-CatsOut)
	;
	    parse_tree2categories1(Q, Module, CatsIn-CatsOut)
	).
parse_tree2categories1(Goal, _Module, CatsIn-CatsIn) :-
	built_in_goal(Goal),
	!,
	call(Goal).
parse_tree2categories1(Goal, Module, CatsIn-CatsOut) :-
	Module:dcg_clause(Goal, Body),
	CatsIn = [Goal | CatsNext],
	parse_tree2categories1(Body, Module, CatsNext-CatsOut).

%---------------------------------------------------------------

prolog_categories_to_regulus([], []).
prolog_categories_to_regulus([F | R], [F1 | R1]) :-
	prolog_category_to_regulus(F, F1),
	!,
	prolog_categories_to_regulus(R, R1).

prolog_category_to_regulus(PrologCat, RegulusCat) :-
	PrologCat =.. [Cat, _Tree, SynFeats, _Local, _Global, _WordsIn, _WordsOut],
	PrologCat1 =.. [Cat, _Tree1, SynFeats, _Local1, _Global1, W, W],
	prolog_dcg_rule_to_regulus((PrologCat1 :- true), (RegulusCat --> _RegulusBody)).

%---------------------------------------------------------------

built_in_goal(Goal) :-
	predicate_property(Goal, built_in),
	!.
built_in_goal(merge_globals(_X, _Y)).

