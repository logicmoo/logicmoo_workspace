
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(stepper_rule_db,
	  [make_expanded_dcg_clause_database/0,
	   expanded_dcg_clause/3,
	   flatten_dcg_clause_body/2,
	   remove_empty_constituents_from_daughters/2]
	 ).

%----------------------------------------------------------------------

:- use_module('$REGULUS/Prolog/regulus_utilities').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).

%----------------------------------------------------------------------

:- dynamic expanded_dcg_clause/3.

make_expanded_dcg_clause_database :-
	retractall(expanded_dcg_clause(_, _, _)),
	make_expanded_dcg_clause_database1.

make_expanded_dcg_clause_database1 :-
	current_predicate(user:dcg_clause/2),
	user:dcg_clause(Head0, Body0),
	normalise_prolog_dcg_clause_to_c_version((Head0 :- Body0), (Head :- Body)),
	handle_merge_globals((Head :- Body), (Head1 :- Body1)),
	expand_dcg_clause(Head1, Body1, Head2, Body2),
	assertz(expanded_dcg_clause(Head2, Body2, normal)),
	fail.
make_expanded_dcg_clause_database1 :-
	current_predicate(user:dcg_clause_for_generation/2),
	user:dcg_clause_for_generation(Head0, Body0),
	normalise_prolog_dcg_clause_to_c_version((Head0 :- Body0), (Head :- Body)),
	handle_merge_globals((Head :- Body), (Head1 :- Body1)),
	expand_dcg_clause(Head1, Body1, Head2, Body2),
	assertz(expanded_dcg_clause(Head2, Body2, generation)),
	fail.
make_expanded_dcg_clause_database1.

%---------------------------------------------------------------------

% Adapted from code in generator_compiler.pl

handle_merge_globals(Rule, Rule) :-
	\+ term_contains_functor(Rule, merge_globals/2),
	!.
handle_merge_globals((Head :- Body), (Head :- Body1)) :-
	arg(3, Head, HeadSem),
	handle_merge_globals1(Body, HeadSem, Body1),
	!.
handle_merge_globals(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n', [handle_merge_globals(X, Y)]),
	fail.

handle_merge_globals1((P, Q), HeadSem, (P1, Q1)) :-
	!,
	handle_merge_globals1(P, HeadSem, P1),
	handle_merge_globals1(Q, HeadSem, Q1).
handle_merge_globals1(merge_globals([value=HeadSem], _), HeadSem, true) :-
	!.
handle_merge_globals1(Other, _HeadSem, Other) :-
	!.

%---------------------------------------------------------------------

expand_dcg_clause(HeadIn, BodyIn, HeadOut, BodyOut) :-
	expand_dcg_clause_body(BodyIn, BodyNext),
	flatten_dcg_clause_body(BodyNext, BodyOut),
	remove_empty_constituents_from_tree_in_dcg_clause_head(HeadIn, HeadNext),
	eliminate_head_only_sem_vars_in_dcg_clause_head(HeadNext, BodyOut, HeadOut).

expand_dcg_clause_body((P, Q), (P1, Q1)) :-
	!,
	expand_dcg_clause_body(P, P1),
	expand_dcg_clause_body(Q, Q1).
expand_dcg_clause_body((P ; Q), Result) :-
	!,
	(   expand_dcg_clause_body(P, Result) ;
	    expand_dcg_clause_body(Q, Result)
	).
expand_dcg_clause_body(X = Y, true) :-
	!,
	X = Y.
expand_dcg_clause_body(merge_globals([_Key=X], Y), true) :-
	!,
	X = Y.
expand_dcg_clause_body(Other, Other).

flatten_dcg_clause_body(In, Out) :-
	flatten_dcg_clause_body1(In, Next),
	!,
	flatten_dcg_clause_body(Next, Out).
flatten_dcg_clause_body(In, In).	

flatten_dcg_clause_body1(((P, Q), R), (P, (Q, R))) :-
	!.
flatten_dcg_clause_body1((P, true), P) :-
	!.
flatten_dcg_clause_body1((true, P), P) :-
	!.
flatten_dcg_clause_body1((P, Q), (P, Q1)) :-
	flatten_dcg_clause_body1(Q, Q1).

remove_empty_constituents_from_tree_in_dcg_clause_head(HeadIn, HeadOut) :-
	HeadIn =.. [Cat, Tree | Rest],
	remove_empty_constituents_from_tree(Tree, Tree1),
	HeadOut =.. [Cat, Tree1 | Rest],
	!.

remove_empty_constituents_from_tree(TreeIn, TreeOut) :-
	TreeIn = phrase(Cat, LineInfoIn, DaughtersIn),
	remove_empty_constituents_from_daughters(DaughtersIn, DaughtersOut),
	add_null_cut_to_line_info(LineInfoIn, LineInfoOut),
	TreeOut = phrase(Cat, LineInfoOut, DaughtersOut).

remove_empty_constituents_from_daughters(DaughtersIn, DaughtersIn) :-
	%DaughtersIn == empty_constituent,
	is_empty_daughter(DaughtersIn),
	!.
remove_empty_constituents_from_daughters(DaughtersIn, DaughtersOut) :-
	comma_list_to_list(DaughtersIn, DaughtersInList),
	remove_empty_constituents_from_daughters_list(DaughtersInList, DaughtersOutList),
	list_to_comma_list(DaughtersOutList, DaughtersOut).

remove_empty_constituents_from_daughters_list([], []).
remove_empty_constituents_from_daughters_list([F | R], R1) :-
	nonvar(F),
	%F == empty_constituent,
	is_empty_daughter(F),
	!,
	remove_empty_constituents_from_daughters_list(R, R1).
remove_empty_constituents_from_daughters_list([F | R], [F | R1]) :-
	!,
	remove_empty_constituents_from_daughters_list(R, R1).

is_empty_daughter(Daughter) :-
	nonvar(Daughter),
	Daughter = empty_constituent,
	!.
is_empty_daughter(Daughter) :-
	nonvar(Daughter),
	Daughter == phrase(null_sem,no_line_info,empty_constituent),
	!.

eliminate_head_only_sem_vars_in_dcg_clause_head(HeadIn, Body, HeadOut) :-
	HeadIn =.. [Cat, Tree, Feats, SemIn | Rest],
	instantiate_head_only_vars_to_null(SemIn, Body),
	simplify_away_null_values_in_sem(SemIn, SemOut),
	HeadOut =.. [Cat, Tree, Feats, SemOut | Rest],
	!.
eliminate_head_only_sem_vars_in_dcg_clause_head(HeadIn, Body, HeadOut) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [eliminate_head_only_sem_vars_in_dcg_clause_head(HeadIn, Body, HeadOut)]),
	fail.

instantiate_head_only_vars_to_null(Sem, Body) :-
	term_variables(Sem, HeadSemVars),
	term_variables(Body, BodyVars),
	instantiate_head_only_vars_to_null1(HeadSemVars, BodyVars),
	!.

instantiate_head_only_vars_to_null1([], _BodyVars).
instantiate_head_only_vars_to_null1([F | R], BodyVars) :-
	instantiate_head_only_var(F, BodyVars),
	!,
	instantiate_head_only_vars_to_null1(R, BodyVars).

instantiate_head_only_var(Var, BodyVars) :-
	var(Var),
	\+ id_member(Var, BodyVars),
	!,
	Var = '*null_value*'.
instantiate_head_only_var(_F, _BodyVars).

simplify_away_null_values_in_sem(In, In) :-
	(   var(In) ;
	    atomic(In)
	),
	!.
simplify_away_null_values_in_sem(In, Out) :-
	In =.. [Function, X, Y],
	binary_gsl_function(Function),
	simplify_away_null_values_in_sem(X, X1),
	simplify_away_null_values_in_sem(Y, Y1),
	(   null_value(X1) ->
	    Out = Y1 ;
	    
	    null_value(Y1) ->
	    Out = X1 ;
	    
	    Out =.. [Function, X1, Y1]
	),
	!.
simplify_away_null_values_in_sem(In, Out) :-
	In =.. [Function, X],
	unary_gsl_function(Function),
	simplify_away_null_values_in_sem(X, X1),
	(   null_value(X1) ->
	    Out = X1 ;
	    
	    Out =.. [Function, X1]
	),	
	!.
simplify_away_null_values_in_sem(Other, Other).

binary_gsl_function(add).
binary_gsl_function(sub).
binary_gsl_function(mul).
binary_gsl_function(div).
binary_gsl_function(strcat).
binary_gsl_function(concat).

unary_gsl_function(neg).

null_value(X) :-
	X == '*null_value*'.

add_null_cut_to_line_info(line_info(NodeID, Lines, File),
			  line_info(NodeID, no_cut, Lines, File)) :-
	!.
add_null_cut_to_line_info(no_line_info, no_line_info) :-
	!.
add_null_cut_to_line_info(X, Y) :-
	format2error('~N*** Error: bad call: ~w~n',
		     [add_null_cut_to_line_info(X, Y)]),
	fail.
	