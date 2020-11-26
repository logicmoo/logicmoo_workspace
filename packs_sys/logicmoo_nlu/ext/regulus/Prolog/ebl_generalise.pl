% ebl_generalise.pl
 
%---------------------------------------------------------------

:- module(ebl_generalise,
	  [generalise/3]
      ).

%---------------------------------------------------------------

:- use_module('$REGULUS/Prolog/ebl_operational').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/PrologLib/utilities').
:- use_module(library(lists)).

%----------------------------------------------------------------------

generalise(Goal, Operationality, Rules) :-
	generalise(Goal, normal, Operationality, no_context_above, Rules-[]).

generalise(Goal, Status, Operationality, Context, [Rule | RulesNext]-RulesOut) :-
	generalise1(Goal, GGoal, Status, Operationality, Context, Conds0-[], GConds0-[], RulesNext-RulesOut),
	list_to_comma_list_or_true(Conds0, Conds),
	list_to_comma_list_or_true(GConds0, GConds),

	GeneralRule = (GGoal :- GConds),
	RuleContext = (Goal :- Conds),
	goal_fringe(Goal, Fringe),

	Rule = rule(GeneralRule, RuleContext, Fringe).	

generalise1(true, true, _Status, _Operationality, _Context, Conds-Conds, GConds-GConds, Rules-Rules) :-
	!.

generalise1((X = Y), (X1 = Y1), _Status, _Operationality, _Context, Conds-Conds, GConds-GConds, Rules-Rules) :-
	!,
	X = Y,
	X1 = Y1.

generalise1((P, Q), (GP, GQ), _Status, Operationality, Context, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut) :-
	!,
	generalise1(P, GP, normal, Operationality, Context, CondsIn-CondsNext, GCondsIn-GCondsNext, RulesIn-RulesNext),
	generalise1(Q, GQ, normal, Operationality, Context, CondsNext-CondsOut, GCondsNext-GCondsOut, RulesNext-RulesOut).

generalise1((P ; Q), (GP ; GQ), _Status, Operationality, Context, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut) :-
	!,
	(    
	    generalise1(P, GP, normal, Operationality, Context, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut) ;
	    generalise1(Q, GQ, normal, Operationality, Context, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut)
	).

generalise1(Goal, GGoal, _, _Operationality, _Context, 
	    [Goal | CondsOut]-CondsOut, [GGoal | GCondsOut]-GCondsOut, Rules-Rules) :-
	built_in_goal(Goal, CallableGoal),
	!,
	call(CallableGoal).

generalise1(Goal, GGoal, normal, Operationality, Context, 
	    [Goal | CondsOut]-CondsOut, [GGoal | GCondsOut]-GCondsOut, RulesIn-RulesOut) :-
	get_operational_goal(Goal, Operationality, Context, NewContext),
	!,
	generalise(Goal, force_non_operational, Operationality, NewContext, RulesIn-RulesOut).

generalise1(Goal, GGoal, _Status, Operationality,  Context, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut) :-
	(   get_change_context_goal(Goal, Operationality, Context, NewContext) ->
	    true ;
	    NewContext = Context
	),
	expand_goal(Goal, GGoal, Body, GBody),
	generalise1(Body, GBody, normal, Operationality,  NewContext, CondsIn-CondsOut, GCondsIn-GCondsOut, RulesIn-RulesOut).

%generalise1(Goal, _GGoal, _Status, Operationality,  Context, CondsIn-CondsIn, GCondsIn-GCondsIn, RulesIn-RulesIn) :-
%	format('~N*** Error: cannot process goal ~w~n', [Goal]),
%	fail.

expand_goal(Goal, GGoal, Body, GBody) :-
	current_predicate(dcg_clause, user:dcg_clause(_, _)),
	functor(Goal, F, N),
	functor(Head, F, N),
	user:dcg_clause(Head, Body),
	copy_term((Head, Body), (GHead, GBody)),
	Goal = Head,
	GGoal = GHead.

%----------------------------------------------------------------------

get_operational_goal(Goal, from_file, Context, NewContext) :-
	(   current_predicate(tmp_ebl_operational:operational_goal/3) ->
	    
	    tmp_ebl_operational:operational_goal(Goal, Context, NewContext) ;
	    
	    format2error('~N*** Error: operational_goal/3 should be defined, but is not~n', []),
	    fail
	).
get_operational_goal(Goal, Operationality, Context, NewContext) :-
	atom(Operationality),
	Operationality \== from_file,
	operational_goal(Goal, Operationality, Context, NewContext),
	\+ functor(NewContext, change_context, 1).

get_change_context_goal(Goal, from_file, Context, NewContext) :-
	current_predicate(tmp_ebl_operational:change_context_goal/3),
	tmp_ebl_operational:change_context_goal(Goal, Context, NewContext).
get_change_context_goal(Goal, Operationality, Context, NewContext) :-
	atom(Operationality),
	Operationality \== from_file,
	operational_goal(Goal, Operationality, Context, change_context(NewContext)).

%----------------------------------------------------------------------

list_to_comma_list_or_true([], true) :-
	!.
list_to_comma_list_or_true(List, CommaList) :-
	list_to_comma_list(List, CommaList),
	!.
