%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% stat.pl
%
% Main procedures:
%
% STAT(GIn,State_Req)
% tells us whether the Goal GIn requires 0, 1, or 2 extra args
% for explicit passing of state variables.
%  0: if G is independent of state.
%  1: if G has a subgoal which might test state, but no subgoal changes state.
%  2: if G has a subgoal which might change state.
%
% STATIVITY_ANALYSIS
% Performs an abstract interpretation over the loaded domain theory.
% This proceeds as follows:
%
% Assume all predicates have stativity 0 (ie we know nothing about their
% stativity).  
% At each iteration I+1, update our assumptions on stativity for each predicate based
% on the current assumptions:
% 1. Compute stativity based on current assumptions.
% 2. If new stativity for this pred is different, save it, and note
% something changed. 
% [When we update an assumption, we can immediately change its entry in
% the table to use on subsequence steps within the same iteration.]
%
% If nothing changed, then we've reached a fixedpoint, so our
% assumptions are now correct (at least we'll gain no more knowledge by
% repeating this analysis).  
% Otherwise, do another loop.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- my_ensure_loaded(library(tracing)).

:- dynamic(stativity/2).


stat(A,0) :- var(A), write("~n Warning: Error in Stat routine, Variable!"), nl, !.

stat((A,B),S) :- !,
	stat(A,StatA),
	stat(B,StatB),
	max(StatA,StatB,S).

stat((A;B),S) :- !,
	stat(A,StatA),
	stat(B,StatB),
	max(StatA,StatB,S).

stat(if(Cond,Then,Else),S) :- !,
	stat(Cond,CondS),
	stat(Then,ThenS),
	stat(Else,ElseS),
	max([CondS,ThenS,ElseS],S).

stat((Cond->Then;Else),S) :- !,
	stat(Cond,CondS),
	stat(Then,ThenS),
	stat(Else,ElseS),
	max([CondS,ThenS,ElseS],S).

stat((Cond->Then),S) :- !,
	stat(Cond,CondS),
	stat(Then,ThenS),
	max([CondS,ThenS],S).

stat(call(Call),S) :- !,
	stat(Call,S).

stat((\+ Call),S) :- !,
	stat(Call,S).

% Setof and bagof are logical, so the may depend on
% state (possibly) but not change it.
stat(setof(_X,Test,_Xs),S) :- !,
	stat(Test,TestS),
	min([TestS,1],S).

stat(bagof(_X,Test,_Xs),S) :- !,
	stat(Test,TestS),
	min([TestS,1],S).

stat(_X^Test,Is) :- !,
	stat(Test,Is).

stat(GIn,1) :- test_goal(GIn), !.

stat(GIn,2) :- change_goal(GIn), !.

stat(GIn,S) :- stativity(GIn,S), !.

stat(_,0).
	

change_goal(add(_Pred)).
change_goal(del(_Pred)).

test_goal(true(_Pred)).

	                 
%============================================================
% Stativity Analysis

stativity_analysis :- 
	initialize_stats(0),
	interpret_stativity.


% Changed will fail unless something really changed.

:- dynamic changed/0.


initialize_stats(InitVal) :- 
	reset_stat,
	theory_clause(GIn,_Body),
	variablized_goal(GIn,VGIn),
	( stativity(VGIn,Stat) 
	-> ( Stat = InitVal 
	   -> true
	   ;  format("Error: Some stat entry not reset properly~n",[])
	   )
	;  assert(stativity(VGIn,InitVal))
	),
	fail.
initialize_stats(_). 

interpret_stativity :- 
	tracing_format(stat(1),"Starting an iteration of stativity interpretation~n",[]),
	do_analysis,
	loop_if_changed.

loop_if_changed :- 
	retract(changed), !, 
	interpret_stativity.
loop_if_changed :- 
	tracing_format(stat(1),"Reached fixed point, interpretation has ended.~n",[]).
	

do_analysis :- 
	clear_changed, 
	( analysis_item(Item),
	  analyze_item(Item),
	  fail
	; true
	).

analysis_item((Goal:-Body)) :- 
	theory_clause(Goal,Body).
	
analyze_item((Goal:-Body)) :- 
	stat(Body,Stat),
	update_assumption(Goal,Stat).

% Only update assumptions to values higher in the lattice.
update_assumption(P,Stat) :- 
	stativity(P,StatOld), 
	Stat =< StatOld, !.
update_assumption(P,Stat) :- 
	variablized_goal(P,VP),
	retract(stativity(VP,StatOld)), 
	assert(stativity(VP,Stat)),
	tracing_format(stat(2),
	       "Procedure <~p> changed stativity from <~p> to <~p>~n",[P,StatOld,Stat]),
	note_changed.



note_changed :- changed, !.
note_changed :- 
	tracing_format(stat(2),"Initial stativity changed this iteration~n",[]),
	assert(changed).	

clear_changed :- retractall(changed).
reset_stat :- retractall(stativity(_,_)).


variablized_goal(GoalIn,GoalOut) :-
	functor(GoalIn,F,A),
	functor(GoalOut,F,A).

%================================================================================
% tracing execution

set_stat_verbosity(Level,Status) :- set_tracing(stat(Level),Status).
silent_stat :- set_stat_verbosity(1,off), 
	       set_stat_verbosity(2,off). 

:- set_stat_verbosity(1,on).
