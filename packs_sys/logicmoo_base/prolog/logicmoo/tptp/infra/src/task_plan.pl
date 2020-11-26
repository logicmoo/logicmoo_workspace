/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

:- module(task_plan, [plan_1/7]).

:- use_module(planner_run).
:- use_module(planner_convert).
:- use_module(obj_task).
:- use_module(obj_inverse).
:- use_module(solution_dotgraph).
:- use_module('swilib/err').
:- use_module('swilib/term_support').
:- use_module('swilib/pretty').

:- use_module(library(time)).

% 
% SWI-BUG in library(time):
% 
% 1 ?- call_with_time_limit(1, fail).
% ERROR: Arguments are not sufficiently instantiated
% 2 ?-
%
% SWI-BUG in library(time):
%
% - if code within time limit is exited due to another exception or break, the
%   signal is not removed (i.e. some later unrelated call throws
%   time_limit_exceeded)
%
% ?- call_with_time_limit(5, sleep(5)).
% [press ctrl-c, "a" for abort, wait 5 seconds]
% ?- true.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Utilities
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(atl/1).

% Notes: call_with_time_limit(+Seconds, :Goal)
% raises time_limit_exceeded
% Time seems to be real time in seconds, granularity system dependent, about
% 50 or 10 milliseconds

:- module_transparent call_limited/3.
:- module_transparent call_limited_solutions/2.

%%%% 
%%%% Enumerate maximal NSols solutions obtained within time limit.
%%%% 
call_limited(NSols, Time, Goal) :-
	retractall(atl(_)),
	catch( call_with_time_limit(Time,
	         once(( task_plan:call_limited_solutions(NSols, Goal),
		        assert(task_plan:atl(Goal)),
		        fail
		      ; true))),
	       time_limit_exceeded,
	       true ),
	retract(atl(Goal)).

call_limited_solutions(0, _) :-
	!,
	fail.
call_limited_solutions(NSols, Goal) :-
	flag(task_plan_atl, _, 1),
	call(Goal),
	flag(task_plan_atl, N, N + 1),
	( N < NSols ->
	  true
	; !
	).



%%%% 
%%%% plan_1(+KB, +Task, +AT, -Solution)
%%%% 
plan_1(KB, Timeout, Mode, Task, AT, KBOut, Solutions) :-
	CsModule = planner_cs_simple,
	task_goal(Task, Goal),
	task_start(Task, Start),
	task_input_constraints(Task, InCs),
	task_rules(Task, Rules),
	fluent_declarations_for_bodies(Rules, Decls),
	append(Rules, Decls, Rules1),
%	tmp_file('debug_planner.pl', OutFile),
%	Options = [out(OutFile),
	Options = [out(_),		   		   
		   p(Plan),pool(Pool),pxo,bd,o1,s1,cs_module(CsModule)],
	( uses_constraints(Task) ->
	  Options1 = [cs(OutCs)|Options]
	; OutCs = [],
	  Options1 = Options
	),

% 	%% Debugging - print the planning task to stderr
% 	%%
% 	fromonto:onto_stream(( writeln('DEBUG: Planning task:'),
% 		      pretty:pp(plan(Options1, Goal, Start, InCs, Rules1)),
% 		      nl ),
% 		    user_error),
	
	Call = ( plan(Options1, Goal, Start, InCs, Rules1),
		 term_variables(t(Goal, Start, InCs, Plan, Pool),
				SolutionVars),
		 CsModule:project_cs(OutCs, SolutionVars, OutCs1) ),

	MaxNumOfRawSolutions = 50, %% *** to config
	
	findall(Plan-raw_solution(Goal, Start, InCs, OutCs1, Pool),
		( Mode = multi ->
		  call_limited(MaxNumOfRawSolutions, Timeout, Call)
		; catch( call_with_time_limit(Timeout, once(Call)),
			 %% SWI 5.6.40 OS-X: explicit once seems
			 time_limit_exceeded,
			 fail )
		),
		RawPlanXs),
	
	gensym('inf_result', KBId),
	create_knowledgebase_like(KB, KBId, [], KBOut),
	%% *** also put the infraengine schema to KBOut
	prepare_solutions(KBOut, RawPlanXs, AT, Solutions).

prepare_solutions(KB, RawPlanXs, AT, Solutions) :-
	length(RawPlanXs, LengthIn),
	msg('Planning task: ~w raw plans.', [LengthIn]),
	RawPlanXs1 = RawPlanXs,
% 	remove_subsumed_elements(RawPlanXs, RawPlanXs1),
% 	length(RawPlanXs1, LengthSPruned),
% 	msg('Planning task: Syntactically pruned to ~w plans.',
%                  [LengthSPruned]),
	msg('Planning task: removing redundant plans.'),
	prune_plans_by_nodeset_1(RawPlanXs1, GPlanXs), 
	length(GPlanXs, LengthPruned),
	msg('Planning task: ~w plans remaining.',
	    [LengthPruned]),
	inverse_access_templates(AT, TA),
	map_prepare_solution(GPlanXs, KB, TA, Solutions).

map_prepare_solution([X|Xs], Y1, Y2, [X1|Xs1]) :-
	prepare_solution(X, Y1, Y2, X1),
	map_prepare_solution(Xs, Y1, Y2, Xs1).
map_prepare_solution([], _, _, []).

prepare_solution(Plan-RawSolution, KB, TA, Solution) :-
	RawSolution = raw_solution(_Goal, _Start, _InCs, OutCs, _Pool),

	gplan_to_obj(Plan, TA, ObjsPlan),

	%% pool_to_obj(Pool, TA, ObjsPool), ***

	prettify_constraints(OutCs, OutCs1),

	%% VTerm = RawSolution, ***
	VTerm = OutCs1,

	plain_to_obj_fix_vars([ObjsPlan], VTerm, [ObjsPlan1]),
	with_output_to(atom(OutCs2), pretty:pp(OutCs1)),

	gplan_goal_node_id(Plan, GoalNode),
	gensym('http://www.infraengine.com/id/id', Solution),
	gensym('http://www.infraengine.com/id/id', ConstrItem),
	
	RDFSol = [rdf(Solution, rdf_type, inf_Solution),
		  rdf(Solution, inf_solutionPlan, GoalNode),
		  rdf(Solution, inf_solutionConstraints, ConstrItem),
		  rdf(ConstrItem, inf_expression, literal(OutCs2))],
	
	objs_to_rdf(ObjsPlan1, RDFPlan),

	add_facts(KB, [inf_tmp, inf_tmp], [RDFSol, RDFPlan], [[]]).

prettify_constraints([atom_number(X, Y)|Cs], Cs1) :-
	unify_with_occurs_check(X, Y),
	!,
	prettify_constraints(Cs, Cs1).
prettify_constraints([C|Cs], [C|Cs1]) :-
	prettify_constraints(Cs, Cs1).
prettify_constraints([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% prune_plans_by_nodeset_1
%%%%
%%%% Follows the implementation of  prune_plans_by_nodeset
%%%% in planner_convert, but allows to associate extra information with
%%%% the plans, that is just passed in and out.
%%%%
%%%% *** TODO: consider constraints and pool
%%%% 
prune_plans_by_nodeset_1(RawPlansX, GPlansX) :-
	map_plan_to_gplan_1(RawPlansX, G1),
	prune_gplans_by_nodeset_1(G1, GPlansX).

prune_gplans_by_nodeset_1(GPlansX, GPlans1X) :-
	red_subs_by_nodeset_1(GPlansX, G3),
	map_gplan_minimize_1(G3, GPlans1X).

map_gplan_minimize_1([X-Y|Xs], [X1-Y|Xs1]) :-
	gplan_minimize(X, X1),
	map_gplan_minimize_1(Xs, Xs1).
map_gplan_minimize_1([], []).

map_plan_to_gplan_1([X-Y|Xs], [X1-Y|Xs1]) :-
	plan_to_gplan(X, X1),
	map_plan_to_gplan_1(Xs, Xs1).
map_plan_to_gplan_1([], []).

red_subs_by_nodeset_1(Gs, Gs1) :-
	rs1_bn(Gs, [], Gs1).

rs1_bn([G-_|Gs], Gs1, Gs2) :-
	( member(G1-_, Gs)
	; member(G1, Gs1)
	),
	gplan_subsumes_chk_by_nodeset(G1, G),
	!,
	rs1_bn(Gs, Gs1, Gs2).
rs1_bn([G-X|Gs], Gs1, [G-X|Gs2]) :-
	rs1_bn(Gs, [G|Gs1], Gs2).
rs1_bn([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gplan_to_rdf(GPlan, Facts).
% hmmm - the actions are inherently Quoted!
% *** vars - must be shared across: plan/pool/constraints/goal/start

gplan_goal_node_id(gplan(Nodes, _), Id) :-
	memberchk(node('$goal',_)-Id, Nodes),
	!.
gplan_goal_node_id(_, _) :-
	err('No goal node found.').

gplan_to_obj(gplan(Nodes, Edges), TA, Objs) :-
	map_node_to_obj(Nodes, TA, Nodes2),
	map_add_edges(Edges, Nodes2, Nodes3),
	map_val(Nodes3, Objs).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

% *** ref/ids too - globalized http://...

map_add_edges([A-B|Es], Ns, Ns1) :-
	( select(B-obj(Type, PVs), Ns, Ns2) ->
	  true
	; err('Edge without target node ~q in plan graph.', [A-B])
	),
	PV = (inf_planFollows=obj(inf_PlanNode, [inf_ref=A])),
	map_add_edges(Es, [B-obj(Type, [PV|PVs])|Ns2], Ns1).
map_add_edges([], Ns, Ns).

map_node_to_obj([X|Xs], TA, [X1|Xs1]) :-
	node_to_obj(X, TA, X1),
	map_node_to_obj(Xs, TA, Xs1).
map_node_to_obj([], _, []).

node_to_obj(X, TA, Id-Y) :-
	X = node(Plain, _)-Id,
	!,
	plain_to_obj(Plain, TA, [], Action),
	Y = obj(inf_PlanNode, [inf_ref=Id, inf_planAction=Action]).
node_to_obj(X, _, _) :-
	err('Bad plan node source: ~q.', [X]).

%% *** Action - wrap to lit if not object form ?

uses_constraints(Task) :-
	task_input_constraints(Task, Cs),
	Cs \= [],
	!.
uses_constraints(Task) :-
	task_rules(Task, Rules),
	member(Rule, Rules),
	rule_constraints(Rule, Cs),
	Cs \= [],
	!.

% *** Q: avoid toplevel variable objects - is that possible, maybe
%        test with a single type

fluent_declarations_for_bodies(Rules, Decls) :-
	findall(declare(fluent, F),
		( member(Rule, Rules),
		  rule_body(Rule, Body),
		  member(F1, Body),
		  functor(F1, P, N),
		  functor(F, P, N)
		),
		Decls1),
	remove_subsumed_elements(Decls1, Decls).

rule_body(rule(_,_,X,_), X).
rule_constraints(rule(_,_,_,X), X).


