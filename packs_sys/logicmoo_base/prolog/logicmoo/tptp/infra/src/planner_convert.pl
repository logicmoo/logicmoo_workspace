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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Plan Conversion
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(planner_convert, [plan_to_gplan/2,
			    gplan_minimize/2,
			    gplan_warshall/2,
			    gplan_subsumes_chk/2,
			    prune_gplans/2,
			    prune_plans/2,
			    gplan_subsumes_chk_by_nodeset/2,
			    prune_gplans_by_nodeset/2,
			    prune_plans_by_nodeset/2,

			    map_gplan_print_info/1
			   ]).

:- use_module(graphs_util).

:- use_module('swilib/err').
:- use_module('swilib/graphs').
:- use_module(library(ordsets)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Canonicalize Plan
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonicalize_plan(Plan, Plan1) :-
	cp_1(Plan, Plan1).

cp_1( goal(SubActions), Plan) :-
	Plan = plan(action('$goal', SubActions1, Label), []),
	gen_id(Label),
	map_cp_1(SubActions, SubActions1).
cp_1( fact(Action), Plan) :-
	Plan = plan(action(Action, [], Label), []),
	gen_id(Label).
cp_1( action(Action, SubActions), Plan) :-
	Plan = plan(action(Action, SubActions1, Label), []),
	gen_id(Label),
	map_cp_1(SubActions, SubActions1).
cp_1( action(Action, SubActions, Label), Plan) :-
	( var(Label) -> gen_id(Label) ; true ),
	Plan = plan(action(Action, SubActions1, Label), []),
	map_cp_1(SubActions, SubActions1).
cp_1( ref(Label), Plan) :-
	% ( var(Label) -> gensym(ref, Label) ; true ),	
	( var(Label) -> gen_id(Label) ; true ),
	Plan = plan(ref(Label), []).
cp_1( empty, Plan) :-
	Plan = plan(empty, []).

map_cp_1([X|Xs], [X1|Xs1]) :-
	cp_1(X, X1),
	map_cp_1(Xs, Xs1).
map_cp_1([], []).

gen_id(Id) :-
	gensym('http://www.infraengine.com/id/id', Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Plan as PGraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Nodes is a list of node(Action, Args)-Id
%%%% Edges is a list of Id1-Id2 (i.e. a PGraph)
%%%% Both output lists are ordered.
%%%%
plan_to_gplan(Plan, gplan(Nodes, Edges)) :-
	canonicalize_plan(Plan, Plan1),
	( setof( Stmt, g_stmt(Plan1, Stmt), Stmts ) ->
	  extract_nodes(Stmts, Nodes1, Edges),
	  Nodes = [node('$start', [])-start|Nodes1]
	; Edges = [],
	  Nodes = []
	).

extract_nodes([node(Node)|Ss], [Node|Ns], Es) :-
	!,
	extract_nodes(Ss, Ns, Es).
extract_nodes(Es, [], Es).

g_stmt( plan(action(Action, SubPlans, Id), Args), Stmt ) :-
	( Stmt = node(node(Action, Args)-Id)
	; member(SubPlan, SubPlans),
	  ( plan_edge(Id, SubPlan, Stmt)
	  ; g_stmt(SubPlan, Stmt)
	  )
	).

plan_edge(Id, plan(action(_, _, Id1), _), Id1-Id) :-
	!.
plan_edge(Id, plan(ref(Id1), _), Id1-Id).
% 
% plan_edge(Id, plan(ref(Id1), _), Id1-Id) :-
% 	Id1 \= start.
% 	   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Operations
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gplan_minimize(gplan(N,E), gplan(N,E1)) :-
	pgraph_minimize(E, E1).

gplan_warshall(gplan(N,E), gplan(N,E1)) :-
	p_to_s_graph(E, S),
	warshall(S, S1),
	s_to_p_graph(S1, E1).

gplan_print_info(gplan(N,E)) :-
	length(N, LN),
	length(E, LE),
	format(' ~w:~w', [LN, LE]).

gplan_subsumes_chk(gplan(N,E), gplan(N1,E1)) :-
	pgraph_subsumes_chk(N, E, N1, E1).

gplan_subsumes_chk_by_nodeset(gplan(N,_), gplan(N1,_)) :-
	nodeset_subsumes_chk(N, N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Prune GPlans
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_plan_to_gplan([X|Xs], [X1|Xs1]) :-
	plan_to_gplan(X, X1),
	map_plan_to_gplan(Xs, Xs1).
map_plan_to_gplan([], []).

map_gplan_minimize([X|Xs], [X1|Xs1]) :-
	gplan_minimize(X, X1),
	map_gplan_minimize(Xs, Xs1).
map_gplan_minimize([], []).

map_gplan_warshall([X|Xs], [X1|Xs1]) :-
	gplan_warshall(X, X1),
	map_gplan_warshall(Xs, Xs1).
map_gplan_warshall([], []).

map_gplan_print_info([X|Xs]) :-
	gplan_print_info(X),
	map_gplan_print_info(Xs).
map_gplan_print_info([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Prune by comparing graph structure.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prune_plans(RawPlans, GPlans) :-
	map_plan_to_gplan(RawPlans, G1),
	prune_gplans(G1, GPlans).

prune_gplans(GPlans, GPlans1) :-
	map_gplan_warshall(GPlans, G2),
	red_subs(G2, G3),
	map_gplan_minimize(G3, GPlans1).

red_subs(Gs, Gs1) :-
	rs1(Gs, [], Gs1).

rs1([G|Gs], Gs1, Gs2) :-
	( member(G1, Gs)
	; member(G1, Gs1)
	),
	gplan_subsumes_chk(G1, G),
	!,
	rs1(Gs, Gs1, Gs2).
rs1([G|Gs], Gs1, [G|Gs2]) :-
	rs1(Gs, [G|Gs1], Gs2).
rs1([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% By Nodeset
%%%% 
%%%% Just compare set of nodes (i.e. actions) in the plans, not their
%%%% graphs.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prune_plans_by_nodeset(RawPlans, GPlans) :-
	map_plan_to_gplan(RawPlans, G1),
	prune_gplans_by_nodeset(G1, GPlans).

prune_gplans_by_nodeset(GPlans, GPlans1) :-
	red_subs_by_nodeset(GPlans, G3),
	map_gplan_minimize(G3, GPlans1).

red_subs_by_nodeset(Gs, Gs1) :-
	rs1_bn(Gs, [], Gs1).

rs1_bn([G|Gs], Gs1, Gs2) :-
	( member(G1, Gs)
	; member(G1, Gs1)
	),
	gplan_subsumes_chk_by_nodeset(G1, G),
	!,
	rs1_bn(Gs, Gs1, Gs2).
rs1_bn([G|Gs], Gs1, [G|Gs2]) :-
	rs1_bn(Gs, [G|Gs1], Gs2).
rs1_bn([], _, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




