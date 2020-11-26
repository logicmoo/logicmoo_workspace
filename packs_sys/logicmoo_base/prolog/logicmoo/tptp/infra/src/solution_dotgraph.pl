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

:- module(solution_dotgraph, [rdf_plan_to_dotgraph/3,
			      action_attribs/4,
			      rdf_plan_to_gplan/3]).

:- use_module('swilib/err').

:- use_module(pages_util).
:- use_module(pages_queries).
:- use_module(textutil).
:- use_module(planner_dotgraph).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% rdf_plan_to_dotgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rdf_plan_to_dotgraph(KB, GoalNode, Dotgraph) :-
	rdf_plan_to_gplan(KB, GoalNode, GPlan),
	gplan_to_dotgraph(GPlan,
			  [action_attributes(solution_dotgraph,
					     action_attribs,
					     KB)],
			  Dotgraph).

action_attribs(_, literal('$start'), _,
	       [label='Start', fillcolor='0.0 0.2 1.0']) :-
 	!.
action_attribs(_, literal('$goal'), _,
	       [label='Goal', fillcolor='0.33 0.2 1.0']) :-
 	!.
action_attribs(KB, Action, _Args, [label=Label, 'URL'=Uri]) :-
	item_uri(KB, dotgraph, Action, Uri),
	%% item_uri(KB, object, Action, Uri), %% *** which one?
	boxed(Action, KB, [], Boxed),
	with_output_to(atom(Label1), pp_boxed(Boxed)),
	concat_atom(Label2, '\n', Label1),
	map_justify(Label2, Label).

map_justify([X|Xs], [l(X)|Xs1]) :-
	map_justify(Xs, Xs1).
map_justify([], []).

boxed(N, _, A, N-['...']) :-
	memberchk(N, A),
	!.
boxed(N, KB, A, N-PVs) :-
	setof(P=Vs, setof(V, boxed_1(N, KB, A, P, V), Vs), PVs1),
	!,
	( select('rdf:type'=Types, PVs1, PVs2) ->
	  PVs = ['rdf:type'=Types|PVs2]
	; PVs = PVs1
	).
boxed(N, _, _, N-[]).

boxed_1(N, KB, A, P, V) :-
	informative_fact(KB, N, P1, V1),
	pretty_item(KB, P1, P),
	( V1 = literal(V2) ->
          pretty_literal(V2, V3),
	  V = literal(V3)
	; V1 = blank(_) ->
	  boxed(V1, KB, [N|A], V)
        ; pretty_item(KB, V1, V)
	).

pretty_literal(Literal, Literal1) :-
	( Literal = [element(Tag, _, _)|_] ->
	  ( Tag = _:Tag1 ->
	    true
	  ; Tag1 = Tag
	  ),
	  concat_atom(['<', Tag1, '...'], Literal2)
	; % abbreviate(Literal, 25, Literal2)
	  abbreviate(Literal, 50, Literal2)
	),
	concat_atom(['"', Literal2, '"'], Literal1).

pretty_item(KB, Item, Label) :-
	atom(Item),
	!,
	resource_pretty_name(KB, Item, Label).
pretty_item(_, Item, Label) :-
	term_to_atom(Item, Label).


pp_boxed(X) :-
	pp_boxed(X, 0).

pp_boxed(N-PVs, Ind) :-
	!,
	( PVs = ['rdf:type'=Types|PVs1] ->
	  pp_types(Types)
	; PVs1 = PVs
	),
	pp_head_object(N),
	Ind1 is Ind+4,
	( member(P=Vs, PVs1),
	  member(V, Vs),
	  nl,
	  pp_item(P, Ind1, Ind2),
	  % write(' = '), Ind3 is Ind2 + 3,
	  write('='), Ind3 is Ind2 + 1,	    
	  pp_boxed(V, Ind3),
	  fail
	; true
	).
pp_boxed(N, _) :-
	pp_item(N).

pp_pretty_item(literal(X), X) :-
	!.
pp_pretty_item(X, X).
	    
pp_item(Item) :-
	pp_pretty_item(Item, Item1),
	write(Item1).

pp_item(Item, Ind1, Ind2) :-
	pp_pretty_item(Item, Item1),
	atom_length(Item1, L),
	Ind2 is Ind1 + L,
	indent(Ind1),
	write(Item1).

pp_types([Type|Types]) :-
	pp_item(Type),
	pp_types_1(Types).
pp_types_1([Type|Types]) :-
	write(', '),
	pp_item(Type),
	pp_types_1(Types).
pp_types_1([]).

pp_head_object(blank(_)) :-
	!.
pp_head_object(Item) :-
	write(' '),
	pp_item(Item).

item_id(blank(BId), Id) :-
	!,
	term_to_atom(blank(BId), Id).
item_id(Item, Item) :-
	atom(Item),
	!.
item_id(Item, _) :-
	err('Can not convert item: ~q.', [Item]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% rdf_plan_to_gplan
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Nodes is a list of node(Action, Args)-Id
%%%% Edges is a list of Id1-Id2 (i.e. a PGraph)
%%%% Both output lists are ordered.
%%%%
%%%% - Action is the URI of the action, in this form of gplan.
%%%% - Ids are atoms, not necessarily URIs.
%%%% - Args is not used (always []).
%%%%
%%%% See also planner_convert.pl.
%%%%

rdf_plan_to_gplan(KB, GoalNode, gplan(Nodes, Edges)) :-
	pdg(GoalNode, KB, Stmts, []),
	sort(Stmts, Stmts1),
	once(( split_list_1(Stmts1, Edges, Nodes),
	       Nodes = [node(_, _)-_|_] )).
		
pdg(N, KB, Stmts, StmtsRest) :-
	pdg1(N, KB, [], _, Stmts, StmtsRest).

pdg1(N, _, Seen, Seen, Stmts, Stmts) :-
	memberchk(N, Seen),
	!.
pdg1(N, KB, Seen, Seen1, [node(A, [])-I|Stmts], Stmts1) :-
	node_to_graph_id(N, I),
	once(fact(KB, N, inf_planAction, A)),
	( setof(N1, fact(KB, N, inf_planFollows, N1 ), N1s) ->
	  map_pdg2(N1s, KB, N, [N|Seen], Seen1, Stmts, Stmts1)
	; Seen1 = [N|Seen],
	  Stmts1 = Stmts
	).

map_pdg2([N|Ns], KB, N1, Seen, Seen1, [I-I1|Stmts], Stmts1) :-
	node_to_graph_id(N, I),
	node_to_graph_id(N1, I1),
	pdg1(N, KB, Seen, Seen2, Stmts, Stmts2),
	map_pdg2(Ns, KB, N1, Seen2, Seen1, Stmts2, Stmts1).
map_pdg2([], _, _, Seen, Seen, Stmts, Stmts).

node_to_graph_id(N, GraphId) :-
	term_to_atom(N, GraphId).

split_list_1(Xs, [], Xs).
split_list_1([X|Xs], [X|Ys], Zs) :-
	split_list_1(Xs, Ys, Zs).


