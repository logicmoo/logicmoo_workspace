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

:- module(planner_dotgraph,
	  [plan_to_dotgraph/3,
	   view_plan/1,
	   view_plan/2,
	   view_gplan/1,
	   view_gplan/2,
	   gplan_to_dotgraph/3]).

:- use_module('planner_convert').
:- use_module(dotgraph).
:- use_module('swilib/err').

plan_to_dotgraph(Plan, Options, Dotgraph) :-
	plan_to_gplan(Plan, GPlan),
	gplan_minimize(GPlan, GPlan1),
	gplan_to_dotgraph(GPlan1, Options, Dotgraph).
	
gplan_to_dotgraph(gplan(Nodes, Edges), Options, Dotgraph) :-
	map_node_to_dot(Nodes, Options, DNodes),
	map_edge_to_dot(Edges, DEdges),
	append(DNodes, DEdges, Stmts1),
	graph_attributes(GAs),
	edge_defaults(EAs),
	node_defaults(NAs),
	Dotgraph = dotgraph('Plan',
			    [ graph_attributes(GAs),
			      edge_defaults(EAs),
			      node_defaults(NAs)
			    |Stmts1]).

graph_attributes([ % Rankdir='LR'
		   bgcolor='0.083 0.13 1.0'
		  ]).

%% 
%% The module qualified config:config permits that two alternative
%% implementations of config/2 can be loaded: From 'config.pl' for
%% the web application, and by just defining config:config/2 in
%% load_just_planner for just the planner application.
%% 

edge_defaults([fontname=FontName, fontsize=FontSize]) :-
	config:config(planner_dotgraph_fontname, FontName),
	config:config(planner_dotgraph_fontsize, FontSize).

node_defaults([fontname=FontName,
	       fontsize=FontSize,
	       shape=box,
	       color=black,
	       style=filled,
	       fillcolor='0.5 0.2 1.0']) :-
	config:config(planner_dotgraph_fontname, FontName),
	config:config(planner_dotgraph_fontsize, FontSize).

action_attributes('$start', _, [label='Start', fillcolor='0.0 0.2 1.0']) :-
	!.
action_attributes('$goal', _, [label='Goal', fillcolor='0.33 0.2 1.0']) :-
	!.
action_attributes(Action, Args, [label=Label]) :-
	term_to_atom(Action, Action1),
	( memberchk(cost(Cost), Args) ->
	  Line2p = true,
	  concat_atom(['Cost: ', Cost], Cost1)
        ; Cost1 = ''
        ),
	( memberchk(time(Time), Args) ->
	  Line2p = true,
	  concat_atom(['Time: ', Time], Time1)
        ; Time1 = ''
        ),
	( memberchk(start_time(Start), Args) ->
	  Line2p = true,
	  concat_atom(['Start: ', Start], Start1)
        ; Start1 = ''
        ),
	( memberchk(cost_sum(Sum), Args) ->
	  Line2p = true, 
	  concat_atom(['Sum: ', Sum], Sum1)
        ; Sum1 = ''
        ),
	( Line2p == true ->
	  concat_atom([Start1, Time1, Sum1, Cost1], ' ', Line2),
	  Label = [Action1, Line2]
        ; Label = Action1
        ).

map_node_to_dot([X|Xs], Options, [X1|Xs1]) :-
	node_to_dot(X, Options, X1),
	map_node_to_dot(Xs, Options, Xs1).
map_node_to_dot([], _, []).

map_edge_to_dot([X|Xs], [X1|Xs1]) :-
	edge_to_dot(X, X1),
	map_edge_to_dot(Xs, Xs1).
map_edge_to_dot([], []).
 
node_to_dot(node(Action, Args)-Id, Options, node(Id, Atts)) :-
 	memberchk(action_attributes(Module, Pred, Param), Options),
 	!,
	Call =.. [Pred, Param, Action, Args, Atts],
	call(Module:Call).
node_to_dot(node(Action, Args)-Id, _, node(Id, Atts)) :-
	action_attributes(Action, Args, Atts).

edge_to_dot(Id-Id1, edge(Id, Id1, [])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% View Plan
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

view_plan(Plan) :-
	view_plan(Plan, []).

view_plan(Plan, Options) :-
	current_prolog_flag(unix, true),
	!,
	plan_to_dotgraph(Plan, Options, DG),
	process_dotgraph(DG, 'image/gif', File, _),
	config:config(planner_image_viewer, Viewer),
	format(atom(Command), '(~w ~w ; rm ~w) &', [Viewer, File, File]),
	msg('Executing ~q', Command),
	( shell(Command) ->
          true
	; err('Shell command failed')
	).
view_plan(_, _) :-
	err('View plan is not supported.').

view_gplan(GPlan) :-
	view_gplan(GPlan, []).
view_gplan(gplan(Nodes, Edges), Options) :-
	current_prolog_flag(unix, true),
	!,
	gplan_to_dotgraph(gplan(Nodes, Edges), Options, DG),
	process_dotgraph(DG, 'image/gif', File, _),
	format(atom(Command), '(xli ~w ; rm ~w) &', [File, File]),
	( shell(Command) ->
          true
	; err('Shell command failed')
	).
view_gplan(_, _) :-
	err('View plan is not supported.').
