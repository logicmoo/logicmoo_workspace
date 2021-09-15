%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(tabx_dotgraph, [tabx_to_dotgraph/2]).
:- use_module(swilib(dotgraph)).
:- use_module(swilib(sysdep)).
:- use_module(swilib(info)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Tabx to Dotgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(dotnode, _, 0).

mk_dotnode_id(Id) :-
	flag_inc(dotnode, N),
	concat_atom(['n', N], Id).

reset_dotnode_id :-
	flag(dotnode, _, 0).

:- dynamic(current_style/1).

tabx_to_dotgraph(Tabx, Dotgraph) :-
	retractall(current_style(_)),
	( Dotgraph = printstyle(Dotgraph1) ->
	  assert(current_style(print))
	; Dotgraph1 = Dotgraph,
	  assert(current_style(screen))
	),
	( atom(Dotgraph1) ->
	  tabx_to_dotgraph_1(Tabx, DG),
	  ( sub_atom(Dotgraph1, _, _, 0, '.gif') ->
	    info(10, 'Writing GIF dotgraph to: ~w', [Dotgraph1]),
	    write_gif_dotgraph(DG, Dotgraph1)
	  ; info(10, 'Writing PNG dotgraph to: ~w', [Dotgraph1]),
	    write_png_dotgraph(DG, Dotgraph1)
	  )
	; tabx_to_dotgraph_1(Tabx, Dotgraph1)
	).

tabx_to_dotgraph_1(Tabx, Dotgraph) :-
	Dotgraph = dotgraph(d1, [graph_attributes([]),
				 node(root, [label='Root'|NodeAtts])
				| Rest]),
	tabx_root_atts(NodeAtts),
	map_tab_to_dot(Tabx, root, Rest, []).


tab_to_dot(A, gamma(L, _, C, S), X, Y) :-
	!,
	tab_to_dot(A, tab(L, C, S), X, Y).
tab_to_dot(ParentId, tab(L, C, SubTabs),
	   [edge(ParentId, Id, [dir=forward, arrowsize=0.7]),
	    node(Id, [label=Label | NodeAtts]) |Rest], Rest1) :-
	!,
	mk_dotnode_id(Id),
	copy_term(L, L1),
	numbervars(L1, 0, _),
	format(atom(Label), '~w', [L1]),
	tabx_node_atts(C, NodeAtts),
	map_tab_to_dot(SubTabs, Id, Rest, Rest1).
tab_to_dot(ParentId, closed(_, _, C2),
	   [edge(ParentId, Id, [dir=none|EdgeAtts]),
 	    node(Id, [label=Label | NodeAtts])|Rest], Rest) :-
	mk_dotnode_id(Id),
	tabx_closed_atts(C2, EdgeAtts, NodeAtts),
	format(atom(Label), '*', []).
% ... utf8 seems not to work if prolog_flag and file are utf8 ?
%	format(atom(Label), '⊥', []).

% tab_to_dot(ParentId, closed(_, _, C2),
% 	   [edge(ParentId, Id, [dir=none]),
%  	    node(Id, [label=Label,
% 		      fontcolor=red,
%  		      shape=box, style=filled, color=Color])|Rest], Rest) :-
% 	mk_dotnode_id(Id),
% 	tabx_dot_bg_color(C2, Color),
% 	format(atom(Label), '⊥', []).

map_tab_to_dot([X|Xs], Parent, Xs1, Xs2) :-
	tab_to_dot(Parent, X, Xs1, Xs3),
	map_tab_to_dot(Xs, Parent, Xs3, Xs2).
map_tab_to_dot([], _, Xs, Xs).

tabx_root_atts(Atts) :-
	current_style(Style),
	tabx_root_atts(Style, Atts).
	
tabx_node_atts(Color, Atts) :-
	current_style(Style),
	tabx_node_atts(Style, Color, Atts).

tabx_closed_atts(Color, AttsEdge, AttsNode) :-
	current_style(Style),
	tabx_closed_atts(Style, Color, AttsEdge, AttsNode).

tabx_root_atts(screen, [style=filled, color=gray80]).
tabx_root_atts(screen1, [style=filled, color=gray80]).
tabx_root_atts(print, [style=dashed, color=black]).

tabx_node_atts(screen, a, [style=filled, color='#ffb6c1']) :- !.
tabx_node_atts(screen, b, [style=filled, color='#94f098']) :- !.
tabx_node_atts(screen, _, [style=filled, color='#deb887']) :- !.
%%%%
tabx_node_atts(screen1, a, [style=filled, color=lightblue2]) :- !.
tabx_node_atts(screen1, b, [style=filled, color=lightpink1]) :- !.
tabx_node_atts(screen1, _, [style=filled, color=plum1]) :- !.
%%%%
tabx_node_atts(print, a, [style=solid, color=black]) :- !.
tabx_node_atts(print, b, [style=filled, color=gray80]) :- !.
tabx_node_atts(print, _, [style=dotted, color=black]) :- !.

tabx_closed_atts(screen, a, [color='#cd0000'], [shape=none, fontcolor='#cd0000']) :- !.
tabx_closed_atts(screen, b, [color='#007010'], [shape=none, fontcolor='#008010']) :- !.
tabx_closed_atts(screen, _, [color='#b8860b'], [shape=none, fontcolor='#b8700b']) :- !.
%%%%
tabx_closed_atts(screen1, a, [color=dodgerblue2], [shape=none, fontcolor=dodgerblue2]) :- !.
tabx_closed_atts(screen1, b, [color=red3], [shape=none, fontcolor=red3]) :- !.
tabx_closed_atts(screen1, _, [color=purple2], [shape=none, fontcolor=purple2]) :- !.
%%%%
tabx_closed_atts(print, a, [color=black], [shape=square, fontcolor=black]) :- !.
tabx_closed_atts(print, b, [color=black], [shape=square, style=filled, color=gray80, fontcolor=black]) :- !.
tabx_closed_atts(print, _, [color=black], [shape=square, style=dotted, fontcolor=black]) :- !.



