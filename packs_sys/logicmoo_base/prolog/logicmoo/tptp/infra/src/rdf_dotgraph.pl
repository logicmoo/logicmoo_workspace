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

:- module(rdf_dotgraph, [triples_to_dotgraph/3,
			 triples_to_dotgraph/4]).

:- use_module(pages_queries).
:- use_module(pages_util).
:- use_module(textutil).
:- use_module('swilib/err').

triples_to_dotgraph(KB, Triples, Dotgraph) :-
	triples_to_dotgraph(KB, Triples, [], Dotgraph).
		    
triples_to_dotgraph(KB, Triples, _Options,
		    dotgraph('RDF', [graph_attributes(GAs),
				     node_defaults(NDs),
				     edge_defaults(EDs) | Stmts])) :-
	graph_attributes(GAs),
	node_defaults(NDs),
	edge_defaults(EDs),
	findall(Stmt, td_1(KB, Triples, Stmt), Stmts).

bgcolor_class('0 0.15 1.0').
bgcolor_property('0.67 0.15 1.0').
bgcolor_object('0.33 0.15 1.0').
bgcolor_literal('0.0 0.0 0.9').
color_property('0.0 1.0 0.0').
color_property(rdf_type, '0.33 1.0 0.6').
color_property(rdfs_subClassOf, '0 1.0 0.6').
color_property(rdfs_subPropertyOf, '0.67 1.0 0.6').

graph_attributes([rankdir='LR',
		  bgcolor='0.083 0.13 1.0'
		 % size='10,10'
		 % , fontpath='/home/ch/tmp-fonts'
		 % , ratio=auto
		 % , concentrate='true'
		 ]).

node_defaults([fontname=Fontname, fontsize=Fontsize]) :-
	config(pages_dotgraph_fontname, Fontname),
	config(pages_dotgraph_fontsize, Fontsize).

edge_defaults([fontname=Fontname, fontsize=Fontsize]) :-
	config(pages_dotgraph_fontname, Fontname),
	config(pages_dotgraph_fontsize, Fontsize).

item_node_attributes([shape=ellipse,
		      color=black,
		      style=filled]).

item_node_attributes(KB, Item, [fillcolor=Color|General]) :-
	item_node_attributes(General),
	( fact(KB, Item, rdf_type, rdfs_Class) ->
	  bgcolor_class(Color)
	; fact(KB, Item, rdf_type, rdf_Property) ->
	  bgcolor_property(Color)
        ; bgcolor_object(Color)
	).

literal_node_attributes([shape=ellipse,
			 color=black,
			 fillcolor=Color,
			 style=filled]) :-
	bgcolor_literal(Color).

edge_attributes(_, Property, [fontcolor=Color, color=Color]) :-
	( color_property(Property, Color) ->
	  true
	; color_property(Color)
	).

td_1(KB, Triples, node(I, [label=Label, 'URL'=Uri | Atts])) :-
	setof(I1, P^I2^( member(rdf(I1,P,I2), Triples)
		       ; member(rdf(I2,P,I1), Triples)
		       ),
	      Is),
	member(I3, Is),
	( I3 = literal(_) ->
	  fail
	; item_id(I3, I),
	  item_label(KB, I3, Label),
	  item_node_attributes(KB, I3, Atts),
	  item_uri(KB, dotgraph, I3, Uri)
	).
td_1(KB, Triples, Stmt) :-
	literal_node_attributes(LitAtts),
	member( rdf(S, P, O), Triples ),	
	edge_attributes(KB, P, Atts),
	item_id(S, S1),
	item_label(KB, P, P1),
	( O = literal(Literal) ->
	  gensym(lit, O1),
	  ( item_uri(KB, object, O, Uri),
	    ( Literal = [element(Tag, _, _)|_] ->
	      ( Tag = _:Tag1 ->
		true
	      ; Tag1 = Tag
	      ),
	      concat_atom(['<', Tag1, '...'], Literal1)
	    ; abbreviate(Literal, 25, Literal1)
	    ),
	    concat_atom(['"', Literal1, '"'], Literal2),
	    Stmt = node(O1, [label=Literal2, 'URL'=Uri |LitAtts])
          ; Stmt = edge(S1, O1, [label=P1|Atts])
	  )
	; item_id(O, O1),
	  Stmt = edge(S1, O1, [label=P1|Atts])
	).

item_id(blank(BId), Id) :-
	!,
	term_to_atom(blank(BId), Id).
item_id(Item, Item) :-
	atom(Item),
	!.
item_id(Item, _) :-
	err('Can not convert item: ~q.', [Item]).

item_label(_, blank(BId), Id) :-
	!,
	term_to_atom(blank(BId), Id).
item_label(KB, Item, Label) :-
	atom(Item),
	!,
	resource_pretty_name(KB, Item, Label).
item_label(_, Item, _) :-
	err('Can not convert item: ~q.', [Item]).

