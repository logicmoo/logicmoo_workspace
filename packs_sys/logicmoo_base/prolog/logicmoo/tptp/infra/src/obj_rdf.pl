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

:- module(obj_rdf, [infer_types/6]).

:- use_module(knowledgebase).
:- use_module('swilib/graphs').
:- use_module('graphs_util').

:- use_module('swilib/err').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Type Inference
%%%% 
%%%% Hmmmm, that RDF...
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% infer_types(+KB, +Facts,
%%%%             -TypeMap, -PropMap, -ClassGraph, -ClassesAndProps)
%%%%
%%%% 
%%%% TypeMap associates with each item (i.e. subject or object) in Facts
%%%% its type. PropMap associates with each property in Facts an effective
%%%% property. (Since properties are single valued, sub-properties can
%%%% be substituted by the effective property). ClassGraph is the class
%%%% graph restricted to classes occuring in the TypeMap.
%%%% ClassesAndProps is a list containing an element Class-Properties
%%%% for each used class. Properties is a list of the Class's properties
%%%% that are used.
%%%%
infer_types(KB, Facts, TypeMap, PropMap, ClassGraph, ClassesAndProps) :-

	findall(Fact, ( member(Fact, Facts), \+ meta_fact(Fact) ), ObjFacts),
	extend_knowledgebase(KB, ObjFacts, KB1),
	
	facts_items(Facts, Items),
	map_assoc_type(Items, KB1, TypeMap),
	delete_knowledgebase(KB1),
	facts_properties(Facts, Props),
	map_assoc_prop(Props, KB, PropMap),
 	( setof(C, I^member(I-C, TypeMap), Classes) ->
	  true
 	; Classes = []
 	),
	classgraph(Classes, KB, ClassGraph),
	classes_and_props(Classes, Facts, TypeMap, PropMap, ClassesAndProps).

meta_property(rdfs_domain).
meta_property(rdfs_range).
meta_property(rdfs_subClassOf).
meta_property(rdfs_subPropertyOf).

meta_fact(rdf(_, P, _)) :-
	meta_property(P),
	!.
meta_fact(rdf(_, rdf_type, rdfs_Class)).
meta_fact(rdf(_, rdf_type, rdf_Property)).

facts_properties(Facts, Properties) :-
	( setof(P, F^(member(F, Facts), fact_property(F, P)), Properties) ->
	  true
	; Properties = []
	).

facts_items(Facts, Items) :-
	( setof(I, F^(member(F, Facts), fact_item(F, I)), Items) ->
	  true
	; Items = []
	).

fact_property(rdf(_, P, _), P) :-
	P \= rdf_type.

fact_item(rdf(S, _, _), S).
fact_item(rdf(_, P, O), O) :-
	O \= literal(_),
	P \= rdf_type.


%% 
%% Ways to restrict types:
%% - identify equal types
%% - use only types explicitely mentioned in input (hmmm)
%% - make a single type for multiple types occuring always together
%% 

assoc_prop(P, KB, P-P1) :-
	( setof(P2, max_prop(KB, P, P2), [P1]) ->
	  true
	; err('Can not standardize property ~q.', [P])
	).

map_assoc_prop([X|Xs], Y, [X1|Xs1]) :-
	assoc_prop(X, Y, X1),
	map_assoc_prop(Xs, Y, Xs1).
map_assoc_prop([], _, []).

max_prop(KB, Prop, Prop1) :-
	( Prop1 = Prop
	; fact(KB, Prop, rdfs_subPropertyOf, Prop1)
	),
	\+ ( fact(KB, Prop1, rdfs_subPropertyOf, Prop2),
	     Prop2 \= Prop1,
	     \+ fact(KB, Prop2, rdfs_subPropertyOf, Prop1) ).

assoc_type(Item, KB, Item-Type) :-
	( setof(Class, item_min_class(KB, Item, Class), Types) ->
	  ( Types = [Type] ->
	    true
	  ; err('Item ~q has multiple direct types: ~q.', [Item, Types])
	  )
	; err('Can not determine the type of ~q.', [Item])
	).

map_assoc_type([X|Xs], Y1, [X1|Xs1]) :-
	assoc_type(X, Y1, X1),
	map_assoc_type(Xs, Y1, Xs1).
map_assoc_type([], _, []).

item_min_class(KB, Item, Class) :-	
	fact(KB, Item, rdf_type, Class),
	\+ ( fact(KB, Item, rdf_type, Class2),
	     Class2 \= Class,
	     fact(KB, Class2, rdfs_subClassOf, Class) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Classgraph
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classgraph(Classes, KB, ClassGraph) :-
	( setof(C1-C2, ( member(C2, Classes),
			 fact(KB, C1, rdfs_subClassOf, C2),
		         memberchk(C1, Classes)
		       ),
		CG) ->
	  pgraph_minimize(CG, ClassGraph)
	; ClassGraph = []
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Classes and Props
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^

%%
%% ASSUMED that '' does not occur as a property.
%%
class_prop(_, Classes, _, _, Class, '') :-
	member(Class, Classes).

% class_prop(_, _, TMap, _, Class, inf_id) :-
% 	member(S-Class, TMap),
% 	\+ S = blank(_).
% *** For now - allow inf_id for all, since parameters might bind object
% late, and we currently dont have syntax to add "can be nonblank
% declarations".
%
class_prop(_, Classes, _, _, Class, inf_id) :-
	member(Class, Classes).
class_prop(Facts, _, TMap, PMap, Class, Prop) :-
	member(rdf(S, P, _), Facts),
	P \= rdf_type,
	P \= inf_var,
	memberchk(S-Class, TMap),
	memberchk(P-Prop, PMap).

classes_and_props(Classes, Facts, TMap, PMap, ClassesAndProps) :-
	setof(Class-Props,
	      setof(Prop,
		    class_prop(Facts, Classes, TMap, PMap, Class, Prop),
		    [_|Props]),
	      ClassesAndProps).
classes_and_props([], _, _, _, []).
