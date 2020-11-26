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

:- module(pages_queries,
          [ resource_pretty_name/3,
	    resource_pretty_name/5,

	    class_direct_superclasses/3,
	    class_direct_subclasses/3,
	    class_equal_classes/3,
	    class_direct_instances/3,
	    class_has_a_direct_instance/2,
	    class_effective_range_properties/3,

	    class_effective_domain_properties/3,
	    property_effective_range_classes/3,
	    
	    property_direct_superproperties/3,
	    property_direct_subproperties/3,
	    property_equal_properties/3,
	    property_direct_domain_classes/3,
	    property_direct_range_classes/3,
	    property_direct_extension/3,

	    object_direct_properties/3,
	    object_direct_inverse_properties/3,
	    object_direct_types/3,

	    object_triples/3
	    
	   ]).

:- use_module(knowledgebase).
:- use_module(queries).


%%%% 
%%%% Answers can be unsorted and might contain duplicates. (They have to be
%%%% sorted anyway, according to presentation criteria, afterwards.)
%%%% 

%%%% 
%%%% *** TODO: adapt to Model Theory
%%%%      ie rewrite the direct stuff to get "minimal"
%%%%      subproperty: consider in domain/range issues
%%%%      property extansion arity+1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% General Queries
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resource_pretty_name(KB, Resource, PrettyName) :-
	%% hmmm - is the xml:lang resolved at read-in time ???
	fact(KB, Resource, rdfs_label, literal(PrettyName1)),
	!,
	( atom(PrettyName1) ->
	  PrettyName = PrettyName1
	; term_to_atom(PrettyName1, PrettyName)
	).
resource_pretty_name(KB, Resource, PrettyName) :-
	atom(Resource),
	match_namespace(KB, Resource, _, NsAbbrev, Name),
	!,
	( NsAbbrev = '' ->
	  PrettyName = Name
	; concat_atom([NsAbbrev, ':', Name], PrettyName)
	).
resource_pretty_name(_, Resource, Resource) :-
	atom(Resource),
	!.
resource_pretty_name(_, Resource, PrettyName) :-
	term_to_atom(Resource, PrettyName).


resource_pretty_name(KB, Resource, _, _, PrettyName) :-
	fact(KB, Resource, rdfs_label, literal(PrettyName1)),
	!,
	( atom(PrettyName1) ->
	  PrettyName = PrettyName1
	; term_to_atom(PrettyName1, PrettyName)
	).
resource_pretty_name(KB, Resource, Namespace, NsAbbrev, Name) :-
	atom(Resource),
	match_namespace(KB, Resource, Namespace, NsAbbrev, Name),
	!.
resource_pretty_name(_, Resource, _, _, Resource) :-
	atom(Resource),
	!.
resource_pretty_name(_, Resource, _, _, Name) :-
	term_to_atom(Resource, Name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Class Queries
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

class_direct_superclasses(KB, Class, Superclasses) :-
	findall(Super,
	        tp_direct_sub_of(KB, rdfs_subClassOf, Class, Super),
	        Superclasses).

class_direct_subclasses(KB, Class, Subclasses) :-
	findall(Sub, 
	        tp_direct_sub_of(KB, rdfs_subClassOf, Sub, Class),
		Subclasses).

class_equal_classes(KB, Class, Classes) :-
	findall(Class1, 
	        tp_equal(KB, rdfs_subClassOf, Class1, Class),
		Classes).

%%%% 
%%%% class_effective_domain_properties/3 and
%%%% property_effective_range_classes/3 are not only used for displaying
%%%% pages, but also e.g. for DTD construction, i.e. their semantics
%%%% is important.
%%%% 	
class_effective_domain_properties(KB, Class, Properties) :-
 	findall(P,
	        ( fact(KB, P, rdf_type, rdf_Property),
		  \+ ( tp_lessq(KB, rdfs_subPropertyOf, P, P1),
		       fact(KB, P1, rdfs_domain, C1),
		       \+ tp_lessq(KB, rdfs_subClassOf, Class, C1) )),
		Properties).

property_effective_range_classes(KB, Property, Classes) :-
 	findall(C,
	        ( fact(KB, C, rdf_type, rdfs_Class),
		  \+ ( tp_lessq(KB, rdfs_subPropertyOf, Property, P1),
		       fact(KB, P1, rdfs_range, C1),
		       \+ tp_lessq(KB, rdfs_subClassOf, C, C1) )),
		Classes).

class_effective_range_properties(KB, Class, Properties) :-
	%% *** ?
 	findall(P,
	        ( fact(KB, P1, rdfs_range, C1),
		  tp_lessq(KB, rdfs_subClassOf, C1, Class),
		  tp_lessq(KB, rdfs_subPropertyOf, P, P1)
		),
		Properties).

class_direct_instances(KB, Class, Instances) :-
	findall(Instance,
	         ( min_class(KB, Instance, rdf_type, Class),
		   Instance \= literal(_)  %% currently for rdf_Resource ***
		 ),
		Instances).

class_has_a_direct_instance(KB, Class) :-
	once(( min_class(KB, Instance, rdf_type, Class),
	       Instance \= literal(_) )).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Object Queries
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_direct_properties(KB, Object, Groups) :-
	setof(Prop-Vals, 
	      setof(Val, min_property(KB, Object, Prop, Val), Vals),
	      Groups),
	!.
object_direct_properties(_, _, []).

object_direct_inverse_properties(KB, Object, Groups) :-
	setof(Prop-Vals, 
	      setof(Val, min_property(KB, Val, Prop, Object), Vals),
	      Groups),
	!.
object_direct_inverse_properties(_, _, []).

object_direct_types(KB, Object, Classes) :-
	findall(Class, 
	        min_class(KB, Object, rdf_type, Class),
		Classes).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Property Queries
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_direct_superproperties(KB, Property, Superproperties) :-
	findall(Super,
	        tp_direct_sub_of(KB, rdfs_subPropertyOf, Property, Super), 
	        Superproperties).

property_direct_subproperties(KB, Property, Subproperties) :-
	findall(Sub,
	        tp_direct_sub_of(KB, rdfs_subPropertyOf, Sub, Property),
		Subproperties).

property_equal_properties(KB, Property, Properties) :-
	findall(Property1,
	        tp_equal(KB, rdfs_subPropertyOf, Property, Property1),
		Properties).

property_direct_domain_classes(KB, Property, Classes) :-
	findall(Class,
	        min_dr_class(KB, rdfs_domain, Property, Class),
		Classes).

property_direct_range_classes(KB, Property, Classes) :-
	findall(Class,
	        min_dr_class(KB, rdfs_range, Property, Class),
		Classes).

%%
%% See min_class/4, which cannot be used here directly.
%%
min_dr_class(KB, DR, P, C) :-
	dr_class(KB, DR, P, C),
	\+ ( dr_class(KB, DR, P, C1),
	     C1 \= C,
	     fact(KB, C1, rdfs_subClassOf, C),
	     \+ fact(KB, C, rdfs_subClassOf, C1) ).

dr_class(KB, DR, P, C) :-
	tp_lessq(KB, rdfs_Property, P, P1),
	fact(KB, P1, DR, C).


property_direct_extension(KB, Property, Extension) :-
	findall(pair(S, O),
	        min_property(KB, S, Property, O),
		Extension ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Object Triples
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

object_triples(KB, S, Triples) :-
	setof(Triple, object_triple(KB, S, [], Triple), Triples),
	!.
object_triples(_, _, []).

object_triple(KB, S, H, T) :-
	\+ memberchk(S, H),
	informative_fact(KB, S, P, O),
	( T = rdf(S, P, O)
	; O = blank(_),
	  object_triple(KB, O, [S|H], T)
	).


