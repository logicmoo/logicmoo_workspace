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
%%%% RDFS DTD
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(rdfs_dtd, [print_dtd/1]).

:- use_module(rdf_writer).
:- use_module(rdf_convert).
:- use_module(xml_writer).
:- use_module(pages_queries).
:- use_module('swilib/err').
:- use_module(library(ordsets)).
:- use_module(textutil).
:- use_module(knowledgebase).

%%%%
%%%% print_dtd(+KB)
%%%%
%%%% Print out a DTD corresponding to the RDFS schema in the knowledgbase
%%%% KB. Not all of that fancy RDF stuff is supported by the DTD, so a valid
%%%% document might fail to validate with it. But the "striped" syntax is
%%%% fully supported.
%%%%
%%%% Features
%%%%
%%%% - The entity %nodeElement; produces a toplevel node element.
%%%% - Namespaces of the KB are used. Additionally required namespaces
%%%%   are generated.
%%%%
%%%% Restrictions and Bugs:
%%%%
%%%% - For containers, rdfs_member and those rdf__N properties that actually
%%%%   occur in the KB are given as properties. This issue is difficult to
%%%%   handle cleanly with full generality.
%%%% - No support for "rdf:parseType=Resource" stuff.
%%%% - No support for generic "rdf:Description" elements (could easily
%%%%   be added).
%%%% - Not sure if quoting is sufficient or even theoretically possible.
%%%% - Performance suffers from the repeated "effective..." queries.
%%%% - System maintained "hidden" information also stored in the KB,
%%%%   such as namespace abbreviations is not output.
%%%%
print_dtd(KB) :-
	( setof(C,
		( fact(KB, C, rdf_type, rdfs_Class),
		  nonblank(C),
		  nonhidden(C),
		  C \= rdfs_Literal ),
		Classes) ->
	    true
	; Classes = []
	),
	( setof(P,
		( fact(KB, P, rdf_type, rdf_Property),
		  nonblank(P),
		  nonhidden(P) ),
		Properties) ->
	  true
	; Properties = []
	),
	( setof(N-A, fact(KB, N, sys_abbreviation, literal(A)), Ns) ->
	  true
	; Ns=[]
	),
	append(Classes, Properties, NsItems),
	gen_required_namespaces(NsItems, Ns, Ns4),
	print_header(KB),
	print_node_element_entity(Classes, Ns4),
	print_rdf_element(Ns4),
	map_assoc_range_classes(Properties, KB, PCs),
	literal_range_properties(PCs, LProps),
	map_xml_name(LProps, Ns4, LProps1),
	( member(Class, Classes),
	  print_class_element(Class, KB, LProps1, Ns4),
	  fail
	; true
	),
	( member(PC, PCs),
	  print_property_element(PC, KB, Ns4),
	  fail
	; true
	),
	print_footer.

print_rdf_element(NAs) :-
	format('<!ELEMENT rdf:RDF ( %nodeElement; )* >~n'),
	format('<!ATTLIST rdf:RDF'),
	format('~n        xml:base CDATA #IMPLIED'),
	format('~n        xml:lang NMTOKEN #IMPLIED'),
	format('~n        xmlns CDATA #IMPLIED'),
	( member(N-A, NAs),
	  A \= '',
	  xml_quote(N, [], N1),
	  xml_quote(A, [], A1),	    
	  format('~n        xmlns:~w CDATA "~w"', [A1, N1]),
	  fail
	; true
	),
	format(' >~n~n').

print_node_element_entity(Classes, NAs) :-
	format('<!ENTITY % nodeElement "'),
	print_cases(Classes, false, NAs),
	format('" >~n~n').

print_class_element(Class, KB, _, NAs) :-
	print_comments(Class, KB, NAs, 'Class'),
	fail.
print_class_element(Class, KB, LProps, NAs) :-
	class_effective_domain_properties(KB, Class, Props0),
	remove_hidden(Props0, Props),
	format('<!ELEMENT '),
	print_name(Class, NAs),
	print_cases(Props, true, NAs),
	format(' >~n'),
	format('<!ATTLIST '),
	print_name(Class, NAs),
	format('~n        '),
	print_name(rdf_about, NAs),
	format(' CDATA #IMPLIED'),
	format('~n        '),
	print_name(rdf_ID, NAs),
	format(' ID #IMPLIED'),
	format('~n        '),
	print_name(rdf_bagID, NAs),
	format(' NMTOKEN #IMPLIED'),
	format('~n        '),
	print_name(rdf_type, NAs),
	format(' CDATA #IMPLIED'),
	format('~n        xml:base CDATA #IMPLIED'),
	format('~n        xml:lang NMTOKEN #IMPLIED'),
	format('~n        xmlns CDATA #IMPLIED'),
	map_xml_name(Props, NAs, Props1),
	sort(Props1, Props2),
	( member(Prop, Props2),
	  memberchk(Prop, LProps),	    
	  format('~n        '),
	  write(Prop),
	  format(' CDATA #IMPLIED'),
	  fail
	; true
	),
	format(' >~n~n').

print_property_element(Property-_, KB, NAs) :-
	print_comments(Property, KB, NAs, 'Property'),
	fail.
print_property_element(Property-Classes, _KB, NAs) :-
	format('<!ELEMENT '),
	print_name(Property, NAs),
	( is_literal_prop(Classes) ->
	  format(' ANY')
	; print_cases(Classes, true, NAs)
	),
	format(' >~n'),
	format('<!ATTLIST '),
	print_name(Property, NAs),
	format('~n        rdf:resource CDATA #IMPLIED'),
%        format('~n        rdf:parseType ( Resource | Literal ) #IMPLIED'),
 	( is_literal_prop(Classes) ->
 	  format('~n        rdf:parseType ( Literal ) #IMPLIED')
 	; true
 	),
	format('~n        rdf:ID ID #IMPLIED'),
        format('~n        rdf:bagID NMTOKEN #IMPLIED'),
	format('~n        xml:base CDATA #IMPLIED'),
	format('~n        xml:lang NMTOKEN #IMPLIED'),
	format('~n        xmlns CDATA #IMPLIED'),
	format(' >~n~n').

print_cases(Cases, Repeat, NAs) :-
	map_xml_name(Cases, NAs, Cases1),
	sort(Cases1, Cases2),
	( Cases2 = [] ->
	  format(' EMPTY')
	; Cases2 = [C1|Cs1],
	  format('~n        ( '),
	  write(C1),
	  ( member(C2, Cs1),
	    format('~n        | '),
	    write(C2),
	    fail
	  ; true
	  ),
	  format(' )'),
	  ( Repeat = true -> format('*') ; true )
	).

map_xml_name([X|Xs], Y1, [X1|Xs1]) :-
	xml_name(X, Y1, X1),
	map_xml_name(Xs, Y1, Xs1).
map_xml_name([], _, []).

print_header(KB) :-
	get_time(Stamp),
	format_time(atom(Time), '%F %T', Stamp),
	format('<!-- RDFS DTD generated by InfraEngine ~w -->~n', [Time]),
	kb_id(KB, Id),
	format('<!-- Knowledgebase: ~q -->~n', [Id]),
	findall( Doc, catalog_fact(Id, sys_document, Doc), Docs ),
	sort(Docs, Docs1),
	( Docs1 = [] ->
	  true
	; format('<!-- The following documents have been loaded into this knowledgebase: -->~n',
		 []),
	  ( member(Doc, Docs),
	    format('<!-- ~w -->~n', [Doc]),
	    fail
	  ; true
	  )
	),
	nl.
	    	
print_footer :-
	format('<!-- EOF -->~n').

print_name(I, Ns) :-
	xml_name(I, Ns, I1),
	write(I1).

xml_name(I, Ns, I1) :-
	public_item(I, I2),
	member(N-A, Ns),
	sub_atom(I2, 0, B, R, N),
	R > 0,
	!,
	sub_atom(I2, B, _, 0, L),
	( A = '' -> I3 = L
	; concat_atom([A, ':', L], I3)
	),
	xml_quote(I3, [], I1).
xml_name(I, Ns, _) :-
	err('Failed to print xml name ~q ~q.', [I, Ns]).

gen_required_namespaces(Is, Ns, Ns1) :-
	sort(Ns, Ns2),
	reverse(Ns2, Ns3),
	grn_1(Is, Ns3, Ns4),
	sort(Ns4, Ns5),
	reverse(Ns5, Ns1).

grn_1([I|Is], Ns, [N-A|Ns1]) :-
	atom(I),
	!,
	public_item(I, I1),
	( member(N-A, Ns), sub_atom(I1, 0, _, R, N), R > 0 ->
          Ns2 = Ns
	; namespace_break(I1, N, _) ->
	  once(( find_name(n, A), \+ (memberchk(_-A, Ns)) )),
	  revord_add_element(Ns, N-A, Ns2)
	; err('Namespace breaking failed for ~q.', [I])
	),
	grn_1(Is, Ns2, Ns1).
grn_1([I|_], _, _) :-
	err('Bad class or property: ~q.', [I]).
grn_1([], _, []).	

revord_add_element([], El, [El]). 
revord_add_element([H|T], El, Add) :-
    compare(Order, H, El),
    rev_addel(Order, H, T, El, Add).

rev_addel(>, H, T,  El, [H|Add]) :-
    revord_add_element(T, El, Add).
rev_addel(=, H, T, _El, [H|T]). 
rev_addel(<, H, T,  El, [El,H|T]).

			
find_name(Name, Name).
find_name(Name, Name1) :-
	gen_new_name_n(Name, 1, Name1).

gen_new_name_n(Name, N, Name1) :-
	concat_atom([Name, N], Name1).
gen_new_name_n(Name, N, Name1) :-
	N1 is N + 1,
	gen_new_name_n(Name, N1, Name1).

nonblank(blank(_)) :-
	!,
	fail.
nonblank(_).

remove_hidden([X|Xs], Xs1) :-
	hidden(X),
	!,
	remove_hidden(Xs, Xs1).
remove_hidden([X|Xs], [X|Xs1]) :-
	remove_hidden(Xs, Xs1).
remove_hidden([], []).

nonhidden(X) :-
	\+ hidden(X).

hidden(X) :-
	atom(X),
	sub_atom(X, 0, _, _, 'sys_').

map_assoc_range_classes([X|Xs], Y, [X1|Xs1]) :-
	assoc_range_classes(X, Y, X1),
	map_assoc_range_classes(Xs, Y, Xs1).
map_assoc_range_classes([], _, []).

assoc_range_classes(Prop, KB, Prop-Classes) :-
	property_effective_range_classes(KB, Prop, Classes1),
	remove_hidden(Classes1, Classes).

literal_range_properties(PCs, Ps) :-
	findall(P, 
		( member(P-C, PCs),
		  is_literal_prop(C)
		),
		Ps).

is_literal_prop([]) :-
	!.
is_literal_prop(Cs) :-
	memberchk(rdfs_Literal, Cs).

print_comments(Item, KB, NAs, Type) :-
	( fact(KB, Item, rdfs_comment, literal(Comment)),
	  ( atom(Comment) ->
	    Comment1 = Comment
	  ; term_to_atom(Comment, Comment1) % *** or extract atoms...
	  ),
	  xml_quote(Comment, [], XComment),
	  format('<!-- ~w ', [Type]),
	  print_name(Item, NAs),
	  nl,
	  write_lines(XComment, 5, 78),  
	  format('~n-->~n'),
	  fail
	; true
	).
