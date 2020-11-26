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

:- module(obj_rdfxml, [xml_to_obj/4, fix_blanks/2]).

:- use_module(rdf_convert).
:- use_module('swilib/err').
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% RDFXML Description to Obj
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% TODO: perhaps base this on an infoset RDF representation of XML instead
%%%% of the prolog library(sgml) terms.
%%%%

%%%% 
%%%% Notes: Element should be
%%%%
%%%% - converted through rdf_read:prepare_xml_rdf
%%%% - blanks "fixed" (substituted by special rdf_about resources)
%%%%   (see rdf_read).
%%%% - but will be replaced by terms here through canonicalize_elements.
%%%%
xml_to_obj(Element, TMap, PMap, Obj) :-
	canonicalize_element(Element, Element1),
	dte(Element1, TMap, PMap, Obj).

dte(element(_, A, C), TMap, PMap, obj(Type, PVs)) :-
	( once(( select(rdf_about=Id, A, A1) ; select(rdf_ID=Id, A, A1) )) ->
	  lookup_type(Id-Type, TMap)
	; err('No rdf:about or rdf:ID attribute in: ~q.', [A])
	),
	map_dtpa(A1, PMap, PVs1),
	map_dtpc(C, TMap, PMap, PVs2),
	append(PVs1, PVs2, PVs3),
	( Id \= blank(_) ->
	  PVs4 = [inf_id=Id|PVs3]
	; PVs4 = PVs3
	),
	sort(PVs4, PVs).

map_dtpa([A=_|ALs], PMap, PVs) :-
	dta_ignore(A),
	!,
	map_dtpa(ALs, PMap, PVs).
map_dtpa([A=L|ALs], PMap, [P=L|Os]) :-
	lookup_prop(A-P, PMap),
	map_dtpa(ALs, PMap, Os).
map_dtpa([], _, []).

map_dtpc([element(P, _, _)|Es], TMap, PMap, PVs) :-
	dta_ignore(P),
	!,
	map_dtpc(Es, TMap, PMap, PVs).
map_dtpc([element(P, A, C)|Es], TMap, PMap, [P1=V|PVs]) :-
	lookup_prop(P-P1, PMap),
	( C = [V], atom(V) ->
	  true
	; memberchk(rdf_parseType='Literal', A), C = [V1] ->
	  term_to_atom(V1, V)
        ; select(rdf_parseType='Resource', A, A1) ->
	  ( select(rdf_resource=O, A1, A2) ->
	    A3 = [rdf_about=O|A2]
	  ; A3 = A2
	  ),
	  dte(element(rdf_Description, A3, C), TMap, PMap, V)
        ; select(rdf_resource=O, A, A1) ->
	  V = obj(TO, PVsO),
	  lookup_type(O-TO, TMap),
	  map_dtpa(A1, PMap, PVsO1),
	  sort([inf_id=O|PVsO1], PVsO)
	; C = [element(_, _, _)] ->
	  C = [E],
	  dte(E, TMap, PMap, V)
	; err('Bad property structure for: ~q.', [element(P, A, C)])
	),
	map_dtpc(Es, TMap, PMap, PVs).
map_dtpc([], _, _, []).


lookup_type(X, Y) :-
	memberchk(X, Y),
	!.
lookup_type(X-_, _) :-
	err('Type lookup failed for ~q.', [X]).

lookup_prop(X, Y) :-
	memberchk(X, Y),
	!.
lookup_prop(X-_, _) :-
	err('Property lookup failed for ~q.', [X]).

dta_ignore(rdf_type).
dta_ignore(rdf_about).
dta_ignore(rdf_ID).
dta_ignore(rdf_parseType).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Canonicalize XML
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonicalize_element(element(T, A, C), element(T1, A1, C1)) :-
	!,
	canonicalize_item(T, T1),
	map_cda(A, A1),
	map_canonicalize_element(C, C1).
canonicalize_element(X, X).

map_canonicalize_element([X|Xs], [X1|Xs1]) :-
	canonicalize_element(X, X1),
	map_canonicalize_element(Xs, Xs1).
map_canonicalize_element([], []).

map_cda([A=V|Xs], [A1=V1|Xs1]) :-
	canonicalize_item(A, A1),
	canonicalize_attribute_value(A1, V, V1),
	map_cda(Xs, Xs1).
map_cda([], []).

canonicalize_attribute_value(A, V, V1) :-
	item_valued_attribute(A),
	!,
	canonicalize_item(V, V1).
canonicalize_attribute_value(_, V, V).

item_valued_attribute(rdf_about).
item_valued_attribute(rdf_ID).
item_valued_attribute(rdf_bagID).
item_valued_attribute(rdf_type).
item_valued_attribute(rdf_resource).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% The ids of blanks must be preserved through various processing stages.
%%%% Therefore they are converted to special URIs ans submitted as
%%%% rdf:about in the input XML. Those URIs are rewritten as corresponding
%%%% blank(_) term by canonicalize... in rdf_convert. knowledgebase_extend
%%%% preserves input blank ids.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fix_blanks(element(T, A, C), element(T, [RDF:about=TmpUri|A], C1)) :-
	RDF='http://www.w3.org/1999/02/22-rdf-syntax-ns#',
	\+ ( memberchk(RDF:about=_, A) ; memberchk(RDF:'ID'=_, A) ),
	!,
	gen_blank_uri(TmpUri),
	map_fix_blanks_prop(C, C1).
fix_blanks(element(T, A, C), element(T, A, C1)) :-
	!,
	map_fix_blanks_prop(C, C1).
fix_blanks(X, X).	   

fix_blanks_prop(element(P, A, C), element(P, A, C1)) :-
	!,
	map_fix_blanks(C, C1).
fix_blanks_prop(X, X).

map_fix_blanks([X|Xs], [X1|Xs1]) :-
	fix_blanks(X, X1),
	map_fix_blanks(Xs, Xs1).
map_fix_blanks([], []).

map_fix_blanks_prop([X|Xs], [X1|Xs1]) :-
	fix_blanks_prop(X, X1),
	map_fix_blanks_prop(Xs, Xs1).
map_fix_blanks_prop([], []).

gen_blank_uri(Uri) :-
	gensym(g, Id),
	term_to_atom(blank(Id), Blank),
	atom_concat('http://www.infraengine.com/blank#', Blank, Uri).
