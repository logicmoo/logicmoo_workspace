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
%%%% RDF Writer
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(rdf_writer, [triples_to_xml/3,
		       namespace_break/3,
		       aux_check_blanks/1,
		       aux_group/3,
		       err_serializability/2]).

:- use_module('swilib/err').
:- use_module('rdf_convert').

% 
% test_rdf(F) :-
% 	get_document(F, rdf, [namespaces(N)], X1),
% 	triples_to_xml(X1, N, Cs),
% 	write_structure(Cs, [encoding('UTF-8'), indent(4)]).
%

/* 

Hmmm - inlining of descriptions seems to be the only means of the
XML RDF serialization to express blank nodes. So graphs which have the
same blank node as object in more than one triple, or graphs with
cyles involving blank nodes can not be serialized. The old RDF Model
and Syntax spec seems to suggests that Ids are used to express shared
blank nodes. These are however resolved to URIs and not to blank nodes.
So, do we need another attribute "blankId"?

The current version of RDF/XMLSyntax Specification (25 March 2002)
flatly contradicts, when it says "applications MUST be able to
distinguish URI references from blank nodes" (3.5) and mentions as
first step in the basic approach to serializing RDF that "all blank
nodes are assigned arbitrary URIs" (6).

*/


triples_to_xml(Triples, Contents) :-
	triples_to_xml(Triples, [], Contents).

triples_to_xml(Triples, NAs, Contents) :-
	Contents = [element(rdf:'RDF', Xmlns, Descs)],
	aux_check_blanks(Triples),
	compute_namespaces(Triples, Triples1, NAs, NAs1),
	aux_group(Triples1, ResDescs, BlankDescs),
        map_desce(ResDescs, BlankDescs, BlankDescs1, Es),
	map_blank_desce(BlankDescs1, Es, Descs),
	map_xmlns(NAs1, Xmlns).
	
aux_group(Triples, ResDescs, BlankDescs) :-
	setof(S-POs, setof(P-O, member( rdf(S, P, O),  Triples ), POs),
                     Descs1),
	!,
        select_blanks(Descs1, BlankDescs1, ResDescs),
	blank_objects(Triples, BlankObjects),
	add_empty_blanks(BlankObjects, BlankDescs1, BlankDescs).
aux_group(_, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonic_item(I, I1) :-
	atom(I),
	pseudo_namespace(N, N1, _),
	sub_atom(I, 0, B, L, N),
	L > 0,
	!,
	sub_atom(I, B, L, 0, I2),
	atom_concat(N1, I2, I1). 
canonic_item(I, I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


map_xmlns([X|Xs], [X1|Xs1]) :-
    xmlns(X, X1),
    map_xmlns(Xs, Xs1).
map_xmlns([], []).

xmlns(N-'', xmlns=N) :-
	!.
xmlns(N-A, xmlns:A=N). 

map_blank_desce([D|Ds], Es, [E|Es1]) :-
	desce(D, Ds, Ds1, E),
	map_blank_desce(Ds1, Es, Es1).
map_blank_desce([], Es, Es).

map_desce([D|Ds], Bs, Bs1, [E|Es]) :-
	desce(D, Bs, Bs2, E),
	map_desce(Ds, Bs2, Bs1, Es).
map_desce([], Bs, Bs, []).

map_poe([PO|POs], Bs, Bs1, [E|Es]) :-
	poe(PO, Bs, Bs2, E),
	map_poe(POs, Bs2, Bs1, Es).
map_poe([], Bs, Bs, []).


desce(S-POs, Bs, Bs1, E) :-
	( select(('http://www.w3.org/1999/02/22-rdf-syntax-ns#':type)-Type,
	         POs, POs1) ->
          E = element(Type, Atts, POEs)
	; E = element(rdf:'Description', Atts, POEs),
	  POs1 = POs
	),
	( S = blank(_) ->
          Atts = []
        ; Atts = [rdf:about=S]
	),
	map_poe(POs1, Bs, Bs1, POEs).

poe(P-O, Bs, Bs1, E) :-
	E = element(P, Atts, Vals),
	( O = literal(Lit) ->
	  ( atom(Lit) ->
	    Atts = [],
	    Vals = [Lit]
	  ; Lit = [element(_, _, _)|_] ->
	    Atts = [rdf:parseType='Literal'],
	    Vals = Lit  
	  ; term_to_atom(Lit, Lit1),
	    Atts = [],
	    Vals = [Lit1]
	  ),  
	  Bs1 = Bs
        ; O = blank(_) ->
          Atts = [],
          Vals = [DescE],
	  select_blank(O, Bs, Bs2, D),
	  desce(D, Bs2, Bs1, DescE)
        ; ( O = N:O1 -> atom_concat(N, O1, O2) ; O2 = O ), %% type values
          Atts = [rdf:resource=O2],
	  Vals = [],
	  Bs1 = Bs
	).

select_blank(B, BDs, BDs1, B-POs) :-
	select(B-POs, BDs, BDs1),
	!.
select_blank(B, _, _, _) :-
	err_serializability(
	    'Not serializable since ~q violates a blank constraint.', [B]).

add_empty_blanks([B|Bs], Ds, Ds1) :-
	memberchk(B-_, Ds),
	!,
	add_empty_blanks(Bs, Ds, Ds1).
add_empty_blanks([B|Bs], Ds, [B-[]|Ds1]) :-
	add_empty_blanks(Bs, Ds, Ds1).
add_empty_blanks([], Ds, Ds).

select_blanks([blank(Id)-POs|Ds], [blank(Id)-POs|Ds1], Ds2) :-
	!,
	select_blanks(Ds, Ds1, Ds2).
select_blanks([D|Ds], Ds1, [D|Ds2]) :-
	select_blanks(Ds, Ds1, Ds2).
select_blanks([], [], []).

blank_objects( Triples, Blanks ) :-
	setof(blank(Id), S^P^member(rdf(S,P,blank(Id)), Triples), Blanks),
	!.
blank_objects( _, [] ).

compute_namespaces(Triples, Triples1, NAs, NAUseds) :-
	remove_pseudo_namespaces(NAs, NAs1),
	NAs2 = ['http://www.w3.org/1999/02/22-rdf-syntax-ns#'-rdf,
	        'http://www.w3.org/2000/01/rdf-schema#'-rdfs | NAs1 ],
	sort(NAs2, NAs3),
	reverse(NAs3, NAs4),
	cns_1(Triples, Triples1, NAs4, _, [], NAUseds).

remove_pseudo_namespaces([N-A|NAs], NAs1) :-
	pseudo_namespace(N, _, A),
	!,
	remove_pseudo_namespaces(NAs, NAs1).
remove_pseudo_namespaces([NA|NAs], [NA|NAs1]) :-
	remove_pseudo_namespaces(NAs, NAs1).
remove_pseudo_namespaces([], []).

cns_1([rdf(S, P, O)|Ts], [rdf(S1, P1, O1)|Ts1], NAs, NAs1, NAUs, NAUs1) :-
	canonic_item(S, S1),
	canonic_item(P, P2),
	canonic_item(O, O2),
	find_ns(P2, P1, NAs, NAs2, NAUs, NAUs2),
	( P2 = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' ->
	  find_ns(O2, O3, NAs2, NAs3, NAUs2, NAUs3)
	; O3 = O2, NAs3 = NAs2, NAUs3 = NAUs2
	),
	( O3 = literal(XML), XML = [element(_, _, _)|_] ->
	  cns_xml(XML, XML1, NAs3, NAs4, NAUs3, NAUs4),
	  O1 = literal(XML1)
	; NAs4 = NAs3, NAUs4 = NAUs3, O1 = O3
	),
	cns_1(Ts, Ts1, NAs4, NAs1, NAUs4, NAUs1).
cns_1([], [], NAs, NAs, NAUs, NAUs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% For XML literals
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cns_xml([element(E, A, C)|Es], [element(E1, A1, C1)|Es1],
	NAs, NAs1, NAUs, NAUs1) :-
	!,
	canonic_item(E, E2),
	find_ns(E2, E1, NAs, NAs2, NAUs, NAUs2),
	map_cns_xml_attrib(A, A1, NAs2, NAs3, NAUs2, NAUs3),
	cns_xml(C, C1, NAs3, NAs4, NAUs3, NAUs4),
	cns_xml(Es, Es1, NAs4, NAs1, NAUs4, NAUs1).
cns_xml([E|Es], [E|Es1], NAs, NAs1, NAUs, NAUs1) :-
	cns_xml(Es, Es1, NAs, NAs1, NAUs, NAUs1).
cns_xml([], [], NAs, NAs, NAUs, NAUs).

map_cns_xml_attrib([A=V|AVs], [A1=V|AVs1], NAs, NAs1, NAUs, NAUs1) :-
	canonic_item(A, A2),
	find_ns(A2, A1, NAs, NAs2, NAUs, NAUs2),
	map_cns_xml_attrib(AVs, AVs1, NAs2, NAs1, NAUs2, NAUs1).
map_cns_xml_attrib([], [], NAs, NAs, NAUs, NAUs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% *** rdf..., sys

find_ns(NP, N:P, NAs, NAs1, NAUs, NAUs1) :-
	!,
	( NP = N:P ->
	  true
        ; split_np(NP, NAs, N, P)
	),
	( memberchk(N-A, NAs) ->
	  NAs1 = NAs
        ; once(( find_name(n, A), \+ memberchk(_-A, NAs) )),
	  revord_add_element(NAs, N-A, NAs1)
	),
	( memberchk(N-A, NAUs) -> NAUs1 = NAUs ; NAUs1 = [N-A|NAUs] ).

split_np(NP, NAs, N, P) :-
	( member(N-_, NAs),
	  sub_atom(NP, 0, B, L, N),
	  L > 0  ->
          sub_atom(NP, B, L, 0, P)
        ; namespace_break(NP, N, P) ->
	  true
	; err_serializability('Namespace breaking failed for ~q.', [NP])
	).

namespace_break(NP, N, P) :-
	atom(NP),
	atom_codes(NP, Cs),
	reverse(Cs, Cs1),
	Cs1 = [C1|_],
	xml_name_code(C1),
	split_list(Cs1, Cs2, [C3|Cs3]),
	\+ xml_name_code(C3),
	!,
	Cs3 \= [],
	reverse([C3|Cs3], CsN),
	reverse(Cs2, CsP),
	atom_codes(P, CsP),
	atom_codes(N, CsN).
	  
%%
%% This might be incomplete.
%%
xml_name_code(C) :-
	code_type(C, alnum),
	!.
xml_name_code(0'.).
xml_name_code(0'-).
xml_name_code(0'_).	

aux_check_blanks(Triples) :-
	map_add_key_object(Triples, Triples1),
	sort(Triples1, Triples2),
	cb_1(Triples2).

cb_1([blank(Id)-_, blank(Id)-_|_]) :-
	!,
	err_serializability(
	    'Not serializable: Blank node ~q appears multiply as value.',
            [blank(Id)]).
cb_1([_|Xs]) :-
	cb_1(Xs).
cb_1([]).

map_add_key_object([rdf(S,P,O)|Xs], [O-rdf(S,P,O)|Xs1]) :-
    map_add_key_object(Xs, Xs1).
map_add_key_object([], []).

find_name(Name, Name).
find_name(Name, Name1) :-
	gen_new_name_n(Name, 1, Name1).

gen_new_name_n(Name, N, Name1) :-
	concat_atom([Name, N], Name1).
gen_new_name_n(Name, N, Name1) :-
	N1 is N + 1,
	gen_new_name_n(Name, N1, Name1).

revord_add_element([], El, [El]). 
revord_add_element([H|T], El, Add) :-
    compare(Order, H, El),
    rev_addel(Order, H, T, El, Add).

rev_addel(>, H, T,  El, [H|Add]) :-
    revord_add_element(T, El, Add).
rev_addel(=, H, T, _El, [H|T]). 
rev_addel(<, H, T,  El, [El,H|T]).

split_list(Xs, [], Xs).
split_list([X|Xs], [X|Xs1], Xs2) :-
	split_list(Xs, Xs1, Xs2).

err_serializability(Fmt, Args) :-
	throw(err_serializability(Fmt, Args)).
%%
%% TODO: default handler that converts this into an error exception
%%
