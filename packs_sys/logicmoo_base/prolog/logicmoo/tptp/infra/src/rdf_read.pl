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
%%%% RDF Read
%%%% 
%%%% Parsing RDF.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(rdf_read, [read_rdf/2, 
                     read_rdf/3,
		     xml_to_rdf/2
		     
		     % prepare_xml_rdf/4,
		     % extract_xml_rdf/2,
		    ]).

:- use_module('swilib/err').

:- use_module(uris).
:- use_module(patched_rdf).
:- use_module(patched_rdf_parser).
:- use_module(patched_rdf_triple).
:- use_module(library(sgml)).

/*
SWI - an option to switch off parsing with DTD, (i.e. ignoring
external DOCTYPE in document) seems missing. So we get an (harmless)
SGML2PL error, if DTD file is not found. There is also a mixed case:
external DTD is not found, additional doctype specs in [] are within
the document. These inner (e.g. entity specs) should be considered,
the external ignored. Currently works this case seems works with
ENTITY declarations, but not with ATTLIST.
*/

%%%% 
%%%% read_rdf(+File, -Triples, [+Options])
%%%% 
%%%% Similar to load_rdf in library(rdf) except:
%%%% 
%%%%   PATCHED BUGS OR OBSOLETE BEHAVIOR:
%%%% 
%%%%   - XML Namespace and XML Base declaration can appear as attributes
%%%%     of RDF tags. The current version of the SWI RDF parser in some
%%%%     cases makes properties out of them. This is prevented by
%%%%     a preprocessing step.
%%%% 
%%%%   - Empty properties should now be handled correctly, i.e. 
%%%%     lead to an empty literal value. This is implemented by
%%%%     a patch in rdf_parser.
%%%% 
%%%%   - xml:base declarations are used to resolve the attributes:
%%%%     rdf:about, rdf:resource, rdf:type, rdf:ID, rdf:bagID
%%%% 
%%%%   EXTENSIONS:  
%%%% 
%%%%   - Triples are collected not only from a toplevel rdf document, but also
%%%%     from all rdf elements embedded into a document of arbitray type.
%%%% 
%%%%   - Option namespaces(Namespaces) returns a list of 
%%%%     Namespace-Abbreviation pairs of namespace declarations found.
%%%%
%%%%   ALTERATIONS:
%%%% 
%%%%   - Skolem constants are terms blank(Atom), where Atom is a 
%%%%     generated atom. This is implemented as a patch to rdf_triple.
%%%%   
%%%%   - The base uri is by default '', and should be handled
%%%%     according to RFC 2396. Specifying '[]' is equivalent to
%%%%     this. All base uri handling is done in preprocessing steps, '[]'
%%%%     is always given to xml_to_rdf/3 in library(rdf).
%%%% 

read_rdf(File, Triples) :-
	read_rdf(File, Triples, []).

read_rdf(File, Triples, Options) :-
	option(base_uri(BaseURI), Options, []),

	load_structure(File,
	               Contents,
		       [ dialect(xmlns),
			 space(sgml)
		       ]),
        ( BaseURI = [] ->
	  Base = ''
	; Base = BaseURI
	),

	prepare_xml_rdf(Contents, Base, Options, Contents1),
	extract_xml_rdf(Contents1, RDFElements),
	map_append_xml_to_rdf(RDFElements, Triples).


%%%% 
%%%% Prepare the xml structure returned by the parser for further processing,
%%%% i.e. before calling xml_to_rdf.
%%%%
prepare_xml_rdf(Contents, Base, Options, Contents1) :-	
	%% We always use [] as BaseUri for the swi predicates, since
	%% we have the base already processed and an explicit base uri
	%% would lead to errors.
        process_xml_base(Contents, Base, Contents2),
	( memberchk(namespaces(Namespaces), Options) ->
	  extract_namespaces(Contents2, Namespaces)
        ; true
	),
	remove_meta_attributes(Contents2, Contents1).

map_append_xml_to_rdf([RDF], Triples) :-
	xml_to_rdf(RDF, Triples),
	!.
map_append_xml_to_rdf([RDF|RDFs], Triples) :-
	xml_to_rdf(RDF, Triples1),
	append(Triples1, Triples2, Triples),
	map_append_xml_to_rdf(RDFs, Triples2).
map_append_xml_to_rdf([], []).

xml_to_rdf(X, Y) :-
	xml_to_rdf(X, [], Y).

%%%% 
%%%% Return list of rdf:RDF elements which are in Contents (at any depth).
%%%% 
extract_xml_rdf(Contents, RDFElements) :-
	extract_rdf(Contents, [], RDFElements).

extract_rdf([element(RDF:'RDF', A, C)|Cs],
            RDFs, 
            [element(RDF:'RDF', A, C)|RDFs1]) :-
	rdf_name_space(RDF),
	!,
	extract_rdf(Cs, RDFs, RDFs1).
extract_rdf([element(_, _, C)|Cs], RDFs, RDFs1) :-
	!,
	extract_rdf(C, RDFs2, RDFs1),
	extract_rdf(Cs, RDFs, RDFs2).
extract_rdf(_, RDFs, RDFs).
	
option(Opt, Options) :-
	memberchk(Opt, Options), !.
option(Opt, Options) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	memberchk(OptName=OptVal, Options), !.

option(Opt, Options, _) :-
	option(Opt, Options), !.
option(Opt, _, Default) :-
	arg(1, Opt, Default).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Extract Namespaces
%%%% 
%%%% Extract all declarations of namespace abbreviations from the document.
%%%% Since an abbreviation can be assigned different namespaces in different
%%%% parts of a document, the result has no "semantic" significance, but is
%%%% useful as hint for pretty printing. They result is a list of
%%%% Namespace-Abbreviation pairs.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_namespaces(Cs, Ns) :-
	ens_1(Cs, [], Ns).

ens_1([C|Cs], Ns, Ns1) :-
	ens_2(C, Ns, Ns2),
	ens_1(Cs, Ns2, Ns1).
ens_1([], Ns, Ns).

ens_2(element(_, Atts, Cs), Ns, Ns1) :-
	!,
	ens_3(Atts, Ns, Ns2),
	ens_1(Cs, Ns2, Ns1).
ens_2(_, Ns, Ns).

ens_3([xmlns:A=N|Atts], Ns, [N-A| Ns1]) :-
	!,
	ens_3(Atts, Ns, Ns1).
ens_3([_|Atts], Ns, Ns1) :-
	ens_3(Atts, Ns, Ns1).
ens_3([], Ns, Ns).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Remove XML Meta Attributes
%%%% 
%%%% Certain attributes should not be converted to rdf properties. 
%%%% This is here as extra preprocessing step, but should be perhaps be
%%%% included  into the rdf_parser.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_meta_attributes([C|Cs], [C1|Cs1]) :-
	rma_1(C, C1),
	remove_meta_attributes(Cs, Cs1).
remove_meta_attributes([], []).

rma_1(element(T, A, C), element(T, A1, C1)) :-
	!,
	rma_2(A, T, A1),
	remove_meta_attributes(C, C1).
rma_1(C, C).

rma_2([A=_|AVs], T, AVs1) :-
	meta_attribute_to_remove(A),
	!,
	rma_2(AVs, T, AVs1).
rma_2([AV|AVs], T, [AV|AVs1]) :-
	rma_2(AVs, T, AVs1).
rma_2([], _, []).

meta_attribute_to_remove(xmlns:_).
meta_attribute_to_remove(xmlns).
meta_attribute_to_remove(xml:base).
meta_attribute_to_remove(xml:lang).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Process XML Base
%%%%
%%%% Walk through the xml tree and apply base uris declared using the
%%%% xml:base attribute or supplied as top level parameter.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_xml_base([X|Xs], Y1, [X1|Xs1]) :-
	apply_base_to_content(X, Y1, X1),
	process_xml_base(Xs, Y1, Xs1).
process_xml_base([], _, []).

resource_attribute(RDF:about) :- rdf_name_space(RDF).
resource_attribute(RDF:resource) :- rdf_name_space(RDF).
resource_attribute(RDF:type) :- rdf_name_space(RDF).

id_attribute(RDF:'ID') :- rdf_name_space(RDF).
id_attribute(RDF:bagID) :- rdf_name_space(RDF).

resolve_resource(Base, UriRef, Uri) :-
	resolve_uri(Base, UriRef, Uri).

resolve_id(Base, UriRef, Uri) :-
	atom_concat('#', UriRef, Fragment),
	resolve_uri(Base, Fragment, Uri).

apply_base_to_content( element(T, A, C), Base, element(T1, A1, C1) ) :-
	!,
	( memberchk( xml:base=Base1, A ) ->
	  resolve_resource(Base, Base1, Base2)
        ; Base2 = Base
	),
	apply_base_to_tag(T, Base2, T1),
	map_apply_base_to_attribute(A, Base2, A1),
	process_xml_base(C, Base2, C1).
apply_base_to_content( C, _, C ).

%%
%% hmmm - is this justified? It works with the slinks.rdf example.
%%
apply_base_to_tag(N:L, _, N:L) :-
	!.
apply_base_to_tag(L, B, B1:L) :-
	atom_concat(B, '#', B1).
	
apply_base_to_attribute(A=V, Base, A=V1) :-
	resource_attribute(A),
	!,
	resolve_resource(Base, V, V1).
apply_base_to_attribute(A=V, Base, A=V1) :-
	id_attribute(A),
	!,
	resolve_id(Base, V, V1).
apply_base_to_attribute(AV, _, AV).

map_apply_base_to_attribute([X|Xs], Y1, [X1|Xs1]) :-
	apply_base_to_attribute(X, Y1, X1),
	map_apply_base_to_attribute(Xs, Y1, Xs1).
map_apply_base_to_attribute([], _, []).

