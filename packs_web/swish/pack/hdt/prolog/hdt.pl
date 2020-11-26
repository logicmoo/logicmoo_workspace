/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(hdt,
	  [ hdt_open/2,			% -HDT, +Path
	    hdt_open/3,			% -HDT, +Path, +Options
	    hdt_close/1,		% +HDT
	    hdt_search/4,		% +HDT, ?S,?P,?O
	    hdt_header/4,		% +HDT, ?S,?P,?O

	    hdt_subject/2,		% +HDT, ?Subject
	    hdt_predicate/2,		% +HDT, ?Predicate
	    hdt_shared/2,		% +HDT, ?Shared
	    hdt_object/2,		% +HDT, ?Object
	    hdt_node/2,     % +HDT, ?Node

	    hdt_suggestions/5,		% +HDT, +Base, +Role, +MaxCount, -List
	    hdt_property/2,		% +HTD, -Property

	    hdt_subject_id/3,		% +HDT, ?Subject, ?Id
	    hdt_predicate_id/3,		% +HDT, ?Predicate, ?Id
	    hdt_object_id/3,		% +HDT, ?Object, ?Id
	    hdt_pre_triple/3,		% +HDT, ?StringTriple, -IdTriple
	    hdt_post_triple/3,		% +HDT, ?StringTriple, +IdTriple
	    hdt_search_id/4,		% +HDT, ?S,?P,?O
	    hdt_search_cost/5,		% +HDT, ?S,?P,?O, -Cost

	    hdt_create_from_file/3,	% +HDTFile, +RDFFile, +Options

	    op(110, xfx, @),		% must be above .
	    op(650, xfx, ^^)		% must be above :
	  ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml)).
:- use_module(library(lists)).

:- use_foreign_library(foreign(hdt4pl)).

/** <module> Access HDT (Header Dictionary Triples) files
*/

:- rdf_meta
	hdt_search(+,r,r,o),
	hdt_subject(+,r),
	hdt_predicate(+,r),
	hdt_shared(+,r),
	hdt_object(+,o),
	hdt_subject_id(+, r, ?),
	hdt_node(+, o),
	hdt_predicate_id(+, r, ?),
	hdt_object_id(+, o, ?),
	hdt_search_cost(+, r, r, o, -).

%%	hdt_open(-HDT, +File) is det.
%%	hdt_open(-HDT, +File, +Options) is det.
%
%	Open an existing HDT file and unify HDT with a handle to it. The
%	handle is an opaque symbol  that   is  subject to (atom) garbage
%	collection.  Options:
%
%	  - access(+Access)
%	  How the file is accessed. On of `map` (map the file
%	  into memory, default) or `load` (load the content of the
%	  file).
%	  - indexed(+Boolean)
%	  Whether an index is created. Default is `true`. Such an index
%	  is needed for partially instantiated calls to hdt_search/4.
%	  The index is maintained in a file with extension `.index`
%	  in the same directory as the HDT file.  An index is not needed
%	  if you only want to extract _all_ triples.

hdt_open(HDT, File) :-
	hdt_open(HDT, File, []).

%%	hdt_search(+HDT, ?S, ?P, ?O)
%
%	True if <S,P,O> is a triple in HDT.

hdt_search(HDT, S, P, O) :-
	pre_object(HDT, O, OHDT),
	hdt_search(HDT, content, S, P, OHDT),
	post_object(O, OHDT).

%%	hdt_header(+HDT, ?S, ?P, ?O)
%
%	True if <S,P,O> is a triple in the header of HDT.

hdt_header(HDT, S, P, O) :-
	hdt_search(HDT, header, S, P, O0),
	header_object(O0, O).

header_object(O0, O) :-
	string(O0), !,
	header_untyped_object(O0, O).
header_object(O, O).

header_untyped_object(O0, O) :-
	catch(xsd_number_string(N, O0),
	      error(syntax_error(xsd_number), _),
	      fail), !,
	(   integer(N)
	->  rdf_equal(O, N^^xsd:integer)
	;   rdf_equal(O, N^^xsd:float)
	).
header_untyped_object(O0, O) :-
	catch(xsd_time_string(Term, Type, O0),
	      error(_,_), fail), !,
	O = Term^^Type.
header_untyped_object(S, O) :-
	rdf_equal(O, S^^xsd:string).

%%	hdt_subject(+HDT, ?S) is nondet.
%%	hdt_predicate(+HDT, ?P) is nondet.
%%	hdt_object(+HDT, ?O) is nondet.
%%	hdt_shared(+HDT, ?SO) is nondet.
%%	hdt_node(+HDT, ?Node) is nondet.
%
%	Enumerate possible values for the   individual components of the
%	triples represented in the HDT. Note   that these enumarators do
%	not  enumerate  _blank  nodes_.    The   predicate  hdt_shared/2
%	enumerates resources that exist in the dataset both as _subject_
%	and  _object_.  If  the   second    argument   is   instantiated
%	hdt_search/4 is used to  perform  an   indexed  search  and  the
%	predicates are _semidet_.

hdt_subject(HDT, Subject) :-
	(   var(Subject)
	->  (   hdt_column_(HDT, shared, Var)
	    ;	hdt_column_(HDT, subject, Var)
	    ),
	    Var = Subject
	;   hdt_search(HDT, Subject, _, _)
	->  true
	).

hdt_predicate(HDT, Predicate) :-
	(   var(Predicate)
	->  hdt_column_(HDT, predicate, Var),
	    Var = Predicate
	;   hdt_search(HDT, _, Predicate, _)
	->  true
	).

hdt_shared(HDT, Shared) :-
	(   var(Shared)
	->  hdt_column_(HDT, shared, Var),
	    Var = Shared
	;   rdf_is_subject(Shared),
	    hdt_subject(HDT, Shared),
	    hdt_object(HDT, Shared)
	->  true
	).

hdt_object(HDT, Object) :-
	(   var(Object)
	->  (   hdt_column_(HDT, shared, Var),
	        Var = Object
	    ;	hdt_object_(HDT, OHDT),
		post_object(Object, OHDT)
	    )
	;   hdt_search(HDT, _, _, Object)
	->  true
	).

hdt_node(HDT, Node) :-
	(   var(Node)
	->  (   hdt_column_(HDT, shared, Var),
	        Var = Node
	    ;   hdt_column_(HDT, subject, Var),
	        Var = Node
	    ;   hdt_object_(HDT, OHDT),
	        post_object(Node, OHDT)
	    )
	;   hdt_search(HDT, Node, _, _)
	->  true
	;   hdt_search(HDT, _, _, Node)
	->  true
	).


%%	pre_object(+HDT, ?O, -OHDT) is det.
%%	post_object(?O, +OHDT) is det.
%
%	Pre/post object processing. The  HDT   library  itself is purely
%	string based.

pre_object(_HDT, O, OHDT) :-
	atom(O), \+ boolean(O), !,
	OHDT = O.
pre_object(_HDT, O, OHDT) :-
	ground(O), !,
	rdf_lexical_form(O, Lexical),
	canonical_string(Lexical, OHDT).
pre_object(HDT, O, OHDT) :-
	nonvar(O),
	O = String@Lang,
	ground(String),
	atomics_to_string(["\"", String, "\"@"], Prefix),
	hdt_suggestions(HDT, Prefix, object, 1000, List),
	length(List, Found),
	Found < 1000, !,		% we got them all
	member(_@Lang, List),
	canonical_string(String@Lang, OHDT).
pre_object(_, _, _).

canonical_string(Lexical^^Type, HDT) :-
	atomics_to_string(["\"", Lexical, "\"^^<", Type, ">"], HDT).
canonical_string(Lexical@Lang, HDT) :-
	atomics_to_string(["\"", Lexical, "\"@", Lang], HDT).

boolean(false).
boolean(true).

%!	post_object(?PrologObj, ?HDTObjectString) is semidet.

post_object(O, _HDT) :-
	ground(O), !.
post_object(O, IRI) :-
	atom(IRI), !,
	O = IRI.
post_object(O, HDT) :-
	rdf_canonical_literal(HDT, O).


%%	hdt_suggestions(+HDT, +Base, +Role, +MaxResults, -Results:list) is det.
%
%	True when Results is a  list  of   suggestions  for  Base in the
%	triple role Role. Some experimentation   suggests  it performs a
%	prefix match on the internal string representation. This implies
%	that literals are only found if the   first character of Base is
%	`"`.
%
%	@arg Base is a string or atom
%	@arg Role is one of `subject`, `predicate` or `object`


%%	hdt_property(+HDT, ?Property) is nondet.
%
%	True if Property is a property of HTD.  Defined properties are
%
%	  - mapping(-Mapping)
%	  - max_id(-ID))
%	  - max_object_id(-ID))
%	  - max_predicate_id(-ID))
%	  - max_subject_id(-ID))
%	  - objects(-Count))
%	  - predicates(-Count))
%	  - shared(-Count))
%	  - subjects(-Count))
%	  - elements(-Count))

hdt_property(HDT, Property) :-
	hdt_property(Property),
	hdt_property_(HDT, Property).

hdt_property(mapping(_)).
hdt_property(max_id(_)).
hdt_property(max_object_id(_)).
hdt_property(max_predicate_id(_)).
hdt_property(max_subject_id(_)).
hdt_property(objects(_)).
hdt_property(predicates(_)).
hdt_property(shared(_)).
hdt_property(subjects(_)).
hdt_property(elements(_)).


		 /*******************************
		 *	    IDENTIFIERS		*
		 *******************************/

%%	hdt_subject_id(+HDT,   ?Subject:atom,   ?Id:integer) is semidet.
%%	hdt_predicate_id(+HDT, ?Predicate:atom, ?Id:integer) is semidet.
%%	hdt_object_id(+HDT,    ?Object:any,     ?Id:integer) is semidet.
%
%	True if String is mapped to Id in   the given role. Fails if the
%	requested String or Id is not known for the given role in HDT.
%
%	@arg Role is one of `subject`, `predicate` or `object`

hdt_subject_id(HDT, String, Id) :-
	hdt_string_id(HDT, subject, String, Id).
hdt_predicate_id(HDT, String, Id) :-
	hdt_string_id(HDT, predicate, String, Id).
hdt_object_id(HDT, Object, Id) :-
	pre_object(HDT, Object, String),
	hdt_string_id(HDT, object, String, Id),
	post_object(Object, String).

%%	hdt_pre_triple(+HDT,  ?TripleIn, -TripleID) is det.
%%	hdt_post_triple(+HDT, ?TripleIn, +TripleID) is det.
%
%	Perform term->id and  id->term  translation   for  triples.  The
%	predicate hdt_search/4 could be defined as:
%
%	  ==
%	  hdt_search(HDT, S, P, O) :-
%	      Triple   = t(S,P,O),
%	      TripleID = t(SID,PID,OID),
%	      hdt_pre_triple(HDT, Triple, TripleID),
%	      hdt_search_id(HDT,SID,PID,OID),
%	      hdt_post_triple(HDT, Triple, TripleID).
%	  ==
%
%	@see hdt_search_id/4.

hdt_pre_triple(HDT, t(S0,P0,O0), t(S,P,O)) :-
	pre_iri_id(HDT, subject, S0, S),
	pre_iri_id(HDT, predicate, P0, P),
	(   ground(O0)
	->  pre_object(HDT, O0, String),
	    hdt_string_id(HDT, object, String, O)
	;   true
	).

hdt_post_triple(HDT, t(S0,P0,O0), t(S,P,O)) :-
	post_iri_id(HDT, subject, S0, S),
	post_iri_id(HDT, predicate, P0, P),
	(   ground(O0)
	->  true
	;   hdt_string_id(HDT, object, String, O),
	    post_object(O0, String)
	).

pre_iri_id(_, _, In, _) :-
	var(In), !.
pre_iri_id(HDT, Role, In, Id) :-
	hdt_string_id(HDT, Role, In, Id).

post_iri_id(_, _, S0, _) :-
	atom(S0), !.
post_iri_id(HDT, Role, In, Id) :-
	hdt_string_id(HDT, Role, In, Id).


%%	hdt_search_id(+HDT, ?S:integer, ?P:integer, ?O:integer) is nondet.
%
%	True if a triple with the indicated identifiers exists.

%%	hdt_search_cost(HDT, ?S, ?P, ?O, -Cost:nonneg) is det.

hdt_search_cost(HDT, S, P, O, Cost) :-
	Triple   = t(S,P,O),
	TripleID = t(SID,PID,OID),
	hdt_pre_triple(HDT, Triple, TripleID),
	hdt_search_cost_id(HDT, SID, PID, OID, Cost), !.
hdt_search_cost(_, _, _, _, 0).


		 /*******************************
		 *	       CREATE		*
		 *******************************/

%%	hdt_create_from_file(+HDTFile, +RDFFile, +Options)
%
%	Create a HDT  file  from  an  RDF   file.  RDFFile  must  be  in
%	`ntriples` format.  Options:
%
%	  * base_uri(+URI)
%	  URI is used for generating the header properties (see
%	  http_header/4.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(hdt_error(Message)) -->
	[ 'HDT: ~w'-[Message] ].
