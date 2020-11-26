/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(rdf_db,
          [ rdf_version/1,              % -Version

            rdf/3,                      % ?Subject, ?Predicate, ?Object
            rdf/4,                      % ?Subject, ?Predicate, ?Object, ?DB
            rdf_has/3,                  % ?Subject, +Pred, ?Obj
            rdf_has/4,                  % ?Subject, +Pred, ?Obj, -RealPred
            rdf_reachable/3,            % ?Subject, +Pred, ?Object
            rdf_reachable/5,            % ?Subject, +Pred, ?Object, +MaxD, ?D
            rdf_resource/1,             % ?Resource
            rdf_subject/1,              % ?Subject

            rdf_member_property/2,      % ?Property, ?Index

            rdf_assert/3,               % +Subject, +Predicate, +Object
            rdf_assert/4,               % +Subject, +Predicate, +Object, +DB
            rdf_retractall/3,           % ?Subject, ?Predicate, ?Object
            rdf_retractall/4,           % ?Subject, ?Predicate, ?Object, +DB
            rdf_update/4,               % +Subject, +Predicate, +Object, +Act
            rdf_update/5,               % +Subject, +Predicate, +Object, +Src, +Act
            rdf_set_predicate/2,        % +Predicate, +Property
            rdf_predicate_property/2,   % +Predicate, ?Property
            rdf_current_predicate/1,    % -Predicate
            rdf_current_literal/1,      % -Literal
            rdf_transaction/1,          % :Goal
            rdf_transaction/2,          % :Goal, +Id
            rdf_transaction/3,          % :Goal, +Id, +Options
            rdf_active_transaction/1,   % ?Id

            rdf_monitor/2,              % :Goal, +Options

            rdf_save_db/1,              % +File
            rdf_save_db/2,              % +File, +DB
            rdf_load_db/1,              % +File
            rdf_reset_db/0,

            rdf_node/1,                 % -Id
            rdf_bnode/1,                % -Id
            rdf_is_bnode/1,             % +Id

            rdf_is_resource/1,          % +Term
            rdf_is_literal/1,           % +Term
            rdf_literal_value/2,        % +Term, -Value

            rdf_load/1,                 % +File
            rdf_load/2,                 % +File, +Options
            rdf_save/1,                 % +File
            rdf_save/2,                 % +File, +Options
            rdf_unload/1,               % +File
            rdf_unload_graph/1,         % +Graph

            rdf_md5/2,                  % +DB, -MD5
            rdf_atom_md5/3,             % +Text, +Times, -MD5

            rdf_create_graph/1,         % ?Graph
            rdf_graph_property/2,       % ?Graph, ?Property
            rdf_set_graph/2,            % +Graph, +Property
            rdf_graph/1,                % ?Graph
            rdf_source/1,               % ?File
            rdf_source/2,               % ?DB, ?SourceURL
            rdf_make/0,                 % Reload modified databases
            rdf_gc/0,                   % Garbage collection

            rdf_source_location/2,      % +Subject, -Source
            rdf_statistics/1,           % -Key
            rdf_set/1,                  % +Term
            rdf_generation/1,           % -Generation
            rdf_snapshot/1,             % -Snapshot
            rdf_delete_snapshot/1,      % +Snapshot
            rdf_current_snapshot/1,     % +Snapshot
            rdf_estimate_complexity/4,  % +S,+P,+O,-Count

            rdf_save_subject/3,         % +Stream, +Subject, +DB
            rdf_save_header/2,          % +Out, +Options
            rdf_save_footer/1,          % +Out

            rdf_equal/2,                % ?Resource, ?Resource
            lang_equal/2,               % +Lang1, +Lang2
            lang_matches/2,             % +Lang, +Pattern

            rdf_prefix/2,               % :Alias, +URI
            rdf_current_prefix/2,       % :Alias, ?URI
            rdf_register_prefix/2,      % +Alias, +URI
            rdf_register_prefix/3,      % +Alias, +URI, +Options
            rdf_unregister_prefix/1,    % +Alias
            rdf_current_ns/2,           % :Alias, ?URI
            rdf_register_ns/2,          % +Alias, +URI
            rdf_register_ns/3,          % +Alias, +URI, +Options
            rdf_global_id/2,            % ?NS:Name, :Global
            rdf_global_object/2,        % +Object, :NSExpandedObject
            rdf_global_term/2,          % +Term, :WithExpandedNS

            rdf_compare/3,              % -Dif, +Object1, +Object2
            rdf_match_label/3,          % +How, +String, +Label
            rdf_split_url/3,            % ?Base, ?Local, ?URL
            rdf_url_namespace/2,        % +URL, ?Base

            rdf_warm_indexes/0,
            rdf_warm_indexes/1,         % +Indexed
            rdf_update_duplicates/0,

            rdf_debug/1,                % Set verbosity

            rdf_new_literal_map/1,      % -Handle
            rdf_destroy_literal_map/1,  % +Handle
            rdf_reset_literal_map/1,    % +Handle
            rdf_insert_literal_map/3,   % +Handle, +Key, +Literal
            rdf_insert_literal_map/4,   % +Handle, +Key, +Literal, -NewKeys
            rdf_delete_literal_map/3,   % +Handle, +Key, +Literal
            rdf_delete_literal_map/2,   % +Handle, +Key
            rdf_find_literal_map/3,     % +Handle, +KeyList, -Literals
            rdf_keys_in_literal_map/3,  % +Handle, +Spec, -Keys
            rdf_statistics_literal_map/2, % +Handle, +Name(-Arg...)

            rdf_graph_prefixes/2,       % ?Graph, -Prefixes
            rdf_graph_prefixes/3,       % ?Graph, -Prefixes, :Filter

            (rdf_meta)/1,               % +Heads
            op(1150, fx, (rdf_meta))
          ]).
:- use_module(library(semweb/rdf_prefixes),
              [ (rdf_meta)/1,
                register_file_prefixes/1,
                rdf_global_id/2,
                rdf_register_ns/2,
                                        % re-exported predicates
                rdf_global_object/2,
                rdf_current_ns/2,
                rdf_prefix/2,
                rdf_global_term/2,
                rdf_register_ns/3,
                rdf_register_prefix/3,
                rdf_register_prefix/2,
                rdf_current_prefix/2,
                rdf_unregister_prefix/1
              ]).

:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(debug),[debug/3,assertion/1]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(gensym),[gensym/2,reset_gensym/1]).
:- autoload(library(lists),
	    [member/2,flatten/2,list_to_set/2,append/3,select/3]).
:- autoload(library(memfile),
	    [atom_to_memory_file/2,open_memory_file/4]).
:- autoload(library(option),
	    [option/2,option/3,merge_options/3,meta_options/3]).
:- autoload(library(rdf),[process_rdf/3]).
:- autoload(library(sgml),
	    [ load_structure/3,
	      xml_quote_attribute/3,
	      xml_name/1,
	      xml_quote_cdata/3,
	      xml_is_dom/1,
	      iri_xml_namespace/3,
	      iri_xml_namespace/2
	    ]).
:- autoload(library(sgml_write),[xml_write/3]).
:- autoload(library(uri),
	    [ uri_file_name/2,
	      uri_is_global/1,
	      uri_normalized/2,
	      uri_components/2,
	      uri_data/3,
	      uri_data/4
	    ]).
:- autoload(library(xsdp_types),[xsdp_numeric_uri/2]).
:- autoload(library(semweb/rdf_cache),[rdf_cache_file/3]).

:- if(exists_source(library(thread))).
:- autoload(library(thread), [concurrent/3]).
:- endif.

:- use_foreign_library(foreign(rdf_db)).
:- public rdf_print_predicate_cloud/2.  % print matrix of reachable predicates

:- meta_predicate
    rdf_transaction(0),
    rdf_transaction(0, +),
    rdf_transaction(0, +, +),
    rdf_monitor(1, +),
    rdf_save(+, :),
    rdf_load(+, :).

:- predicate_options(rdf_graph_prefixes/3, 3,
                     [expand(callable), filter(callable), min_count(nonneg)]).
:- predicate_options(rdf_load/2, 2,
                     [ base_uri(atom),
                       blank_nodes(oneof([share,noshare])),
                       cache(boolean),
                       concurrent(positive_integer),
                       db(atom),
                       format(oneof([xml,triples,turtle,trig,nquads,ntriples])),
                       graph(atom),
                       multifile(boolean),
                       if(oneof([true,changed,not_loaded])),
                       modified(-float),
                       prefixes(-list),
                       silent(boolean),
                       register_namespaces(boolean)
                     ]).
:- predicate_options(rdf_save/2, 2,
                     [ graph(atom),
                       db(atom),
                       anon(boolean),
                       base_uri(atom),
                       write_xml_base(boolean),
                       convert_typed_literal(callable),
                       encoding(encoding),
                       document_language(atom),
                       namespaces(list(atom)),
                       xml_attributes(boolean),
                       inline(boolean)
                     ]).
:- predicate_options(rdf_save_header/2, 2,
                     [ graph(atom),
                       db(atom),
                       namespaces(list(atom))
                     ]).
:- predicate_options(rdf_save_subject/3, 3,
                     [ graph(atom),
                       base_uri(atom),
                       convert_typed_literal(callable),
                       document_language(atom)
                     ]).
:- predicate_options(rdf_transaction/3, 3,
                     [ snapshot(any)
                     ]).

:- discontiguous
    term_expansion/2.

/** <module> Core RDF database

The file library(semweb/rdf_db) provides the core  of the SWI-Prolog RDF
store.

@deprecated     New applications should use library(semweb/rdf11), which
                provides a much more intuitive API to the RDF store, notably
                for handling literals.  The library(semweb/rdf11) runs
                currently on top of this library and both can run side-by-side
                in the same application.  Terms retrieved from the database
                however have a different shape and can not be exchanged without
                precautions.
*/

		 /*******************************
		 *            PREFIXES		*
		 *******************************/

% the ns/2 predicate is historically defined  in this module. We'll keep
% that for compatibility reasons.

:- multifile ns/2.
:- dynamic   ns/2.                      % ID, URL

:- multifile
    rdf_prefixes:rdf_empty_prefix_cache/2.

rdf_prefixes:rdf_empty_prefix_cache(_Prefix, _IRI) :-
    rdf_empty_prefix_cache.

:- rdf_meta
    rdf(r,r,o),
    rdf_has(r,r,o,r),
    rdf_has(r,r,o),
    rdf_assert(r,r,o),
    rdf_retractall(r,r,o),
    rdf(r,r,o,?),
    rdf_assert(r,r,o,+),
    rdf_retractall(r,r,o,?),
    rdf_reachable(r,r,o),
    rdf_reachable(r,r,o,+,?),
    rdf_update(r,r,o,t),
    rdf_update(r,r,o,+,t),
    rdf_equal(o,o),
    rdf_source_location(r,-),
    rdf_resource(r),
    rdf_subject(r),
    rdf_create_graph(r),
    rdf_graph(r),
    rdf_graph_property(r,?),
    rdf_set_graph(r,+),
    rdf_unload_graph(r),
    rdf_set_predicate(r, t),
    rdf_predicate_property(r, -),
    rdf_estimate_complexity(r,r,r,-),
    rdf_print_predicate_cloud(r,+).

%!  rdf_equal(?Resource1, ?Resource2)
%
%   Simple equality test to exploit goal-expansion.

rdf_equal(Resource, Resource).

%!  lang_equal(+Lang1, +Lang2) is semidet.
%
%   True if two RFC language specifiers denote the same language
%
%   @see lang_matches/2.

lang_equal(Lang, Lang) :- !.
lang_equal(Lang1, Lang2) :-
    downcase_atom(Lang1, LangCannon),
    downcase_atom(Lang2, LangCannon).

%!  lang_matches(+Lang, +Pattern) is semidet.
%
%   True if Lang  matches  Pattern.   This  implements  XML language
%   matching  conform  RFC  4647.   Both    Lang   and  Pattern  are
%   dash-separated strings of  identifiers  or   (for  Pattern)  the
%   wildcard *. Identifiers are  matched   case-insensitive  and a *
%   matches any number of identifiers. A   short pattern is the same
%   as *.


                 /*******************************
                 *     BASIC TRIPLE QUERIES     *
                 *******************************/

%!  rdf(?Subject, ?Predicate, ?Object) is nondet.
%
%   Elementary query for triples. Subject   and  Predicate are atoms
%   representing the fully qualified URL of  the resource. Object is
%   either an atom representing a resource  or literal(Value) if the
%   object  is  a  literal  value.   If    a   value   of  the  form
%   NameSpaceID:LocalName is provided it  is   expanded  to a ground
%   atom  using  expand_goal/2.  This  implies   you  can  use  this
%   construct in compiled code without paying a performance penalty.
%   Literal values take one of the following forms:
%
%     * Atom
%     If the value is a simple atom it is the textual representation
%     of a string literal without explicit type or language
%     qualifier.
%
%     * lang(LangID, Atom)
%     Atom represents the text of a string literal qualified with
%     the given language.
%
%     * type(TypeID, Value)
%     Used for attributes qualified using the =|rdf:datatype|=
%     TypeID. The Value is either the textual representation or a
%     natural Prolog representation. See the option
%     convert_typed_literal(:Convertor) of the parser. The storage
%     layer provides efficient handling of atoms, integers (64-bit)
%     and floats (native C-doubles). All other data is represented
%     as a Prolog record.
%
%   For literal querying purposes, Object can be of the form
%   literal(+Query, -Value), where Query is one of the terms below.
%   If the Query takes a literal argument and the value has a
%   numeric type numerical comparison is performed.
%
%     * plain(+Text)
%     Perform exact match and demand the language or type qualifiers
%     to match. This query is fully indexed.
%
%     * icase(+Text)
%     Perform a full but case-insensitive match. This query is
%     fully indexed.
%
%     * exact(+Text)
%     Same as icase(Text).  Backward compatibility.
%
%     * substring(+Text)
%     Match any literal that contains Text as a case-insensitive
%     substring. The query is not indexed on Object.
%
%     * word(+Text)
%     Match any literal that contains Text delimited by a non
%     alpha-numeric character, the start or end of the string. The
%     query is not indexed on Object.
%
%     * prefix(+Text)
%     Match any literal that starts with Text. This call is intended
%     for completion. The query is indexed using the skip list of
%     literals.
%
%     * ge(+Literal)
%     Match any literal that is equal or larger than Literal in the
%     ordered set of literals.
%
%     * gt(+Literal)
%     Match any literal that is larger than Literal in the ordered set
%     of literals.
%
%     * eq(+Literal)
%     Match any literal that is equal to Literal in the ordered set
%     of literals.
%
%     * le(+Literal)
%     Match any literal that is equal or smaller than Literal in the
%     ordered set of literals.
%
%     * lt(+Literal)
%     Match any literal that is smaller than Literal in the ordered set
%     of literals.
%
%     * between(+Literal1, +Literal2)
%     Match any literal that is between Literal1 and Literal2 in the
%     ordered set of literals. This may include both Literal1 and
%     Literal2.
%
%     * like(+Pattern)
%     Match any literal that matches Pattern case insensitively,
%     where the `*' character in Pattern matches zero or more
%     characters.
%
%   Backtracking never returns duplicate triples.  Duplicates can be
%   retrieved using rdf/4. The predicate   rdf/3 raises a type-error
%   if called with improper arguments.  If   rdf/3  is called with a
%   term  literal(_)  as  Subject  or   Predicate  object  it  fails
%   silently.  This  allows   for   graph    matching   goals   like
%   rdf(S,P,O),rdf(O,P2,O2) to proceed without errors.

%!  rdf(?Subject, ?Predicate, ?Object, ?Source) is nondet.
%
%   As rdf/3 but in addition query  the   graph  to which the triple
%   belongs. Unlike rdf/3, this predicate does not remove duplicates
%   from the result set.
%
%   @param Source is a term Graph:Line.  If Source is instatiated,
%   passing an atom is the same as passing Atom:_.


%!  rdf_has(?Subject, +Predicate, ?Object) is nondet.
%
%   Succeeds if the triple rdf(Subject,   Predicate, Object) is true
%   exploiting the rdfs:subPropertyOf predicate as   well as inverse
%   predicates   declared   using   rdf_set_predicate/2   with   the
%   =inverse_of= property.

%!  rdf_has(?Subject, +Predicate, ?Object, -RealPredicate) is nondet.
%
%   Same as rdf_has/3, but RealPredicate is   unified  to the actual
%   predicate that makes this relation   true. RealPredicate must be
%   Predicate or an rdfs:subPropertyOf  Predicate.   If  an  inverse
%   match is found, RealPredicate is the term inverse_of(Pred).

%!  rdf_reachable(?Subject, +Predicate, ?Object) is nondet.
%
%   Is true if Object can  be   reached  from  Subject following the
%   transitive predicate Predicate or a  sub-property thereof, while
%   repecting the symetric(true) or inverse_of(P2) properties.
%
%   If used with either Subject or  Object unbound, it first returns
%   the origin, followed by  the  reachable  nodes  in breadth-first
%   search-order. The implementation internally   looks one solution
%   ahead and succeeds deterministically on  the last solution. This
%   predicate never generates the same  node   twice  and  is robust
%   against cycles in the transitive relation.
%
%   With all arguments instantiated,   it succeeds deterministically
%   if a path can be found from  Subject to Object. Searching starts
%   at Subject, assuming the branching factor   is normally lower. A
%   call  with  both  Subject   and    Object   unbound   raises  an
%   instantiation  error.  The  following    example  generates  all
%   subclasses of rdfs:Resource:
%
%     ==
%     ?- rdf_reachable(X, rdfs:subClassOf, rdfs:'Resource').
%     X = 'http://www.w3.org/2000/01/rdf-schema#Resource' ;
%     X = 'http://www.w3.org/2000/01/rdf-schema#Class' ;
%     X = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property' ;
%     ...
%     ==


%!  rdf_reachable(?Subject, +Predicate, ?Object, +MaxD, -D) is nondet.
%
%   Same as rdf_reachable/3, but in addition, MaxD limits the number
%   of edges expanded and D is   unified with the `distance' between
%   Subject and Object. Distance 0 means  Subject and Object are the
%   same resource. MaxD can be the  constant =infinite= to impose no
%   distance-limit.

%!  rdf_subject(?Resource) is nondet.
%
%   True if Resource appears as a   subject. This query respects the
%   visibility rules implied by the logical update view.
%
%   @see rdf_resource/1.

rdf_subject(Resource) :-
    rdf_resource(Resource),
    ( rdf(Resource, _, _) -> true ).

%!  rdf_resource(?Resource) is nondet.
%
%   True when Resource is a resource used as a subject or object in
%   a triple.
%
%   This predicate is primarily intended  as   a  way to process all
%   resources without processing resources twice.   The user must be
%   aware that some of the returned resources  may not appear in any
%   _visible_ triple.


                 /*******************************
                 *     TRIPLE MODIFICATIONS     *
                 *******************************/

%!  rdf_assert(+Subject, +Predicate, +Object) is det.
%
%   Assert a new triple into  the   database.  This is equivalent to
%   rdf_assert/4 using Graph  =user=.  Subject   and  Predicate  are
%   resources. Object is either a resource or a term literal(Value).
%   See rdf/3 for an explanation  of   Value  for typed and language
%   qualified literals. All arguments  are   subject  to  name-space
%   expansion. Complete duplicates (including  the   same  graph and
%   `line' and with a compatible `lifespan')   are  not added to the
%   database.

%!  rdf_assert(+Subject, +Predicate, +Object, +Graph) is det.
%
%   As rdf_assert/3, adding the  predicate   to  the indicated named
%   graph.
%
%   @param Graph is either the name of a   graph (an atom) or a term
%   Graph:Line, where Line is an integer that denotes a line number.

%!  rdf_retractall(?Subject, ?Predicate, ?Object) is det.
%
%   Remove   all   matching   triples   from    the   database.   As
%   rdf_retractall/4 using an unbound graph.

%!  rdf_retractall(?Subject, ?Predicate, ?Object, ?Graph) is det.
%
%   As rdf_retractall/3, also matching Graph.   This  is particulary
%   useful to remove all triples coming from a loaded file. See also
%   rdf_unload/1.

%!  rdf_update(+Subject, +Predicate, +Object, ++Action) is det.
%!  rdf_update(+Subject, +Predicate, +Object, +Graph, ++Action) is det
%
%   Replaces one of the three  (four)   fields  on  the matching triples
%   depending on Action:
%
%     * subject(Resource)
%     Changes the first field of the triple.
%     * predicate(Resource)
%     Changes the second field of the triple.
%     * object(Object)
%     Changes the last field of the triple to the given resource or
%     literal(Value).
%     * graph(Graph)
%     Moves the triple from its current named graph to Graph.
%     This only works with rdf_update/5 and throws an error when
%     used with rdf_update/4.


                 /*******************************
                 *          COLLECTIONS         *
                 *******************************/

%!  rdf_member_property(?Prop, ?Index)
%
%   Deal with the rdf:_1, ... properties.

term_expansion(member_prefix(x),
               member_prefix(Prefix)) :-
    rdf_db:ns(rdf, NS),
    atom_concat(NS, '_', Prefix).
member_prefix(x).

rdf_member_property(P, N) :-
    integer(N),
    !,
    member_prefix(Prefix),
    atom_concat(Prefix, N, P).
rdf_member_property(P, N) :-
    member_prefix(Prefix),
    atom_concat(Prefix, Sub, P),
    atom_number(Sub, N).


                 /*******************************
                 *      ANONYMOUS SUBJECTS      *
                 *******************************/

%!  rdf_node(-Id)
%
%   Generate a unique blank node identifier for a subject.
%
%   @deprecated     New code should use rdf_bnode/1.

rdf_node(Resource) :-
    rdf_bnode(Resource).

%!  rdf_bnode(-Id)
%
%   Generate a unique anonymous identifier for a subject.

rdf_bnode(Value) :-
    repeat,
    gensym('_:genid', Value),
    \+ rdf(Value, _, _),
    \+ rdf(_, _, Value),
    \+ rdf(_, Value, _),
    !.



                 /*******************************
                 *             TYPES            *
                 *******************************/

%!  rdf_is_bnode(+Id)
%
%   Tests if a resource is  a  blank   node  (i.e.  is  an anonymous
%   resource). A blank node is represented   as  an atom that starts
%   with =|_:|=. For backward compatibility   reason, =|__|= is also
%   considered to be a blank node.
%
%   @see rdf_bnode/1.

%!  rdf_is_resource(@Term) is semidet.
%
%   True if Term is an RDF  resource.   Note  that  this is merely a
%   type-test; it does not mean  this   resource  is involved in any
%   triple.  Blank nodes are also considered resources.
%
%   @see rdf_is_bnode/1

rdf_is_resource(Term) :-
    atom(Term).

%!  rdf_is_literal(@Term) is semidet.
%
%   True if Term is an RDF literal object. Currently only checks for
%   groundness and the literal functor.

rdf_is_literal(literal(Value)) :-
    ground(Value).

                 /*******************************
                 *             LITERALS         *
                 *******************************/

%!  rdf_current_literal(-Literal) is nondet.
%
%   True when Literal is a currently  known literal. Enumerates each
%   unique literal exactly once. Note that   it is possible that the
%   literal only appears in already deleted triples. Deleted triples
%   may be locked due to active   queries, transactions or snapshots
%   or may not yet be reclaimed by the garbage collector.


%!  rdf_literal_value(+Literal, -Value) is semidet.
%
%   True when value is  the   appropriate  Prolog  representation of
%   Literal in the RDF _|value space|_.  Current mapping:
%
%     | Plain literals              | Atom                    |
%     | Language tagged literal     | Atom holding plain text |
%     | xsd:string                  | Atom                    |
%     | rdf:XMLLiteral              | XML DOM Tree            |
%     | Numeric XSD type            | Number                  |
%
%   @tbd    Well, this is the long-term idea.
%   @tbd    Add mode (-,+)

:- rdf_meta
    rdf_literal_value(o, -),
    typed_value(r, +, -),
    numeric_value(r, +, -).

rdf_literal_value(literal(String), Value) :-
    atom(String),
    !,
    Value = String.
rdf_literal_value(literal(lang(_Lang, String)), String).
rdf_literal_value(literal(type(Type, String)), Value) :-
    typed_value(Type, String, Value).

typed_value(Numeric, String, Value) :-
    xsdp_numeric_uri(Numeric, NumType),
    !,
    numeric_value(NumType, String, Value).
typed_value(xsd:string, String, String).
typed_value(rdf:'XMLLiteral', Value, DOM) :-
    (   atom(Value)
    ->  setup_call_cleanup(
            ( atom_to_memory_file(Value, MF),
              open_memory_file(MF, read, In, [free_on_close(true)])
            ),
            load_structure(stream(In), DOM, [dialect(xml)]),
            close(In))
    ;   DOM = Value
    ).

numeric_value(xsd:integer, String, Value) :-
    atom_number(String, Value),
    integer(Value).
numeric_value(xsd:float, String, Value) :-
    atom_number(String, Number),
    Value is float(Number).
numeric_value(xsd:double, String, Value) :-
    atom_number(String, Number),
    Value is float(Number).
numeric_value(xsd:decimal, String, Value) :-
    atom_number(String, Value).


                 /*******************************
                 *            SOURCE            *
                 *******************************/

%!  rdf_source_location(+Subject, -Location) is nondet.
%
%   True when triples for Subject are loaded from Location.
%
%   @param Location is a term File:Line.

rdf_source_location(Subject, Source) :-
    findall(Source, rdf(Subject, _, _, Source), Sources),
    sort(Sources, Unique),
    member(Source, Unique).


                 /*******************************
                 *       GARBAGE COLLECT        *
                 *******************************/

%!  rdf_create_gc_thread
%
%   Create the garbage collection thread.

:- public
    rdf_create_gc_thread/0.

rdf_create_gc_thread :-
    thread_create(rdf_gc_loop, _,
                  [ alias('__rdf_GC')
                  ]).

%!  rdf_gc_loop
%
%   Take care of running the RDF garbage collection.  This predicate
%   is called from a thread started by creating the RDF DB.

rdf_gc_loop :-
    catch(rdf_gc_loop(0), E, recover_gc(E)).

recover_gc('$aborted') :-
    !,
    thread_self(Me),
    thread_detach(Me).
recover_gc(Error) :-
    print_message(error, Error),
    rdf_gc_loop.

rdf_gc_loop(CPU) :-
    repeat,
    (   consider_gc(CPU)
    ->  rdf_gc(CPU1),
        sleep(CPU1)
    ;   sleep(0.1)
    ),
    fail.

%!  rdf_gc(-CPU) is det.
%
%   Run RDF GC one time. CPU is  the   amount  of CPU time spent. We
%   update this in Prolog because portable access to thread specific
%   CPU is really hard in C.

rdf_gc(CPU) :-
    statistics(cputime, CPU0),
    (   rdf_gc_
    ->  statistics(cputime, CPU1),
        CPU is CPU1-CPU0,
        rdf_add_gc_time(CPU)
    ;   CPU = 0.0
    ).

%!  rdf_gc is det.
%
%   Run the RDF-DB garbage collector until no   garbage  is left and all
%   tables are fully optimized. Under normal operation a separate thread
%   with identifier =|__rdf_GC|= performs garbage  collection as long as
%   it is considered `useful'.
%
%   Using rdf_gc/0 should  only  be  needed   to  ensure  a  fully clean
%   database for analysis purposes such as leak detection.

rdf_gc :-
    has_garbage,
    !,
    rdf_gc(_),
    rdf_gc.
rdf_gc.

%!  has_garbage is semidet.
%
%   True if there is something to gain using GC.

has_garbage :-
    rdf_gc_info_(Info),
    has_garbage(Info),
    !.

has_garbage(Info) :- arg(2, Info, Garbage),     Garbage > 0.
has_garbage(Info) :- arg(3, Info, Reindexed),   Reindexed > 0.
has_garbage(Info) :- arg(4, Info, Optimizable), Optimizable > 0.

%!  consider_gc(+CPU) is semidet.
%
%   @param CPU is the amount of CPU time spent in the most recent
%   GC.

consider_gc(_CPU) :-
    (   rdf_gc_info_(gc_info(Triples,       % Total #triples in DB
                             Garbage,       % Garbage triples in DB
                             Reindexed,     % Reindexed & not reclaimed
                             Optimizable,   % Non-optimized tables
                             _KeepGen,      % Oldest active generation
                             _LastGCGen,    % Oldest active gen at last GC
                             _ReindexGen,
                             _LastGCReindexGen))
    ->  (   (Garbage+Reindexed) * 5 > Triples
        ;   Optimizable > 4
        )
    ;   print_message(error, rdf(invalid_gc_info)),
        sleep(10)
    ),
    !.


                 /*******************************
                 *           STATISTICS         *
                 *******************************/

%!  rdf_statistics(?KeyValue) is nondet.
%
%   Obtain statistics on the RDF database.  Defined statistics are:
%
%     * graphs(-Count)
%     Number of named graphs.
%
%     * triples(-Count)
%     Total number of triples in the database.  This is the number
%     of asserted triples minus the number of retracted ones.  The
%     number of _visible_ triples in a particular context may be
%     different due to visibility rules defined by the logical
%     update view and transaction isolation.
%
%     * resources(-Count)
%     Number of resources that appear as subject or object in a
%     triple.  See rdf_resource/1.
%
%     * properties(-Count)
%     Number of current predicates.  See rdf_current_predicate/1.
%
%     * literals(-Count)
%     Number of current literals.  See rdf_current_literal/1.
%
%     * gc(GCCount, ReclaimedTriples, ReindexedTriples, Time)
%     Information about the garbage collector.
%
%     * searched_nodes(-Count)
%     Number of nodes expanded by rdf_reachable/3 and
%     rdf_reachable/5.
%
%     * lookup(rdf(S,P,O,G), Count)
%     Number of queries that have been performed for this particular
%     instantiation pattern.  Each of S,P,O,G is either + or -.
%     Fails in case the number of performed queries is zero.
%
%     * hash_quality(rdf(S,P,O,G), Buckets, Quality, PendingResize)
%     Statistics on the index for this pattern.  Indices are created
%     lazily on the first relevant query.
%
%     * triples_by_graph(Graph, Count)
%     This statistics is produced for each named graph. See
%     =triples= for the interpretation of this value.

rdf_statistics(graphs(Count)) :-
    rdf_statistics_(graphs(Count)).
rdf_statistics(triples(Count)) :-
    rdf_statistics_(triples(Count)).
rdf_statistics(duplicates(Count)) :-
    rdf_statistics_(duplicates(Count)).
rdf_statistics(lingering(Count)) :-
    rdf_statistics_(lingering(Count)).
rdf_statistics(resources(Count)) :-
    rdf_statistics_(resources(Count)).
rdf_statistics(properties(Count)) :-
    rdf_statistics_(predicates(Count)).
rdf_statistics(literals(Count)) :-
    rdf_statistics_(literals(Count)).
rdf_statistics(gc(Count, Reclaimed, Reindexed, Time)) :-
    rdf_statistics_(gc(Count, Reclaimed, Reindexed, Time)).
rdf_statistics(searched_nodes(Count)) :-
    rdf_statistics_(searched_nodes(Count)).
rdf_statistics(lookup(Index, Count)) :-
    functor(Indexed, indexed, 16),
    rdf_statistics_(Indexed),
    index(Index, I),
    Arg is I + 1,
    arg(Arg, Indexed, Count),
    Count \== 0.
rdf_statistics(hash_quality(Index, Size, Quality,Optimize)) :-
    rdf_statistics_(hash_quality(List)),
    member(hash(Place,Size,Quality,Optimize), List),
    index(Index, Place).
rdf_statistics(triples_by_graph(Graph, Count)) :-
    rdf_graph_(Graph, Count).

index(rdf(-,-,-,-), 0).
index(rdf(+,-,-,-), 1).
index(rdf(-,+,-,-), 2).
index(rdf(+,+,-,-), 3).
index(rdf(-,-,+,-), 4).
index(rdf(+,-,+,-), 5).
index(rdf(-,+,+,-), 6).
index(rdf(+,+,+,-), 7).

index(rdf(-,-,-,+), 8).
index(rdf(+,-,-,+), 9).
index(rdf(-,+,-,+), 10).
index(rdf(+,+,-,+), 11).
index(rdf(-,-,+,+), 12).
index(rdf(+,-,+,+), 13).
index(rdf(-,+,+,+), 14).
index(rdf(+,+,+,+), 15).


                 /*******************************
                 *           PREDICATES         *
                 *******************************/

%!  rdf_current_predicate(?Predicate) is nondet.
%
%   True when Predicate is a   currently known predicate. Predicates
%   are created if a triples is created  that uses this predicate or
%   a property of the predicate   is  set using rdf_set_predicate/2.
%   The predicate may (no longer) have triples associated with it.
%
%   Note that resources that have  =|rdf:type|= =|rdf:Property|= are
%   not automatically included in the  result-set of this predicate,
%   while _all_ resources that appear as   the  second argument of a
%   triple _are_ included.
%
%   @see rdf_predicate_property/2.

rdf_current_predicate(P, DB) :-
    rdf_current_predicate(P),
    (   rdf(_,P,_,DB)
    ->  true
    ).

%!  rdf_predicate_property(?Predicate, ?Property)
%
%   Query properties of  a  defined   predicate.  Currently  defined
%   properties are given below.
%
%     * symmetric(Bool)
%     True if the predicate is defined to be symetric. I.e., {A} P
%     {B} implies {B} P {A}. Setting symmetric is equivalent to
%     inverse_of(Self).
%
%     * inverse_of(Inverse)
%     True if this predicate is the inverse of Inverse. This
%     property is used by rdf_has/3, rdf_has/4, rdf_reachable/3 and
%     rdf_reachable/5.
%
%     * transitive(Bool)
%     True if this predicate is transitive. This predicate is
%     currently not used. It might be used to make rdf_has/3 imply
%     rdf_reachable/3 for transitive predicates.
%
%     * triples(Triples)
%     Unify Triples with the number of existing triples using this
%     predicate as second argument. Reporting the number of triples
%     is intended to support query optimization.
%
%     * rdf_subject_branch_factor(-Float)
%     Unify Float with the average number of triples associated with
%     each unique value for the subject-side of this relation. If
%     there are no triples the value 0.0 is returned. This value is
%     cached with the predicate and recomputed only after
%     substantial changes to the triple set associated to this
%     relation. This property is intended for path optimalisation
%     when solving conjunctions of rdf/3 goals.
%
%     * rdf_object_branch_factor(-Float)
%     Unify Float with the average number of triples associated with
%     each unique value for the object-side of this relation. In
%     addition to the comments with the =rdf_subject_branch_factor=
%     property, uniqueness of the object value is computed from the
%     hash key rather than the actual values.
%
%     * rdfs_subject_branch_factor(-Float)
%     Same as =rdf_subject_branch_factor=, but also considering
%     triples of `subPropertyOf' this relation. See also rdf_has/3.
%
%     * rdfs_object_branch_factor(-Float)
%     Same as =rdf_object_branch_factor=, but also considering
%     triples of `subPropertyOf' this relation. See also rdf_has/3.
%
%   @see rdf_set_predicate/2.

rdf_predicate_property(P, Prop) :-
    var(P),
    !,
    rdf_current_predicate(P),
    rdf_predicate_property_(P, Prop).
rdf_predicate_property(P, Prop) :-
    rdf_predicate_property_(P, Prop).

%!  rdf_set_predicate(+Predicate, +Property) is det.
%
%   Define a property of  the   predicate.  This predicate currently
%   supports the following properties:
%
%       - symmetric(+Boolean)
%       Set/unset the predicate as being symmetric.  Using
%       symmetric(true) is the same as inverse_of(Predicate),
%       i.e., creating a predicate that is the inverse of
%       itself.
%       - transitive(+Boolean)
%       Sets the transitive property.
%       - inverse_of(+Predicate2)
%       Define Predicate as the inverse of Predicate2. An inverse
%       relation is deleted using inverse_of([]).
%
%   The `transitive` property is currently not used. The `symmetric`
%   and `inverse_of` properties are considered   by  rdf_has/3,4 and
%   rdf_reachable/3.
%
%   @tbd    Maintain these properties based on OWL triples.


                 /*******************************
                 *            SNAPSHOTS         *
                 *******************************/

%!  rdf_snapshot(-Snapshot) is det.
%
%   Take a snapshot of the current state   of  the RDF store. Later,
%   goals may be executed in the  context   of  the database at this
%   moment using rdf_transaction/3 with  the   =snapshot=  option. A
%   snapshot created outside  a  transaction   exists  until  it  is
%   deleted. Snapshots taken inside a transaction   can only be used
%   inside this transaction.

%!  rdf_delete_snapshot(+Snapshot) is det.
%
%   Delete a snapshot as obtained   from  rdf_snapshot/1. After this
%   call, resources used for maintaining the snapshot become subject
%   to garbage collection.

%!  rdf_current_snapshot(?Term) is nondet.
%
%   True when Term is a currently known snapshot.
%
%   @bug    Enumeration of snapshots is slow.

rdf_current_snapshot(Term) :-
    current_blob(Term, rdf_snapshot).


                 /*******************************
                 *          TRANSACTION         *
                 *******************************/

%!  rdf_transaction(:Goal) is semidet.
%
%   Same as rdf_transaction(Goal, user, []).  See rdf_transaction/3.

%!  rdf_transaction(:Goal, +Id) is semidet.
%
%   Same as rdf_transaction(Goal, Id, []).  See rdf_transaction/3.

%!  rdf_transaction(:Goal, +Id, +Options) is semidet.
%
%   Run Goal in an RDF  transaction.   Compared to the ACID model,
%   RDF transactions have the following properties:
%
%     1. Modifications inside the transactions become all atomically
%        visible to the outside world if Goal succeeds or remain
%        invisible if Goal fails or throws an exception.  I.e.,
%        the _atomicy_ property is fully supported.
%     2. _Consistency_ is not guaranteed. Later versions may
%        implement consistency constraints that will be checked
%        serialized just before the actual commit of a transaction.
%     3. Concurrently executing transactions do not infuence each
%        other.  I.e., the _isolation_ property is fully supported.
%     4. _Durability_ can be activated by loading
%        library(semweb/rdf_persistency).
%
%   Processed options are:
%
%     * snapshot(+Snapshot)
%     Execute Goal using the state of the RDF store as stored in
%     Snapshot.  See rdf_snapshot/1.  Snapshot can also be the
%     atom =true=, which implies that an anonymous snapshot is
%     created at the current state of the store.  Modifications
%     due to executing Goal are only visible to Goal.

rdf_transaction(Goal) :-
    rdf_transaction(Goal, user, []).
rdf_transaction(Goal, Id) :-
    rdf_transaction(Goal, Id, []).

%!  rdf_active_transaction(?Id) is nondet.
%
%   True if Id is the identifier of  a transaction in the context of
%   which  this  call  is  executed.  If  Id  is  not  instantiated,
%   backtracking yields transaction identifiers   starting  with the
%   innermost nested transaction. Transaction   identifier terms are
%   not copied, need not be ground   and  can be instantiated during
%   the transaction.

rdf_active_transaction(Id) :-
    rdf_active_transactions_(List),
    member(Id, List).

%!  rdf_monitor(:Goal, +Options)
%
%   Call Goal if specified actions occur on the database.

rdf_monitor(Goal, Options) :-
    monitor_mask(Options, 0xffff, Mask),
    rdf_monitor_(Goal, Mask).

monitor_mask([], Mask, Mask).
monitor_mask([H|T], Mask0, Mask) :-
    update_mask(H, Mask0, Mask1),
    monitor_mask(T, Mask1, Mask).

update_mask(-X, Mask0, Mask) :-
    !,
    monitor_mask(X, M),
    Mask is Mask0 /\ \M.
update_mask(+X, Mask0, Mask) :-
    !,
    monitor_mask(X, M),
    Mask is Mask0 \/ M.
update_mask(X, Mask0, Mask) :-
    monitor_mask(X, M),
    Mask is Mask0 \/ M.

%!  monitor_mask(Name, Mask)
%
%   Mask bit for the monitor events.  Note that this must be kept
%   consistent with the enum broadcast_id defined in rdf_db.c

                                        % C-defined broadcasts
monitor_mask(assert,       0x0001).
monitor_mask(assert(load), 0x0002).
monitor_mask(retract,      0x0004).
monitor_mask(update,       0x0008).
monitor_mask(new_literal,  0x0010).
monitor_mask(old_literal,  0x0020).
monitor_mask(transaction,  0x0040).
monitor_mask(load,         0x0080).
monitor_mask(create_graph, 0x0100).
monitor_mask(reset,        0x0200).
                                        % prolog defined broadcasts
monitor_mask(parse,        0x1000).
monitor_mask(unload,       0x1000).     % FIXME: Duplicate
                                        % mask for all
monitor_mask(all,          0xffff).

%rdf_broadcast(Term, MaskName) :-
%%      monitor_mask(MaskName, Mask),
%%      rdf_broadcast_(Term, Mask).


                 /*******************************
                 *            WARM              *
                 *******************************/

%!  rdf_warm_indexes
%
%   Warm all indexes.  See rdf_warm_indexes/1.

rdf_warm_indexes :-
    findall(Index, rdf_index(Index), Indexes),
    rdf_warm_indexes(Indexes).

rdf_index(s).
rdf_index(p).
rdf_index(o).
rdf_index(sp).
rdf_index(o).
rdf_index(po).
rdf_index(spo).
rdf_index(g).
rdf_index(sg).
rdf_index(pg).

%!  rdf_warm_indexes(+Indexes) is det.
%
%   Create the named indexes.  Normally,   the  RDF database creates
%   indexes on lazily the first time they are needed. This predicate
%   serves two purposes: it provides an   explicit  way to make sure
%   that the required indexes  are   present  and  creating multiple
%   indexes at the same time is more efficient.


                 /*******************************
                 *          DUPLICATES          *
                 *******************************/

%!  rdf_update_duplicates is det.
%
%   Update the duplicate administration of the RDF store. This marks
%   every triple that is potentionally  a   duplicate  of another as
%   duplicate. Being potentially a  duplicate   means  that subject,
%   predicate and object are equivalent and   the  life-times of the
%   two triples overlap.
%
%   The duplicates marks are used to  reduce the administrative load
%   of avoiding duplicate answers.  Normally,   the  duplicates  are
%   marked using a background thread that   is  started on the first
%   query that produces a substantial amount of duplicates.

:- public
    rdf_update_duplicates_thread/0.

%!  rdf_update_duplicates_thread
%
%   Start a thread to initialize the duplicate administration.

rdf_update_duplicates_thread :-
    thread_create(rdf_update_duplicates, _,
                  [ detached(true),
                    alias('__rdf_duplicate_detecter')
                  ]).

%!  rdf_update_duplicates is det.
%
%   Update the duplicate administration. If   this  adminstration is
%   up-to-date, each triples that _may_ have a duplicate is flagged.
%   The predicate rdf/3 uses this administration to speedup checking
%   for duplicate answers.
%
%   This predicate is normally  executed   from  a background thread
%   named =__rdf_duplicate_detecter= which is created   when a query
%   discovers that checking for duplicates becomes too expensive.


                 /*******************************
                 *    QUICK BINARY LOAD/SAVE    *
                 *******************************/

%!  rdf_save_db(+File) is det.
%!  rdf_save_db(+File, +Graph) is det.
%
%   Save triples into File in a   quick-to-load binary format. If Graph
%   is supplied only triples flagged to originate from that database
%   are  added.  Files  created  this  way    can  be  loaded  using
%   rdf_load_db/1.

:- create_prolog_flag(rdf_triple_format, 3, [type(integer)]).

rdf_save_db(File) :-
    current_prolog_flag(rdf_triple_format, Version),
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( set_stream(Out, record_position(false)),
          rdf_save_db_(Out, _, Version)
        ),
        close(Out)).


rdf_save_db(File, Graph) :-
    current_prolog_flag(rdf_triple_format, Version),
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( set_stream(Out, record_position(false)),
          rdf_save_db_(Out, Graph, Version)
        ),
        close(Out)).


%!  rdf_load_db_no_admin(+File, +Id, -Graphs) is det.
%
%   Load triples from a  .trp  file   without  updating  the  source
%   administration. Id is  handled  to   monitor  action.  Graphs is
%   a list of graph-names encountered in File.

rdf_load_db_no_admin(File, Id, Graphs) :-
    open(File, read, In, [type(binary)]),
    set_stream(In, record_position(false)),
    call_cleanup(rdf_load_db_(In, Id, Graphs), close(In)).


%!  check_loaded_cache(+Graph, +Graphs, +Modified) is det.
%
%   Verify the loaded cache file and optionally fix the modification
%   time (new versions save this along with the snapshot).
%
%   @tbd    What to do if there is a cache mismatch? Delete the loaded
%           graphs and fail?

check_loaded_cache(DB, [DB], _Modified) :- !.
check_loaded_cache(DB, Graphs, _) :-
    print_message(warning, rdf(inconsistent_cache(DB, Graphs))).


%!  rdf_load_db(+File) is det.
%
%   Load triples from a file created using rdf_save_db/2.

rdf_load_db(File) :-
    uri_file_name(URL, File),
    rdf_load_db_no_admin(File, URL, _Graphs).


                 /*******************************
                 *          LOADING RDF         *
                 *******************************/

:- multifile
    rdf_open_hook/8,
    rdf_open_decode/4,              % +Encoding, +File, -Stream, -Cleanup
    rdf_load_stream/3,              % +Format, +Stream, +Options
    rdf_file_type/2,                % ?Extension, ?Format
    rdf_storage_encoding/2,         % ?Extension, ?Encoding
    url_protocol/1.                 % ?Protocol

%!  rdf_load(+FileOrList) is det.
%
%   Same as rdf_load(FileOrList, []).  See rdf_load/2.

%!  rdf_load(+FileOrList, :Options) is det.
%
%   Load RDF data. Options provides   additional processing options.
%   Defined options are:
%
%       * blank_nodes(+ShareMode)
%       How to handle equivalent blank nodes.  If =share= (default),
%       equivalent blank nodes are shared in the same resource.
%
%       * base_uri(+URI)
%       URI that is used for rdf:about="" and other RDF constructs
%       that are relative to the base uri.  Default is the source
%       URL.
%
%       * concurrent(+Jobs)
%       If FileOrList is a list of files, process the input files
%       using Jobs threads concurrently.  Default is the mininum
%       of the number of cores and the number of inputs.  Higher
%       values can be useful when loading inputs from (slow)
%       network connections.  Using 1 (one) does not use
%       separate worker threads.
%
%       * format(+Format)
%       Specify the source format explicitly. Normally this is
%       deduced from the filename extension or the mime-type. The
%       core library understands the formats xml (RDF/XML) and
%       triples (internal quick load and cache format).  Plugins,
%       such as library(semweb/turtle) extend the set of recognised
%       extensions.
%
%       * graph(?Graph)
%       Named graph in which to load the data.  It is *not* allowed
%       to load two sources into the same named graph.  If Graph is
%       unbound, it is unified to the graph into which the data is
%       loaded.  The default graph is a =|file://|= URL when loading
%       a file or, if the specification is a URL, its normalized
%       version without the optional _|#fragment|_.
%
%       * if(Condition)
%       When to load the file. One of =true=, =changed= (default) or
%       =not_loaded=.
%
%       * modified(-Modified)
%       Unify Modified with one of =not_modified=, cached(File),
%       last_modified(Stamp) or =unknown=.
%
%       * cache(Bool)
%       If =false=, do not use or create a cache file.
%
%       * register_namespaces(Bool)
%       If =true= (default =false=), register =xmlns= namespace
%       declarations or Turtle =|@prefix|= prefixes using
%       rdf_register_prefix/3 if there is no conflict.
%
%       * silent(+Bool)
%       If =true=, the message reporting completion is printed using
%       level =silent=. Otherwise the level is =informational=. See
%       also print_message/2.
%
%       * prefixes(-Prefixes)
%       Returns the prefixes defined in the source   data file as a list
%       of pairs.
%
%       * multifile(+Boolean)
%       Indicate that the addressed graph may be populated with
%       triples from multiple sources. This disables caching and
%       avoids that an rdf_load/2 call affecting the specified
%       graph cleans the graph.
%
%   Other  options  are  forwarded  to  process_rdf/3.  By  default,
%   rdf_load/2 only loads RDF/XML from files.  It can be extended to
%   load data from other formats and   locations  using plugins. The
%   full set of plugins relevant to   support  different formats and
%   locations is below:
%
%     ==
%     :- use_module(library(semweb/turtle)).        % Turtle and TriG
%     :- use_module(library(semweb/rdf_ntriples)).
%     :- use_module(library(semweb/rdf_zlib_plugin)).
%     :- use_module(library(semweb/rdf_http_plugin)).
%     :- use_module(library(http/http_ssl_plugin)).
%     ==
%
%   @see    rdf_db:rdf_open_hook/3, library(semweb/rdf_persistency) and
%           library(semweb/rdf_cache)

:- dynamic
    rdf_loading/3.                          % Graph, Queue, Thread

rdf_load(Spec) :-
    rdf_load(Spec, []).

:- if(\+current_predicate(concurrent/3)).
concurrent(_, Goals, _) :-
    forall(member(G, Goals), call(G)).
:- endif.

% Note that we kill atom garbage collection.  This improves performance
% with about 15% loading the LUBM Univ_50 benchmark.

rdf_load(Spec, M:Options) :-
    must_be(list, Options),
    current_prolog_flag(agc_margin, Old),
    setup_call_cleanup(
        set_prolog_flag(agc_margin, 0),
        rdf_load_noagc(Spec, M, Options),
        set_prolog_flag(agc_margin, Old)).

rdf_load_noagc(List, M, Options) :-
    is_list(List),
    !,
    flatten(List, Inputs),          % Compatibility: allow nested lists
    maplist(must_be(ground), Inputs),
    length(Inputs, Count),
    load_jobs(Count, Jobs, Options),
    (   Jobs =:= 1
    ->  forall(member(Spec, Inputs),
               rdf_load_one(Spec, M, Options))
    ;   maplist(load_goal(Options, M), Inputs, Goals),
        concurrent(Jobs, Goals, [])
    ).
rdf_load_noagc(One, M, Options) :-
    must_be(ground, One),
    rdf_load_one(One, M, Options).

load_goal(Options, M, Spec, rdf_load_one(Spec, M, Options)).

load_jobs(_, Jobs, Options) :-
    option(concurrent(Jobs), Options),
    !,
    must_be(positive_integer, Jobs).
load_jobs(Count, Jobs, _) :-
    current_prolog_flag(cpu_count, CPUs),
    CPUs > 0,
    !,
    Jobs is max(1, min(CPUs, Count)).
load_jobs(_, 1, _).


rdf_load_one(Spec, M, Options) :-
    source_url(Spec, Protocol, SourceURL),
    load_graph(SourceURL, Graph, Options),
    setup_call_cleanup(
        with_mutex(rdf_load_file,
                   rdf_start_load(SourceURL, Loading)),
        rdf_load_file(Loading, Spec, SourceURL, Protocol,
                      Graph, M, Options),
        rdf_end_load(Loading)).

%!  rdf_start_load(+SourceURL, -WhatToDo) is det.
%!  rdf_end_load(+WhatToDo) is det.
%!  rdf_load_file(+WhatToDo, +Spec, +SourceURL, +Protocol, +Graph,
%!                +Module, +Options) is det.
%
%   Of these three predicates, rdf_load_file/7   does the real work.
%   The others deal with the  possibility   that  the graph is being
%   loaded by another thread. In that case,   we  wait for the other
%   thread to complete the work.
%
%   @tbd    What if both threads disagree on what is loaded into the
%           graph?
%   @see    Code is modelled closely after how concurrent loading
%           is handled in SWI-Prolog's boot/init.pl

rdf_start_load(SourceURL, queue(Queue)) :-
    rdf_loading(SourceURL, Queue, LoadThread),
    \+ thread_self(LoadThread),
    !,
    debug(rdf(load), '~p is being loaded by thread ~w; waiting ...',
          [ SourceURL, LoadThread]).
rdf_start_load(SourceURL, Ref) :-
    thread_self(Me),
    message_queue_create(Queue),
    assertz(rdf_loading(SourceURL, Queue, Me), Ref).

rdf_end_load(queue(_)) :- !.
rdf_end_load(Ref) :-
    clause(rdf_loading(_, Queue, _), _, Ref),
    erase(Ref),
    thread_send_message(Queue, done),
    message_queue_destroy(Queue).

rdf_load_file(queue(Queue), _Spec, _SourceURL, _Protocol, _Graph, _M, _Options) :-
    !,
    catch(thread_get_message(Queue, _), _, true).
rdf_load_file(_Ref, _Spec, SourceURL, Protocol, Graph, M, Options) :-
    debug(rdf(load), 'RDF: Loading ~q into ~q', [SourceURL, Graph]),
    statistics(cputime, T0),
    rdf_open_input(SourceURL, Protocol, Graph,
                   In, Cleanup, Modified, Format, Options),
    supported_format(Format, Cleanup),
    return_modified(Modified, Options),
    (   Modified == not_modified
    ->  Action = none
    ;   Modified = cached(CacheFile)
    ->  do_unload(Graph),
        catch(rdf_load_db_no_admin(CacheFile, cache(Graph), Graphs), _, fail),
        check_loaded_cache(Graph, Graphs, Modified),
        Action = load
    ;   option(base_uri(BaseURI), Options, Graph),
        (   var(BaseURI)
        ->  BaseURI = SourceURL
        ;   true
        ),
        once(phrase(derived_options(Options, NSList), Extra)),
        merge_options([ base_uri(BaseURI),
                        graph(Graph),
                        format(Format)
                      | Extra
                      ], Options, RDFOptions),
        (   option(multifile(true), Options)
        ->  true
        ;   do_unload(Graph)
        ),
        graph_modified(Modified, ModifiedStamp),
        rdf_set_graph_source(Graph, SourceURL, ModifiedStamp),
        call_cleanup(rdf_load_stream(Format, In, M:RDFOptions),
                     Cleanup),
        save_cache(Graph, SourceURL, Options),
        register_file_prefixes(NSList),
        format_action(Format, Action)
    ),
    rdf_statistics_(triples(Graph, Triples)),
    report_loaded(Action, SourceURL, Graph, Triples, T0, Options).

supported_format(Format, _Cleanup) :-
    rdf_file_type(_, Format),
    !.
supported_format(Format, Cleanup) :-
    call(Cleanup),
    existence_error(rdf_format_plugin, Format).

format_action(triples, load) :- !.
format_action(_, parsed).

save_cache(Graph, SourceURL, Options) :-
    option(cache(true), Options, true),
    rdf_cache_file(SourceURL, write, CacheFile),
    !,
    catch(save_cache(Graph, CacheFile), E,
          print_message(warning, E)).
save_cache(_, _, _).

derived_options([], _) -->
    [].
derived_options([H|T], NSList) -->
    (   {   H == register_namespaces(true)
        ;   H == (register_namespaces = true)
        }
    ->  [ namespaces(NSList) ]
    ;   []
    ),
    derived_options(T, NSList).

graph_modified(last_modified(Stamp), Stamp).
graph_modified(unknown, Stamp) :-
    get_time(Stamp).

return_modified(Modified, Options) :-
    option(modified(M0), Options),
    !,
    M0 = Modified.
return_modified(_, _).


                 /*******************************
                 *        INPUT HANDLING        *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section deals with pluggable input sources.  The task of the input
layer is

    * Decide on the graph-name
    * Decide on the source-location
    * Decide whether loading is needed (if-modified)
    * Decide on the serialization in the input

The protocol must ensure minimal  overhead,   in  particular for network
protocols. E.g. for HTTP we want to make a single call on the server and
use If-modified-since to verify that we need not reloading this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  rdf_open_input(+SourceURL, +Protocol, +Graph,
%!                 -Stream, -Cleanup, -Modified, -Format, +Options)
%
%   Open an input source.
%
%   Options processed:
%
%       * graph(Graph)
%       * db(Graph)
%       * if(Condition)
%       * cache(Cache)
%       * format(Format)
%
%   @param  Modified is one of =not_modified=, last_modified(Time),
%           cached(CacheFile) or =unknown=

rdf_open_input(SourceURL, Protocol, Graph,
               Stream, Cleanup, Modified, Format, Options) :-
    (   option(multifile(true), Options)
    ->  true
    ;   option(if(If), Options, changed),
        (   If == true
        ->  true
        ;   rdf_graph_source_(Graph, SourceURL, HaveModified)
        ->  true
        ;   option(cache(true), Options, true),
            rdf_cache_file(SourceURL, read, CacheFile)
        ->  time_file(CacheFile, HaveModified)
        ;   true
        )
    ),
    option(format(Format), Options, _),
    open_input_if_modified(Protocol, SourceURL, HaveModified,
                           Stream, Cleanup, Modified0, Format, Options),
    (   Modified0 == not_modified
    ->  (   nonvar(CacheFile)
        ->  Modified = cached(CacheFile)
        ;   Modified = not_modified
        )
    ;   Modified = Modified0
    ).


%!  source_url(+Spec, -Class, -SourceURL) is det.
%
%   Determine class and url of the source.  Class is one of
%
%       * stream(Stream)
%       * file
%       * a url-protocol (e.g., =http=)

source_url(stream(In), stream(In), SourceURL) :-
    !,
    (   stream_property(In, file_name(File))
    ->  to_url(File, SourceURL)
    ;   gensym('stream://', SourceURL)
    ).
source_url(Stream, Class, SourceURL) :-
    is_stream(Stream),
    !,
    source_url(stream(Stream), Class, SourceURL).
source_url(Spec, Protocol, SourceURL) :-
    compound(Spec),
    !,
    source_file(Spec, Protocol, SourceURL).
source_url(FileURL, Protocol, SourceURL) :-             % or return FileURL?
    uri_file_name(FileURL, File),
    !,
    source_file(File, Protocol, SourceURL).
source_url(SourceURL0, Protocol, SourceURL) :-
    is_url(SourceURL0, Protocol, SourceURL),
    !.
source_url(File, Protocol, SourceURL) :-
    source_file(File, Protocol, SourceURL).

source_file(Spec, file(SExt), SourceURL) :-
    findall(Ext, valid_extension(Ext), Exts),
    absolute_file_name(Spec, File, [access(read), extensions([''|Exts])]),
    storage_extension(_Plain, SExt, File),
    uri_file_name(SourceURL, File).

to_url(URL, URL) :-
    uri_is_global(URL),
    !.
to_url(File, URL) :-
    absolute_file_name(File, Path),
    uri_file_name(URL, Path).

storage_extension(Plain, SExt, File) :-
    file_name_extension(Plain, SExt, File),
    SExt \== '',
    rdf_storage_encoding(SExt, _),
    !.
storage_extension(File, '', File).

%!  load_graph(+SourceURL, -Graph, +Options) is det.
%
%   Graph is the graph into which  we   load  the  data. Tries these
%   options:
%
%     1. The graph(Graph) option
%     2. The db(Graph) option (backward compatibility)
%     3. The base_uri(BaseURI) option
%     4. The source URL

load_graph(_Source, Graph, Options) :-
    option(multifile(true), Options),
    !,
    (   (   option(graph(Graph), Options)
        ->  true
        ;   option(db(Graph), Options)
        ),
        ground(Graph)
    ->  true
    ;   throw(error(existence_error(option, graph),
                    context(_, "rdf_load/2: using multifile requires graph")))
    ).
load_graph(Source, Graph, Options) :-
    (   option(graph(Graph), Options)
    ;   option(db(Graph), Options)
    ),
    !,
    load_graph2(Source, Graph, Options).
load_graph(Source, Graph, Options) :-
    load_graph2(Source, Graph, Options).

load_graph2(_, Graph, _) :-
    ground(Graph),
    !.
load_graph2(_Source, Graph, Options) :-
    option(base_uri(Graph), Options),
    Graph \== [],
    ground(Graph),
    !.
load_graph2(Source, Graph, _) :-
    load_graph(Source, Graph).

load_graph(SourceURL, BaseURI) :-
    file_name_extension(BaseURI, Ext, SourceURL),
    rdf_storage_encoding(Ext, _),
    !.
load_graph(SourceURL, SourceURL).


open_input_if_modified(stream(In), SourceURL, _, In, true,
                       unknown, Format, _) :-
    !,
    (   var(Format)
    ->  guess_format(SourceURL, Format)
    ;   true
    ).
open_input_if_modified(file(SExt), SourceURL, HaveModified, Stream, Cleanup,
                       Modified, Format, _) :-
    !,
    uri_file_name(SourceURL, File),
    (   SExt == '' -> Plain = File; file_name_extension(Plain, SExt, File)),
    time_file(File, LastModified),
    (   nonvar(HaveModified),
        HaveModified >= LastModified
    ->  Modified = not_modified,
        Cleanup = true
    ;   storage_open(SExt, File, Stream, Cleanup),
        Modified = last_modified(LastModified),
        (   var(Format)
        ->  guess_format(Plain, Format)
        ;   true
        )
    ).
open_input_if_modified(file, SourceURL, HaveModified, Stream, Cleanup,
                       Modified, Format, Options) :-
    !,
    open_input_if_modified(file(''), SourceURL, HaveModified,
                           Stream, Cleanup,
                           Modified, Format, Options).
open_input_if_modified(Protocol, SourceURL, HaveModified, Stream, Cleanup,
                       Modified, Format, Options) :-
    rdf_open_hook(Protocol, SourceURL, HaveModified, Stream, Cleanup,
                  Modified, Format, Options).

guess_format(File, Format) :-
    file_name_extension(_, Ext, File),
    (   rdf_file_type(Ext, Format)
    ->  true
    ;   Format = xml,
        print_message(warning, rdf(guess_format(Ext)))
    ).

%!  storage_open(+Extension, +File, -Stream, -Cleanup)
%
%   Open the low-level storage. Note  that   the  file  is opened as
%   binary. This is the same  as   for  HTTP  resources. The correct
%   encoding will be set by the XML parser or the Turtle parser.

storage_open('', File, Stream, close(Stream)) :-
    !,
    open(File, read, Stream, [type(binary)]).
storage_open(Ext, File, Stream, Cleanup) :-
    rdf_storage_encoding(Ext, Encoding),
    rdf_open_decode(Encoding, File, Stream, Cleanup).

valid_extension(Ext) :-
    rdf_file_type(Ext, _).
valid_extension(Ext) :-
    rdf_storage_encoding(Ext, _).

%!  is_url(@Term, -Scheme, -URL) is semidet.
%
%   True if Term is an atom denoting URL of the given Scheme. URL is
%   normalized  (see  uri_normalized/2)  and   a  possible  fragment
%   identifier (#fragment) is removed. This  predicate only succeeds
%   if  the  scheme  is   registered    using   the  multifile  hook
%   url_protocol/1.

is_url(URL, Scheme, FetchURL) :-
    atom(URL),
    uri_is_global(URL),
    uri_normalized(URL, URL1),              % case normalization
    uri_components(URL1, Components),
    uri_data(scheme, Components, Scheme0),
    url_protocol(Scheme0),
    !,
    Scheme = Scheme0,
    uri_data(fragment, Components, _, Components1),
    uri_components(FetchURL, Components1).

url_protocol(file).                     % built-in

%!  rdf_file_type(+Extension, -Format) is semidet.
%
%   True if Format  is  the  format   belonging  to  the  given file
%   extension.  This predicate is multifile and can thus be extended
%   by plugins.

rdf_file_type(xml,   xml).
rdf_file_type(rdf,   xml).
rdf_file_type(rdfs,  xml).
rdf_file_type(owl,   xml).
rdf_file_type(htm,   xhtml).
rdf_file_type(html,  xhtml).
rdf_file_type(xhtml, xhtml).
rdf_file_type(trp,   triples).


%!  rdf_file_encoding(+Extension, -Format) is semidet.
%
%   True if Format describes the storage encoding of file.

rdf_storage_encoding('', plain).


%!  rdf_load_stream(+Format, +Stream, :Options)
%
%   Load RDF data from Stream.
%
%   @tbd    Handle mime-types?

rdf_load_stream(xml, Stream, Options) :-
    !,
    graph(Options, Graph),
    rdf_transaction(load_stream(Stream, Options),
                    parse(Graph)).
rdf_load_stream(xhtml, Stream, M:Options) :-
    !,
    graph(Options, Graph),
    rdf_transaction(load_stream(Stream, M:[embedded(true)|Options]),
                    parse(Graph)).
rdf_load_stream(triples, Stream, Options) :-
    !,
    graph(Options, Graph),
    rdf_load_db_(Stream, Graph, _Graphs).

load_stream(Stream, M:Options) :-
    process_rdf(Stream, assert_triples, M:Options),
    option(graph(Graph), Options),
    rdf_graph_clear_modified_(Graph).


%!  report_loaded(+Action, +Source, +DB, +Triples, +StartCPU, +Options)

report_loaded(none, _, _, _, _, _) :- !.
report_loaded(Action, Source, DB, Triples, T0, Options) :-
    statistics(cputime, T1),
    Time is T1 - T0,
    (   option(silent(true), Options)
    ->  Level = silent
    ;   Level = informational
    ),
    print_message(Level,
                  rdf(loaded(Action, Source, DB, Triples, Time))).


%!  rdf_unload(+Source) is det.
%
%   Identify the graph loaded from Source and use rdf_unload_graph/1
%   to erase this graph.
%
%   @deprecated     For compatibility, this predicate also accepts a
%                   graph name instead of a source specification.
%                   Please update your code to use
%                   rdf_unload_graph/1.

rdf_unload(Spec) :-
    source_url(Spec, _Protocol, SourceURL),
    rdf_graph_source_(Graph, SourceURL, _),
    !,
    rdf_unload_graph(Graph).
rdf_unload(Graph) :-
    atom(Graph),
    rdf_graph(Graph),
    !,
    warn_deprecated_unload(Graph),
    rdf_unload_graph(Graph).
rdf_unload(_).

:- dynamic
    warned/0.

warn_deprecated_unload(_) :-
    warned,
    !.
warn_deprecated_unload(Graph) :-
    assertz(warned),
    print_message(warning, rdf(deprecated(rdf_unload(Graph)))).


%!  rdf_unload_graph(+Graph) is det.
%
%   Remove Graph from the RDF store.  Succeeds silently if the named
%   graph does not exist.

rdf_unload_graph(Graph) :-
    must_be(atom, Graph),
    (   rdf_graph(Graph)
    ->  rdf_transaction(do_unload(Graph), unload(Graph))
    ;   true
    ).

do_unload(Graph) :-
    (   rdf_graph_(Graph, Triples),
        Triples > 0
    ->  rdf_retractall(_,_,_,Graph)
    ;   true
    ),
    rdf_destroy_graph(Graph).

                 /*******************************
                 *         GRAPH QUERIES        *
                 *******************************/

%!  rdf_create_graph(+Graph) is det.
%
%   Create an RDF graph without triples.   Succeeds  silently if the
%   graph already exists.


%!  rdf_graph(?Graph) is nondet.
%
%   True when Graph is an existing graph.

rdf_graph(Graph) :-
    rdf_graph_(Graph, _Triples).

%!  rdf_source(?Graph, ?SourceURL) is nondet.
%
%   True if named Graph is loaded from SourceURL.
%
%   @deprecated Use rdf_graph_property(Graph, source(SourceURL)).

rdf_source(Graph, SourceURL) :-
    rdf_graph(Graph),
    rdf_graph_source_(Graph, SourceURL, _Modified).

%!  rdf_source(?Source)
%
%   True if Source is a loaded source.
%
%   @deprecated     Use rdf_graph/1 or rdf_source/2.

rdf_source(SourceURL) :-
    rdf_source(_Graph, SourceURL).

%!  rdf_make
%
%   Reload all loaded files that have been modified since the last
%   time they were loaded.

rdf_make :-
    findall(Source-Graph, modified_graph(Source, Graph), Modified),
    forall(member(Source-Graph, Modified),
           catch(rdf_load(Source, [graph(Graph), if(changed)]), E,
                 print_message(error, E))).

modified_graph(SourceURL, Graph) :-
    rdf_graph(Graph),
    rdf_graph_source_(Graph, SourceURL, Modified),
    \+ sub_atom(SourceURL, 0, _, _, 'stream://'),
    Modified > 0.

%!  rdf_graph_property(?Graph, ?Property) is nondet.
%
%   True when Property is a property of Graph.  Defined properties
%   are:
%
%       * hash(Hash)
%       Hash is the (MD5-)hash for the content of Graph.
%       * modified(Boolean)
%       True if the graph is modified since it was loaded or
%       rdf_set_graph/2 was called with modified(false).
%       * source(Source)
%       The graph is loaded from the Source (a URL)
%       * source_last_modified(?Time)
%       Time is the last-modified timestamp of Source at the moment
%       the graph was loaded from Source.
%       * triples(Count)
%       True when Count is the number of triples in Graph.
%
%    Additional graph properties can be added  by defining rules for
%    the multifile predicate  property_of_graph/2.   Currently,  the
%    following extensions are defined:
%
%       - library(semweb/rdf_persistency)
%         - persistent(Boolean)
%           Boolean is =true= if the graph is persistent.

rdf_graph_property(Graph, Property) :-
    rdf_graph(Graph),
    property_of_graph(Property, Graph).

:- multifile
    property_of_graph/2.

property_of_graph(hash(Hash), Graph) :-
    rdf_md5(Graph, Hash).
property_of_graph(modified(Boolean), Graph) :-
    rdf_graph_modified_(Graph, Boolean, _).
property_of_graph(source(URL), Graph) :-
    rdf_graph_source_(Graph, URL, _).
property_of_graph(source_last_modified(Time), Graph) :-
    rdf_graph_source_(Graph, _, Time),
    Time > 0.0.
property_of_graph(triples(Count), Graph) :-
    rdf_graph_(Graph, Count).

%!  rdf_set_graph(+Graph, +Property) is det.
%
%   Set properties of Graph.  Defined properties are:
%
%       * modified(false)
%       Set the modified state of Graph to false.

rdf_set_graph(Graph, modified(Modified)) :-
    must_be(oneof([false]), Modified),
    rdf_graph_clear_modified_(Graph).


%!  save_cache(+DB, +Cache) is det.
%
%   Save triples belonging to DB in the file Cache.

save_cache(DB, Cache) :-
    current_prolog_flag(rdf_triple_format, Version),
    setup_call_cleanup(
        catch(open(Cache, write, CacheStream, [type(binary)]), _, fail),
        rdf_save_db_(CacheStream, DB, Version),
        close(CacheStream)).

%!  assert_triples(+Triples, +Source)
%
%   Assert a list of triples into the database. Foir security
%   reasons we check we aren't inserting anything but nice RDF
%   triples.

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], DB) :-
    !,
    rdf_assert(S, P, O, DB),
    assert_triples(T, DB).
assert_triples([H|_], _) :-
    throw(error(type_error(rdf_triple, H), _)).


                 /*******************************
                 *             RESET            *
                 *******************************/

%!  rdf_reset_db
%
%   Remove all triples from the RDF database and reset all its
%   statistics.
%
%   @bug    This predicate checks for active queries, but this check is
%           not properly synchronized and therefore the use of this
%           predicate is unsafe in multi-threaded contexts. It is
%           mainly used to run functionality tests that need to
%           start with an empty database.

rdf_reset_db :-
    reset_gensym('_:genid'),
    rdf_reset_db_.


                 /*******************************
                 *           SAVE RDF           *
                 *******************************/

%!  rdf_save(+Out) is det.
%
%   Same as rdf_save(Out, []).  See rdf_save/2 for details.

%!  rdf_save(+Out, :Options) is det.
%
%   Write RDF data as RDF/XML. Options is a list of one or more of
%   the following options:
%
%           * graph(+Graph)
%           Save only triples associated to the given named Graph.
%
%           * anon(Bool)
%           If =false= (default =true=) do not save blank nodes that do
%           not appear (indirectly) as object of a named resource.
%
%           * base_uri(URI)
%           BaseURI used. If present, all URIs that can be
%           represented relative to this base are written using
%           their shorthand.  See also =write_xml_base= option.
%
%           * convert_typed_literal(:Convertor)
%           Call Convertor(-Type, -Content, +RDFObject), providing
%           the opposite for the convert_typed_literal option of
%           the RDF parser.
%
%           * document_language(+Lang)
%           Initial =|xml:lang|= saved with rdf:RDF element.
%
%           * encoding(Encoding)
%           Encoding for the output.  Either utf8 or iso_latin_1.
%
%           * inline(+Bool)
%           If =true= (default =false=), inline resources when
%           encountered for the first time. Normally, only bnodes
%           are handled this way.
%
%           * namespaces(+List)
%           Explicitly specify saved namespace declarations. See
%           rdf_save_header/2 option namespaces for details.
%
%           * sorted(+Boolean)
%           If =true= (default =false=), emit subjects sorted on
%           the full URI.  Useful to make file comparison easier.
%
%           * write_xml_base(Bool)
%           If =false=, do _not_ include the =|xml:base|=
%           declaration that is written normally when using the
%           =base_uri= option.
%
%           * xml_attributes(+Bool)
%           If =false= (default =true=), never use xml attributes to
%           save plain literal attributes, i.e., always used an XML
%           element as in =|<name>Joe</name>|=.
%
%   @param Out      Location to save the data.  This can also be a
%                   file-url (=|file://path|=) or a stream wrapped
%                   in a term stream(Out).
%   @see rdf_save_db/1

:- thread_local
    named_anon/2,                   % +Resource, -Id
    inlined/1.                      % +Resource

rdf_save(File) :-
    rdf_save2(File, []).

rdf_save(Spec, M:Options0) :-
    is_list(Options0),
    !,
    meta_options(save_meta_option, M:Options0, Options),
    to_file(Spec, File),
    rdf_save2(File, Options).
rdf_save(Spec, _:DB) :-
    atom(DB),                      % backward compatibility
    !,
    to_file(Spec, File),
    rdf_save2(File, [graph(DB)]).

save_meta_option(convert_typed_literal).

to_file(URL, File) :-
    atom(URL),
    uri_file_name(URL, File),
    !.
to_file(File, File).

rdf_save2(File, Options) :-
    option(encoding(Encoding), Options, utf8),
    valid_encoding(Encoding),
    open_output(File, Encoding, Out, Close),
    flag(rdf_db_saved_subjects, OSavedSubjects, 0),
    flag(rdf_db_saved_triples, OSavedTriples, 0),
    call_cleanup(rdf_do_save(Out, Options),
                 Reason,
                 cleanup_save(Reason,
                              File,
                              OSavedSubjects,
                              OSavedTriples,
                              Close)).

open_output(stream(Out), Encoding, Out, Cleanup) :-
    !,
    stream_property(Out, encoding(Old)),
    (   (   Old == Encoding
        ;   Old == wchar_t          % Internal encoding
        )
    ->  Cleanup = true
    ;   set_stream(Out, encoding(Encoding)),
        Cleanup = set_stream(Out, encoding(Old))
    ).
open_output(File, Encoding, Out,
            close(Out)) :-
    open(File, write, Out, [encoding(Encoding)]).

valid_encoding(Enc) :-
    (   xml_encoding_name(Enc, _)
    ->  true
    ;   throw(error(domain_error(encoding, Enc), _))
    ).


cleanup_save(Reason,
             File,
             OSavedSubjects,
             OSavedTriples,
             Close) :-
    call(Close),
    flag(rdf_db_saved_subjects, SavedSubjects, OSavedSubjects),
    flag(rdf_db_saved_triples, SavedTriples, OSavedTriples),
    retractall(named_anon(_, _)),
    retractall(inlined(_)),
    (   Reason == exit
    ->  print_message(informational,
                      rdf(saved(File, SavedSubjects, SavedTriples)))
    ;   format(user_error, 'Reason = ~w~n', [Reason])
    ).

rdf_do_save(Out, Options0) :-
    rdf_save_header(Out, Options0, Options),
    graph(Options, DB),
    (   option(sorted(true), Options, false)
    ->  (   var(DB)
        ->  setof(Subject, rdf_subject(Subject), Subjects)
        ;   findall(Subject, rdf(Subject, _, _, DB:_), SubjectList),
            sort(SubjectList, Subjects)
        ),
        forall(member(Subject, Subjects),
               rdf_save_non_anon_subject(Out, Subject, Options))
    ;   forall(rdf_subject_in_graph(Subject, DB),
               rdf_save_non_anon_subject(Out, Subject, Options))
    ),
    rdf_save_footer(Out),
    !.                                  % dubious cut; without the
                                        % cleanup handlers isn't called!?

%!  rdf_subject_in_graph(-Subject, ?DB) is nondet.
%
%   True when Subject is a subject in the   graph  DB. If DB is unbound,
%   all  subjects  are  enumerated.  Otherwise   we  have  two  options:
%   enumerate all subjects and filter by graph or collect all triples of
%   the graph and get the unique subjects.   The  first is attractive if
%   the graph is big compared  to  the   DB,  also  because  it does not
%   require memory, the second if the graph is small compared to the DB.

rdf_subject_in_graph(Subject, DB) :-
    var(DB),
    !,
    rdf_subject(Subject).
rdf_subject_in_graph(Subject, DB) :-
    rdf_statistics(triples(AllTriples)),
    rdf_graph_property(DB, triples(DBTriples)),
    DBTriples > AllTriples // 10,
    !,
    rdf_resource(Subject),
    (   rdf(Subject, _, _, DB:_)
    ->  true
    ).
rdf_subject_in_graph(Subject, DB) :-
    findall(Subject, rdf(Subject, _, _, DB:_), SubjectList),
    list_to_set(SubjectList, Subjects),
    member(Subject, Subjects).


graph(Options0, DB) :-
    strip_module(Options0, _, Options),
    (   memberchk(graph(DB0), Options)
    ->  DB = DB0
    ;   memberchk(db(DB0), Options)
    ->  DB = DB0
    ;   true                            % leave unbound
    ).


%!  rdf_save_header(+Fd, +Options)
%
%   Save XML document header, doctype and open the RDF environment.
%   This predicate also sets up the namespace notation.
%
%   Save an RDF header, with the XML header, DOCTYPE, ENTITY and
%   opening the rdf:RDF element with appropriate namespace
%   declarations. It uses the primitives from section 3.5 to
%   generate the required namespaces and desired short-name. Options
%   is one of:
%
%     * graph(+URI)
%     Only search for namespaces used in triples that belong to the
%     given named graph.
%
%     * namespaces(+List)
%     Where List is a list of namespace abbreviations. With this
%     option, the expensive search for all namespaces that may be
%     used by your data is omitted. The namespaces =rdf= and =rdfs=
%     are added to the provided List. If a namespace is not
%     declared, the resource is emitted in non-abreviated form.

rdf_save_header(Out, Options) :-
    rdf_save_header(Out, Options, _).

rdf_save_header(Out, Options, OptionsOut) :-
    is_list(Options),
    !,
    option(encoding(Enc), Options, utf8),
    xml_encoding(Enc, Encoding),
    format(Out, '<?xml version=\'1.0\' encoding=\'~w\'?>~n', [Encoding]),
    format(Out, '<!DOCTYPE rdf:RDF [', []),
    header_namespaces(Options, NSIdList),
    nsmap(NSIdList, NsMap),
    append(Options, [nsmap(NsMap)], OptionsOut),
    forall(member(Id=URI, NsMap),
           (   xml_quote_attribute(URI, NSText0, Enc),
               xml_escape_parameter_entity(NSText0, NSText),
               format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NSText])
           )),
    format(Out, '~N]>~n~n', []),
    format(Out, '<rdf:RDF', []),
    (   member(Id, NSIdList),
        format(Out, '~N    xmlns:~w="&~w;"~n', [Id, Id]),
        fail
    ;   true
    ),
    (   option(base_uri(Base), Options),
        option(write_xml_base(true), Options, true)
    ->  xml_quote_attribute(Base, BaseText, Enc),
        format(Out, '~N    xml:base="~w"~n', [BaseText])
    ;   true
    ),
    (   memberchk(document_language(Lang), Options)
    ->  format(Out, '~N    xml:lang="~w"', [Lang])
    ;   true
    ),
    format(Out, '>~n', []).
rdf_save_header(Out, FileRef, OptionsOut) :-    % compatibility
    atom(FileRef),
    rdf_save_header(Out, [graph(FileRef)], OptionsOut).

xml_encoding(Enc, Encoding) :-
    (   xml_encoding_name(Enc, Encoding)
    ->  true
    ;   throw(error(domain_error(rdf_encoding, Enc), _))
    ).

xml_encoding_name(ascii,       'US-ASCII').
xml_encoding_name(iso_latin_1, 'ISO-8859-1').
xml_encoding_name(utf8,        'UTF-8').

%!  nsmap(+NSIds, -Map:list(id=uri)) is det.
%
%   Create a namespace-map that is compatible to xml_write/2
%   for dealing with XML-Literals

nsmap([], []).
nsmap([Id|T0], [Id=URI|T]) :-
    ns(Id, URI),
    nsmap(T0, T).

%!  xml_escape_parameter_entity(+In, -Out) is det.
%
%   Escape % as &#37; for entity declarations.

xml_escape_parameter_entity(In, Out) :-
    sub_atom(In, _, _, _, '%'),
    !,
    atom_codes(In, Codes),
    phrase(escape_parent(Codes), OutCodes),
    atom_codes(Out, OutCodes).
xml_escape_parameter_entity(In, In).

escape_parent([]) --> [].
escape_parent([H|T]) -->
    (   { H == 37 }
    ->  "&#37;"
    ;   [H]
    ),
    escape_parent(T).


%!  header_namespaces(Options, -List)
%
%   Get namespaces we will define as entities

header_namespaces(Options, List) :-
    memberchk(namespaces(NSL0), Options),
    !,
    sort([rdf,rdfs|NSL0], List).
header_namespaces(Options, List) :-
    graph(Options, DB),
    used_namespace_entities(List, DB).

%!  rdf_graph_prefixes(?Graph, -List:ord_set) is det.
%!  rdf_graph_prefixes(?Graph, -List:ord_set, :Options) is det.
%
%   List is a sorted list of  prefixes (namepaces) in Graph. Options
%   defined are:
%
%       * filter(:Filter)
%       optional Filter argument is used to filter the results. It
%       is called with 3 additional arguments:
%
%           ==
%           call(Filter, Where, Prefix, URI)
%           ==
%
%       The Where argument gives the location of the prefix ans is
%       one of =subject=, =predicate=, =object= or =type=. The
%       Prefix argument is the potentionally new prefix and URI is
%       the full URI that is being processed.
%
%       * expand(:Goal)
%       Hook to generate the graph.  Called using
%
%           ==
%           call(Goal,S,P,O,Graph)
%           ==
%
%       * min_count(+Count)
%       Only include prefixes that appear at least N times.  Default
%       is 1. Declared prefixes are always returned if found at
%       least one time.
%
%       * get_prefix(:GetPrefix)
%       Predicate to extract the candidate prefix from an IRI.  Default
%       is iri_xml_namespace/2.


:- thread_local
    graph_prefix/3.
:- meta_predicate
    rdf_graph_prefixes(?, -, :).

rdf_graph_prefixes(Graph, List) :-
    rdf_graph_prefixes(Graph, List, []).

rdf_graph_prefixes(Graph, List, M:QOptions) :-
    is_list(QOptions),
    !,
    meta_options(is_meta, M:QOptions, Options),
    option(filter(Filter), Options, true),
    option(expand(Expand), Options, rdf_db),
    option(min_count(MinCount), Options, 1),
    option(get_prefix(GetPrefix), Options, iri_xml_namespace),
    call_cleanup(prefixes(Expand, Graph, Prefixes, Filter, MinCount, GetPrefix),
                 retractall(graph_prefix(_,_,_))),
    sort(Prefixes, List).
rdf_graph_prefixes(Graph, List, M:Filter) :-
    rdf_graph_prefixes(Graph, List, M:[filter(Filter)]).

is_meta(filter).
is_meta(expand).
is_meta(get_prefix).


prefixes(Expand, Graph, Prefixes, Filter, MinCount, GetPrefix) :-
    (   call(Expand, S, P, O, Graph),
        add_ns(subject, GetPrefix, Filter, S, MinCount, s(S)),
        add_ns(predicate, GetPrefix, Filter, P, MinCount, sp(S,P)),
        add_ns_obj(GetPrefix, Filter, O, MinCount, spo(S,P,O)),
        fail
    ;   true
    ),
    findall(Prefix, graph_prefix(Prefix, MinCount, _), Prefixes).

add_ns(Where, GetPrefix, Filter, S, MinCount, Context) :-
    \+ rdf_is_bnode(S),
    call(GetPrefix, S, Full),
    Full \== '',
    !,
    (   graph_prefix(Full, MinCount, _)
    ->  true
    ;   Filter == true
    ->  add_ns(Full, Context)
    ;   call(Filter, Where, Full, S)
    ->  add_ns(Full, Context)
    ;   true
    ).
add_ns(_, _, _, _, _, _).

add_ns(Full, Context) :-
    graph_prefix(Full, _, Contexts),
    memberchk(Context, Contexts),
    !.
add_ns(Full, Context) :-
    retract(graph_prefix(Full, C0, Contexts)),
    !,
    C1 is C0+1,
    asserta(graph_prefix(Full, C1, [Context|Contexts])).
add_ns(Full, _) :-
    ns(_, Full),
    !,
    asserta(graph_prefix(Full, _, _)).
add_ns(Full, Context) :-
    asserta(graph_prefix(Full, 1, [Context])).


add_ns_obj(GetPrefix, Filter, O, MinCount, Context) :-
    atom(O),
    !,
    add_ns(object, GetPrefix, Filter, O, MinCount, Context).
add_ns_obj(GetPrefix, Filter, literal(type(Type, _)), MinCount, _) :-
    atom(Type),
    !,
    add_ns(type, GetPrefix, Filter, Type, MinCount, t(Type)).
add_ns_obj(_, _, _, _, _).


%!  used_namespace_entities(-List, ?Graph) is det.
%
%   Return the namespace aliases that are actually used in Graph. In
%   addition, this predicate creates ns<N>   aliases  for namespaces
%   used in predicates because RDF/XML cannot write predicates other
%   than as an XML name.

used_namespace_entities(List, Graph) :-
    decl_used_predicate_ns(Graph),
    used_namespaces(List, Graph).

used_namespaces(List, DB) :-
    rdf_graph_prefixes(DB, FullList),
    ns_abbreviations(FullList, List0),
    sort([rdf|List0], List).

ns_abbreviations([], []).
ns_abbreviations([H0|T0], [H|T]) :-
    ns(H, H0),
    !,
    ns_abbreviations(T0, T).
ns_abbreviations([_|T0], T) :-
    ns_abbreviations(T0, T).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For every URL used as a predicate  we   *MUST*  define a namespace as we
cannot use names holding /, :, etc. as XML identifiers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
    predicate_ns/2.

decl_used_predicate_ns(DB) :-
    retractall(predicate_ns(_,_)),
    (   rdf_current_predicate(P, DB),
        decl_predicate_ns(P),
        fail
    ;   true
    ).

decl_predicate_ns(Pred) :-
    predicate_ns(Pred, _),
    !.
decl_predicate_ns(Pred) :-
    rdf_global_id(NS:Local, Pred),
    xml_name(Local),
    !,
    assert(predicate_ns(Pred, NS)).
decl_predicate_ns(Pred) :-
    atom_codes(Pred, Codes),
    append(NSCodes, LocalCodes, Codes),
    xml_codes(LocalCodes),
    !,
    (   NSCodes \== []
    ->  atom_codes(NS, NSCodes),
        (   ns(Id, NS)
        ->  assert(predicate_ns(Pred, Id))
        ;   between(1, infinite, N),
            atom_concat(ns, N, Id),
            \+ ns(Id, _)
        ->  rdf_register_ns(Id, NS),
            print_message(informational,
                          rdf(using_namespace(Id, NS)))
        ),
        assert(predicate_ns(Pred, Id))
    ;   assert(predicate_ns(Pred, -)) % no namespace used
    ).

xml_codes([]).
xml_codes([H|T]) :-
    xml_code(H),
    xml_codes(T).

xml_code(X) :-
    code_type(X, csym),
    !.
xml_code(0'-).                          % Match 0'-


%!  rdf_save_footer(Out:stream) is det.
%
%   Finish XML generation and write the document footer.
%
%   @see rdf_save_header/2, rdf_save_subject/3.

rdf_save_footer(Out) :-
    retractall(named_anon(_, _)),
    retractall(inlined(_)),
    format(Out, '</rdf:RDF>~n', []).

%!  rdf_save_non_anon_subject(+Out, +Subject, +Options)
%
%   Save an object.  Anonymous objects not saved if anon(false)
%   is present in the Options list.

rdf_save_non_anon_subject(_Out, Subject, Options) :-
    rdf_is_bnode(Subject),
    (   memberchk(anon(false), Options)
    ;   graph(Options, DB),
        rdf_db(_, _, Subject, DB)
    ),
    !.
rdf_save_non_anon_subject(Out, Subject, Options) :-
    rdf_save_subject(Out, Subject, Options),
    flag(rdf_db_saved_subjects, X, X+1).


%!  rdf_save_subject(+Out, +Subject:resource, +Options) is det.
%
%   Save the triples associated to Subject to Out. Options:
%
%     * graph(+Graph)
%     Only save properties from Graph.
%     * base_uri(+URI)
%     * convert_typed_literal(:Goal)
%     * document_language(+XMLLang)
%
%   @see rdf_save/2 for a description of these options.

rdf_save_subject(Out, Subject, Options) :-
    is_list(Options),
    !,
    option(base_uri(BaseURI), Options, '-'),
    (   rdf_save_subject(Out, Subject, BaseURI, 0, Options)
    ->  format(Out, '~n', [])
    ;   throw(error(rdf_save_failed(Subject), 'Internal error'))
    ).
rdf_save_subject(Out, Subject, DB) :-
    (   var(DB)
    ->  rdf_save_subject(Out, Subject, [])
    ;   rdf_save_subject(Out, Subject, [graph(DB)])
    ).


%!  rdf_save_subject(+Out:stream, +Subject:resource, +BaseURI,
%!                   +Indent:int, +Options) is det.
%
%   Save properties of Subject.
%
%   @param Indent   Current indentation

rdf_save_subject(_, Subject, _, _, _) :-
    inlined(Subject),
    !.
rdf_save_subject(Out, Subject, BaseURI, Indent, Options) :-
    do_save_subject(Out, Subject, BaseURI, Indent, Options).

do_save_subject(Out, Subject, BaseURI, Indent, Options) :-
    graph(Options, DB),
    findall(Pred=Object, rdf_db(Subject, Pred, Object, DB), Atts0),
    sort(Atts0, Atts),              % remove duplicates
    length(Atts, L),
    (   length(Atts0, L0),
        Del is L0-L,
        Del > 0
    ->  print_message(informational,
                      rdf(save_removed_duplicates(Del, Subject)))
    ;   true
    ),
    rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options),
    flag(rdf_db_saved_triples, X, X+L).

rdf_db(Subject, Pred, Object, DB) :-
    var(DB),
    !,
    rdf(Subject, Pred, Object).
rdf_db(Subject, Pred, Object, DB) :-
    rdf(Subject, Pred, Object, DB:_).

%!  rdf_save_subject(+Out:stream, +Subject:resource, +BaseURI,
%!                   +Atts:list(Pred=Obj), +Indent:int, +Options) is det.
%
%   Save triples defined by Atts on Subject.

rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options) :-
    rdf_equal(rdf:type, RdfType),
    select(RdfType=Type, Atts, Atts1),
    \+ rdf_is_bnode(Type),
    rdf_id(Type, BaseURI, TypeId),
    xml_is_name(TypeId),
    !,
    format(Out, '~*|<', [Indent]),
    rdf_write_id(Out, TypeId),
    save_about(Out, BaseURI, Subject, Options),
    save_attributes(Atts1, BaseURI, Out, TypeId, Indent, Options).
rdf_save_subject(Out, Subject, BaseURI, Atts, Indent, Options) :-
    format(Out, '~*|<rdf:Description', [Indent]),
    save_about(Out, BaseURI, Subject, Options),
    save_attributes(Atts, BaseURI, Out, rdf:'Description', Indent, Options).

xml_is_name(_NS:Atom) :-
    !,
    xml_name(Atom).
xml_is_name(Atom) :-
    xml_name(Atom).

%!  save_about(+Out, +BaseURI, +Subject, +Options) is det.
%
%   Save the rdf:about. If Subject is a  blank node, save the nodeID
%   if any.

save_about(Out, _BaseURI, Subject, _Options) :-
    rdf_is_bnode(Subject),
    !,
    (   named_anon(Subject, NodeID)
    ->  format(Out, ' rdf:nodeID="~w"', [NodeID])
    ;   true
    ).
save_about(Out, BaseURI, Subject, Options) :-
    option(encoding(Encoding), Options, utf8),
    rdf_value(Subject, BaseURI, QSubject, Encoding),
    format(Out, ' rdf:about="~w"', [QSubject]).

%!  save_attributes(+List, +BaseURI, +Stream, +Element, +Indent, +Options)
%
%   Save the attributes.  Short literal attributes are saved in the
%   tag.  Others as the content of the description element.  The
%   begin tag has already been filled.

save_attributes(Atts, BaseURI, Out, Element, Indent, Options) :-
    split_attributes(Atts, InTag, InBody, Options),
    SubIndent is Indent + 2,
    save_attributes2(InTag, BaseURI, tag, Out, SubIndent, Options),
    (   InBody == []
    ->  format(Out, '/>~n', [])
    ;   format(Out, '>~n', []),
        save_attributes2(InBody, BaseURI, body, Out, SubIndent, Options),
        format(Out, '~N~*|</', [Indent]),
        rdf_write_id(Out, Element),
        format(Out, '>~n', [])
    ).

%!  split_attributes(+Attributes, -HeadAttrs, -BodyAttr, Options)
%
%   Split attribute (Name=Value) list into attributes for the head
%   and body. Attributes can only be in the head if they are literal
%   and appear only one time in the attribute list.

split_attributes(Atts, [], Atts, Options) :-
    option(xml_attributes(false), Options),
    !.
split_attributes(Atts, HeadAttr, BodyAttr, _) :-
    duplicate_attributes(Atts, Dupls, Singles),
    simple_literal_attributes(Singles, HeadAttr, Rest),
    append(Dupls, Rest, BodyAttr).

%!  duplicate_attributes(+Attrs, -Duplicates, -Singles)
%
%   Extract attributes that appear more than onces as we cannot
%   dublicate an attribute in the head according to the XML rules.

duplicate_attributes([], [], []).
duplicate_attributes([H|T], Dupls, Singles) :-
    H = (Name=_),
    named_attributes(Name, T, D, R),
    D \== [],
    append([H|D], Dupls2, Dupls),
    !,
    duplicate_attributes(R, Dupls2, Singles).
duplicate_attributes([H|T], Dupls2, [H|Singles]) :-
    duplicate_attributes(T, Dupls2, Singles).

named_attributes(_, [], [], []) :- !.
named_attributes(Name, [H|T], D, R) :-
    (   H = (Name=_)
    ->  D = [H|DT],
        named_attributes(Name, T, DT, R)
    ;   R = [H|RT],
        named_attributes(Name, T, D, RT)
    ).

%!  simple_literal_attributes(+Attributes, -Inline, -Body)
%
%   Split attributes for (literal) attributes to be used in the
%   begin-tag and ones that have to go into the body of the description.

simple_literal_attributes([], [], []).
simple_literal_attributes([H|TA], [H|TI], B) :-
    in_tag_attribute(H),
    !,
    simple_literal_attributes(TA, TI, B).
simple_literal_attributes([H|TA], I, [H|TB]) :-
    simple_literal_attributes(TA, I, TB).

in_tag_attribute(_=literal(Text)) :-
    atom(Text),                     % may not have lang qualifier
    atom_length(Text, Len),
    Len < 60.

%!  save_attributes2(+List, +BaseURI, +TagOrBody, +Stream, +Indent, +Options)
%
%   Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], BaseURI, Where, Out, Indent, Options) :-
    save_attribute(Where, H, BaseURI, Out, Indent, Options),
    save_attributes2(T, BaseURI, Where, Out, Indent, Options).

save_attribute(tag, Name=literal(Value), BaseURI, Out, Indent, Options) :-
    AttIndent is Indent + 2,
    rdf_id(Name, BaseURI, NameText),
    option(encoding(Encoding), Options, utf8),
    xml_quote_attribute(Value, QVal, Encoding),
    format(Out, '~N~*|', [AttIndent]),
    rdf_write_id(Out, NameText),
    format(Out, '="~w"', [QVal]).
save_attribute(body, Name=literal(Literal0), BaseURI, Out, Indent, Options) :-
    !,
    rdf_id(Name, BaseURI, NameText),
    (   memberchk(convert_typed_literal(Converter), Options),
        call(Converter, Type, Content, Literal0)
    ->  Literal = type(Type, Content)
    ;   Literal = Literal0
    ),
    save_body_literal(Literal, NameText, BaseURI, Out, Indent, Options).
save_attribute(body, Name=Value, BaseURI, Out, Indent, Options) :-
    rdf_is_bnode(Value),
    !,
    rdf_id(Name, BaseURI, NameText),
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    (   named_anon(Value, NodeID)
    ->  format(Out, ' rdf:nodeID="~w"/>', [NodeID])
    ;   (   rdf(S1, Name, Value),
            rdf(S2, P2, Value),
            (S1 \== S2 ; Name \== P2)
        ->  predicate_property(named_anon(_,_), number_of_clauses(N)),
            atom_concat('bn', N, NodeID),
            assertz(named_anon(Value, NodeID))
        ;   true
        ),
        SubIndent is Indent + 2,
        (   rdf_collection(Value)
        ->  save_about(Out, BaseURI, Value, Options),
            format(Out, ' rdf:parseType="Collection">~n', []),
            rdf_save_list(Out, Value, BaseURI, SubIndent, Options)
        ;   format(Out, '>~n', []),
            rdf_save_subject(Out, Value, BaseURI, SubIndent, Options)
        ),
        format(Out, '~N~*|</', [Indent]),
        rdf_write_id(Out, NameText),
        format(Out, '>~n', [])
    ).
save_attribute(body, Name=Value, BaseURI, Out, Indent, Options) :-
    option(inline(true), Options),
    has_attributes(Value, Options),
    \+ inlined(Value),
    !,
    assertz(inlined(Value)),
    rdf_id(Name, BaseURI, NameText),
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    SubIndent is Indent + 2,
    (   rdf_collection(Value)
    ->  save_about(Out, BaseURI, Value, Options),
        format(Out, ' rdf:parseType="Collection">~n', []),
        rdf_save_list(Out, Value, BaseURI, SubIndent, Options)
    ;   format(Out, '>~n', []),
        do_save_subject(Out, Value, BaseURI, SubIndent, Options)
    ),
    format(Out, '~N~*|</', [Indent]),
    rdf_write_id(Out, NameText),
    format(Out, '>~n', []).
save_attribute(body, Name=Value, BaseURI, Out, Indent, Options) :-
    option(encoding(Encoding), Options, utf8),
    rdf_value(Value, BaseURI, QVal, Encoding),
    rdf_id(Name, BaseURI, NameText),
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    format(Out, ' rdf:resource="~w"/>', [QVal]).

has_attributes(URI, Options) :-
    graph(Options, DB),
    rdf_db(URI, _, _, DB),
    !.

%!  save_body_literal(+Literal, +NameText, +BaseURI,
%!                    +Out, +Indent, +Options).

save_body_literal(lang(Lang, Value),
                  NameText, BaseURI, Out, Indent, Options) :-
    !,
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    (   memberchk(document_language(Lang), Options)
    ->  write(Out, '>')
    ;   rdf_id(Lang, BaseURI, LangText),
        format(Out, ' xml:lang="~w">', [LangText])
    ),
    save_attribute_value(Value, Out, Options),
    write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(type(Type, DOM),
                  NameText, _BaseURI, Out, Indent, Options) :-
    rdf_equal(Type, rdf:'XMLLiteral'),
    !,
    (   atom(DOM)
    ->  format(Out, '~N~*|<', [Indent]),
        rdf_write_id(Out, NameText),
        format(Out, ' rdf:parseType="Literal">~w</', [DOM]),
        rdf_write_id(Out, NameText), write(Out, '>')
    ;   save_xml_literal(DOM, NameText, Out, Indent, Options)
    ).
save_body_literal(type(Type, Value),
                  NameText, BaseURI, Out, Indent, Options) :-
    !,
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    option(encoding(Encoding), Options, utf8),
    rdf_value(Type, BaseURI, QVal, Encoding),
    format(Out, ' rdf:datatype="~w">', [QVal]),
    save_attribute_value(Value, Out, Options),
    write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(Literal,
                  NameText, _, Out, Indent, Options) :-
    atomic(Literal),
    !,
    format(Out, '~N~*|<', [Indent]),
    rdf_write_id(Out, NameText),
    write(Out, '>'),
    save_attribute_value(Literal, Out, Options),
    write(Out, '</'), rdf_write_id(Out, NameText), write(Out, '>').
save_body_literal(DOM,
                  NameText, BaseURI, Out, Indent, Options) :-
    rdf_equal(Type, rdf:'XMLLiteral'),
    save_body_literal(type(Type, DOM),
                      NameText, BaseURI, Out, Indent, Options).

save_attribute_value(Value, Out, Options) :-  % strings
    (	atom(Value)
    ;	string(Value)
    ),
    !,
    option(encoding(Encoding), Options, utf8),
    xml_quote_cdata(Value, QVal, Encoding),
    write(Out, QVal).
save_attribute_value(Value, Out, _Options) :-  % numbers
    number(Value),
    !,
    writeq(Out, Value).             % quoted: preserve floats
save_attribute_value(Value, _Out, _Options) :-
    throw(error(save_attribute_value(Value), _)).

%!  save_xml_literal(+DOM, +Attr, +Out, +Indent, +Options) is det.
%
%   Save an XMLLiteral value. We already emitted
%
%           ==
%           <prop parseType="literal"
%           ==
%
%   but  not  the  terminating  =|>|=.  We  need  to  establish  the
%   namespaces used in the DOM. The   namespaces in the rdf document
%   are in the nsmap-option of Options.

save_xml_literal(DOM, Attr, Out, Indent, Options) :-
    xml_is_dom(DOM),
    !,
    memberchk(nsmap(NsMap), Options),
    id_to_atom(Attr, Atom),
    xml_write(Out,
              element(Atom, ['rdf:parseType'='Literal'], DOM),
              [ header(false),
                indent(Indent),
                nsmap(NsMap)
              ]).
save_xml_literal(NoDOM, _, _, _, _) :-
    must_be(xml_dom, NoDOM).

id_to_atom(NS:Local, Atom) :-
    !,
    atomic_list_concat([NS,Local], :, Atom).
id_to_atom(ID, ID).


%!  rdf_collection(+URI) is semidet.
%
%   True  if  URI  represents  an  RDF    list  that  fits  the  RDF
%   parseType=collection syntax. This means it is   a linked list of
%   bnode-cells with a rdf:first that is   a  resource, optionally a
%   rdf:type that is an rdf:list and the list ends in an rdf:nil.

:- rdf_meta
    rdf_collection(r),
    collection_p(r,r).

rdf_collection(rdf:nil) :- !.
rdf_collection(Cell) :-
    rdf_is_bnode(Cell),
    findall(F, rdf(Cell, rdf:first, F), [_]),
    findall(F, rdf(Cell, rdf:rest, F), [Rest]),
    forall(rdf(Cell, P, V),
           collection_p(P, V)),
    rdf_collection(Rest).

collection_p(rdf:first, V) :- atom(V).
collection_p(rdf:rest, _).
collection_p(rdf:type, rdf:'List').


%!  rdf_save_list(+Out, +List, +BaseURI, +Indent, +Options)

rdf_save_list(_, List, _, _, _) :-
    rdf_equal(List, rdf:nil),
    !.
rdf_save_list(Out, List, BaseURI, Indent, Options) :-
    rdf_has(List, rdf:first, First),
    (   rdf_is_bnode(First)
    ->  nl(Out),
        rdf_save_subject(Out, First, BaseURI, Indent, Options)
    ;   option(encoding(Encoding), Options, utf8),
        rdf_value(First, BaseURI, QVal, Encoding),
        format(Out, '~N~*|<rdf:Description rdf:about="~w"/>',
               [Indent, QVal])
    ),
    flag(rdf_db_saved_triples, X, X+3),
    (   rdf_has(List, rdf:rest, List2),
        \+ rdf_equal(List2, rdf:nil)
    ->  rdf_save_list(Out, List2, BaseURI, Indent, Options)
    ;   true
    ).


%!  rdf_id(+Resource, +BaseURI, -NSLocal)
%
%   Generate a NS:Local  name  for   Resource  given  the  indicated
%   default namespace. This call is used for elements.

rdf_id(Id, BaseURI, Local) :-
    assertion(atom(BaseURI)),
    atom_concat(BaseURI, Local, Id),
    sub_atom(Local, 0, 1, _, #),
    !.
rdf_id(Id, _, NS:Local) :-
    iri_xml_namespace(Id, Full, Local),
    ns(NS, Full),
    !.
rdf_id(Id, _, NS:Local) :-
    ns(NS, Full),
    Full \== '',
    atom_concat(Full, Local, Id),
    !.
rdf_id(Id, _, Id).


%!  rdf_write_id(+Out, +NSLocal) is det.
%
%   Write an identifier. We cannot use native write on it as both NS
%   and Local can be operators.

rdf_write_id(Out, NS:Local) :-
    !,
    format(Out, '~w:~w', [NS, Local]).
rdf_write_id(Out, Atom) :-
    write(Out, Atom).

%!  rdf_value(+Resource, +BaseURI, -Text, +Encoding)
%
%   According  to  "6.4  RDF  URI  References"  of  the  RDF  Syntax
%   specification, a URI reference is  UNICODE string not containing
%   control sequences, represented as  UTF-8   and  then  as escaped
%   US-ASCII.

rdf_value(Base, Base, '', _) :- !.
rdf_value(V, Base, Text, Encoding) :-
    atom_concat(Base, Local, V),
    sub_atom(Local, 0, _, _, #),
    !,
    xml_quote_attribute(Local, Text, Encoding).
rdf_value(V, _, Text, Encoding) :-
    ns(NS, Full),
    atom_concat(Full, Local, V),
    xml_is_name(Local),
    !,
    xml_quote_attribute(Local, QLocal, Encoding),
    atomic_list_concat(['&', NS, (';'), QLocal], Text).
rdf_value(V, _, Q, Encoding) :-
    xml_quote_attribute(V, Q, Encoding).


                 /*******************************
                 *       MATCH AND COMPARE      *
                 *******************************/

%!  rdf_compare(-Dif, +Object1, +Object2) is det.
%
%   Compare  two  object  terms.  Where  SPARQL  defines  a  partial
%   ordering, we define a complete ordering   of terms. The ordering
%   is defines as:
%
%     - Blank nodes < IRIs < Literals
%     - Numeric literals < other literals
%     - Numeric literals are compared by value and then by type,
%       where Integer < Decimal < Double
%     - Other literals are compare lexically, case insensitive.
%       If equal, uppercase preceeds lowercase.  If still equal,
%       the types are compared lexically.

%!  rdf_match_label(+How, +Pattern, +Label) is semidet.
%
%   True if Label matches Pattern according to   How.  How is one of
%   `icase`, `substring`, `word`, `prefix` or   `like`. For backward
%   compatibility, `exact` is a synonym for `icase`.


                 /*******************************
                 *      DEPRECATED MATERIAL     *
                 *******************************/

%!  rdf_split_url(+Prefix, +Local, -URL) is det.
%!  rdf_split_url(-Prefix, -Local, +URL) is det.
%
%   Split/join a URL.  This functionality is moved to library(sgml).
%
%   @deprecated Use iri_xml_namespace/3. Note that the argument
%   order is iri_xml_namespace(+IRI, -Namespace, -Localname).

rdf_split_url(Prefix, Local, URL) :-
    atomic(URL),
    !,
    iri_xml_namespace(URL, Prefix, Local).
rdf_split_url(Prefix, Local, URL) :-
    atom_concat(Prefix, Local, URL).

%!  rdf_url_namespace(+URL, -Namespace)
%
%   Namespace is the namespace of URL.
%
%   @deprecated Use iri_xml_namespace/2

rdf_url_namespace(URL, Prefix) :-
    iri_xml_namespace(URL, Prefix).


                 /*******************************
                 *            LITERALS          *
                 *******************************/

%!  rdf_new_literal_map(-Map) is det.
%
%   Create a new literal map, returning an opaque handle.

%!  rdf_destroy_literal_map(+Map) is det.
%
%   Destroy a literal map. After this call,   further use of the Map
%   handle is illegal. Additional synchronisation  is needed if maps
%   that are shared between threads are   destroyed to guarantee the
%   handle    is    no    longer    used.    In    some    scenarios
%   rdf_reset_literal_map/1 provides a safe alternative.

%!  rdf_reset_literal_map(+Map) is det.
%
%   Delete all content from the literal map.

%!  rdf_insert_literal_map(+Map, +Key, +Value) is det.
%
%   Add a relation between  Key  and  Value   to  the  map.  If this
%   relation already exists no action is performed.

%!  rdf_insert_literal_map(+Map, +Key, +Value, -KeyCount) is det.
%
%   As rdf_insert_literal_map/3. In addition, if Key is a new key in
%   Map, unify KeyCount with the number of  keys in Map. This serves
%   two purposes. Derived maps, such as  the stem and metaphone maps
%   need to know about new  keys   and  it avoids additional foreign
%   calls for doing the progress in rdf_litindex.pl.

%!  rdf_delete_literal_map(+Map, +Key) is det.
%
%   Delete Key and all associated values from the map.

%!  rdf_delete_literal_map(+Map, +Key, +Value) is det.
%
%   Delete the association between Key and Value from the map.

%!  rdf_find_literal_map(+Map, +KeyList, -ValueList) is det.
%
%   Unify ValueList with an ordered set  of values associated to all
%   keys from KeyList. Each key in  KeyList   is  either an atom, an
%   integer or a term not(Key).  If   not-terms  are provided, there
%   must be at least one positive keywords. The negations are tested
%   after establishing the positive matches.

%!  rdf_keys_in_literal_map(+Map, +Spec, -Answer) is det.
%
%   Realises various queries on the key-set:
%
%     * all
%
%     Unify Answer with an ordered list of all keys.
%     * key(+Key)
%
%     Succeeds if Key is a key in the map and unify Answer with the
%     number of values associated with the key. This provides a fast
%     test of existence without fetching the possibly large
%     associated value set as with rdf_find_literal_map/3.
%
%     * prefix(+Prefix)
%     Unify Answer with an ordered set of all keys that have the
%     given prefix. See section 3.1 for details on prefix matching.
%     Prefix must be an atom. This call is intended for
%     auto-completion in user interfaces.
%
%     * ge(+Min)
%     Unify Answer with all keys that are larger or equal to the
%     integer Min.
%
%     * le(+Max)
%     Unify Answer with all keys that are smaller or equal to the integer
%     Max.
%
%     * between(+Min, +Max) Unify
%     Answer with all keys between Min and Max (including).

%!  rdf_statistics_literal_map(+Map, -KeyValue)
%
%   Query some statistics of the map. Provides KeyValue are:
%
%     * size(-Keys, -Relations)
%     Unify Keys with the total key-count of the index and Relation
%     with the total Key-Value count.



                 /*******************************
                 *             MISC             *
                 *******************************/

%!  rdf_version(-Version) is det.
%
%   True when Version is the numerical version-id of this library.
%   The version is computed as
%
%           Major*10000 + Minor*100 + Patch.

%!  rdf_set(+Term) is det.
%
%   Set properties of the RDF store.  Currently defines:
%
%     * hash(+Hash, +Parameter, +Value)
%     Set properties for a triple index.  Hash is one of =s=,
%     =p=, =sp=, =o=, =po=, =spo=, =g=, =sg= or =pg=.  Parameter
%     is one of:
%
%       - size
%       Value defines the number of entries in the hash-table.
%       Value is rounded _down_ to a power of 2.  After setting
%       the size explicitly, auto-sizing for this table is
%       disabled.  Setting the size smaller than the current
%       size results in a =permission_error= exception.
%
%       - average_chain_len
%       Set maximum average collision number for the hash.
%
%       - optimize_threshold
%       Related to resizing hash-tables.  If 0, all triples are
%       moved to the new size by the garbage collector.  If more
%       then zero, those of the last Value resize steps remain at
%       their current location.  Leaving cells at their current
%       location reduces memory fragmentation and slows down
%       access.

%!  rdf_md5(+Graph, -MD5) is det.
%
%   True when MD5 is the MD5 hash for  all triples in graph. The MD5
%   digest itself is represented as an   atom holding a 32-character
%   hexadecimal   string.   The   library   maintains   the   digest
%   incrementally on rdf_load/[1,2], rdf_load_db/1, rdf_assert/[3,4]
%   and  rdf_retractall/[3,4].  Checking  whether   the  digest  has
%   changed since the last rdf_load/[1,2]  call provides a practical
%   means for checking whether the file needs to be saved.
%
%   @deprecated New code should use rdf_graph_property(Graph,
%   hash(Hash)).

%!  rdf_generation(-Generation) is det.
%
%   True when Generation is the current  generation of the database.
%   Each modification to the database  increments the generation. It
%   can be used to check the validity of cached results deduced from
%   the database. Committing a non-empty  transaction increments the
%   generation by one.
%
%   When inside a transaction,  Generation  is   unified  to  a term
%   _TransactionStartGen_ + _InsideTransactionGen_. E.g.,  4+3 means
%   that the transaction was started at   generation 4 of the global
%   database and we have  created  3   new  generations  inside  the
%   transaction. Note that this choice  of representation allows for
%   comparing  generations  using  Prolog  arithmetic.  Comparing  a
%   generation in one  transaction  with   a  generation  in another
%   transaction is meaningless.

%!  rdf_estimate_complexity(?Subject, ?Predicate, ?Object, -Complexity)
%
%   Return the number of alternatives as   indicated by the database
%   internal hashed indexing. This is a rough measure for the number
%   of alternatives we can expect for   an  rdf_has/3 call using the
%   given three arguments. When  called   with  three variables, the
%   total number of triples is returned.   This  estimate is used in
%   query  optimisation.  See  also    rdf_predicate_property/2  and
%   rdf_statistics/1 for additional information to help optimizers.

%!  rdf_debug(+Level) is det.
%
%   Set debugging to Level.  Level is an integer 0..9.  Default is
%   0 no debugging.

%!  rdf_atom_md5(+Text, +Times, -MD5) is det.
%
%   Computes the MD5 hash from Text, which is an atom, string or list of
%   character codes. Times is  an  integer  >=   1.  When  >  0, the MD5
%   algorithm is repeated Times times on the generated hash. This can be
%   used for password encryption algorithms   to  make generate-and-test
%   loops slow.
%
%   @deprecated Obviously, password hash  primitives   do  not belong in
%   this library. The  library(crypto)  from   the  \const{ssl}  package
%   provides extensive support for  hashes.   The  \const{clib}  package
%   provides library(crypt) to  access  the   OS  (Unix)  password  hash
%   implementation as well as  lightweight   implementations  of several
%   popular hashes.


                 /*******************************
                 *             MESSAGES         *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(rdf(Term)) -->
    message(Term).

message(loaded(How, What, BaseURI, Triples, Time)) -->
    how(How),
    source(What),
    into(What, BaseURI),
    in_time(Triples, Time).
message(save_removed_duplicates(N, Subject)) -->
    [ 'Removed ~d duplicate triples about "~p"'-[N,Subject] ].
message(saved(File, SavedSubjects, SavedTriples)) -->
    [ 'Saved ~D triples about ~D subjects into ~p'-
      [SavedTriples, SavedSubjects, File]
    ].
message(using_namespace(Id, NS)) -->
    [ 'Using namespace id ~w for ~w'-[Id, NS] ].
message(inconsistent_cache(DB, Graphs)) -->
    [ 'RDF cache file for ~w contains the following graphs'-[DB], nl,
      '~t~8|~p'-[Graphs]
    ].
message(guess_format(Ext)) -->
    [ 'Unknown file-extension: ~w.  Assuming RDF/XML'-[Ext] ].
message(meta(not_expanded(G))) -->
    [ 'rdf_meta/1: ~p is not expanded'-[G] ].
message(deprecated(rdf_unload(Graph))) -->
    [ 'rdf_unload/1: Use ~q'-[rdf_unload_graph(Graph)] ].


how(load)   --> [ 'Loaded' ].
how(parsed) --> [ 'Parsed' ].

source(SourceURL) -->
    { uri_file_name(SourceURL, File),
      !,
      file_base_name(File, Base)    % TBD: relative file?
    },
    [ ' "~w"'-[Base] ].
source(SourceURL) -->
    [ ' "~w"'-[SourceURL] ].

into(_, _) --> [].                      % TBD

in_time(Triples, ParseTime) -->
    [ ' in ~2f sec; ~D triples'-[ParseTime, Triples]
    ].
