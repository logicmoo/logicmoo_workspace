/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Wouter Beek
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2018, VU University Amsterdam
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


:- module(rdf11,
          [ rdf/3,                      % ?S, ?P, ?O
            rdf/4,                      % ?S, ?P, ?O, ?G
            rdf_has/3,                  % ?S, ?P, ?O
            rdf_has/4,                  % ?S, ?P, ?O, -RealP
            rdf_update/4,               % +S, +P, +O, +Action
            rdf_update/5,               % +S, +P, +O, +G, +Action
            rdf_reachable/3,            % ?S, ?P, ?O
            rdf_reachable/5,            % ?S, ?P, ?O, +MaxD, -D

            rdf_assert/3,               % +S, +P, +O
            rdf_assert/4,               % +S, +P, +O, ?G
            rdf_retractall/3,           % ?S, ?P, ?O
            rdf_retractall/4,           % ?S, ?P, ?O, ?G

            {}/1,                       % +Where
            rdf_where/1,                % +Where
            rdf_compare/3,              % -Diff, +Left, +Right

            rdf_term/1,                 % ?Term
            rdf_literal/1,              % ?Term
            rdf_bnode/1,                % ?Term
            rdf_iri/1,                  % ?Term
            rdf_name/1,                 % ?Term

            rdf_is_iri/1,               % @Term
            rdf_is_bnode/1,             % @Term
            rdf_is_literal/1,           % @Term
            rdf_is_name/1,              % @Term
            rdf_is_object/1,            % @Term
            rdf_is_predicate/1,         % @Term
            rdf_is_subject/1,           % @Term
            rdf_is_term/1,              % @Term

            rdf_subject/1,              % ?Term
            rdf_predicate/1,            % ?Term
            rdf_object/1,               % ?Term
            rdf_node/1,                 % ?Term

            rdf_create_bnode/1,         % ?Term

            rdf_canonical_literal/2,    % +In, -Canonical
            rdf_lexical_form/2,         % +Literal, -Lexical

            rdf_default_graph/1,        % -Graph
            rdf_default_graph/2,        % -Old, +New

            rdf_estimate_complexity/4,  % ?S, ?P, ?O, -Estimate
            rdf_assert_list/2,          % +PrologList, ?RDFList
            rdf_assert_list/3,          % +PrologList, ?RDFList, +G
            rdf_last/2,                 % +RDFList, ?Last
            rdf_list/1,                 % ?RDFList
            rdf_list/2,                 % +RDFList, -PrologList
            rdf_length/2,               % ?RDFList, ?Length
            rdf_member/2,               % ?Member, +RDFList
            rdf_nextto/2,               % ?X, ?Y
            rdf_nextto/3,               % ?X, ?Y, ?RdfList
            rdf_nth0/3,                 % ?Index, +RDFList, ?X
            rdf_nth1/3,                 % ?Index, +RDFList, ?X
            rdf_retract_list/1,         % +RDFList

            op(110, xfx, @),            % must be above .
            op(650, xfx, ^^),           % must be above :
            op(1150, fx, rdf_meta)
          ]).
:- use_module(library(semweb/rdf_prefixes),
              [ (rdf_meta)/1, op(_,_,rdf_meta)
              ]).
:- use_module(library(semweb/rdf_db),
              [ rdf_transaction/2,
                rdf_match_label/3,
                rdf_equal/2,
                rdf_is_bnode/1,
                rdf_transaction/1
              ]).

:- autoload(library(apply),[partition/4]).
:- autoload(library(c14n2),[xml_write_canonical/3]).
:- autoload(library(debug),[assertion/1,debug/3]).
:- autoload(library(error),
	    [ must_be/2,
	      domain_error/2,
	      instantiation_error/1,
	      existence_error/2,
	      type_error/2,
	      is_of_type/2,
	      uninstantiation_error/1
	    ]).
:- autoload(library(lists),[select/3,append/3]).
:- autoload(library(memfile),
	    [new_memory_file/1,open_memory_file/3,free_memory_file/1]).
:- autoload(library(sgml),
	    [ xsd_number_string/2,
	      xsd_time_string/3,
	      xml_is_dom/1,
	      load_xml/3,
	      load_html/3
	    ]).
:- autoload(library(sgml_write),[html_write/3,xml_write/2]).
:- autoload(library(solution_sequences),[distinct/2]).

:- reexport(library(semweb/rdf_db),
            except([ rdf/3,
                     rdf/4,
                     rdf_assert/3,
                     rdf_assert/4,
                     rdf_current_literal/1,
                     rdf_current_predicate/1,
                     rdf_has/3,
                     rdf_has/4,
                     rdf_update/4,
                     rdf_update/5,
                     rdf_reachable/3,
                     rdf_reachable/5,
                     rdf_retractall/3,
                     rdf_retractall/4,
                     rdf_node/1,
                     rdf_bnode/1,
                     rdf_is_literal/1,
                     rdf_is_resource/1,
                     rdf_literal_value/2,
                     rdf_compare/3,
                     rdf_estimate_complexity/4
                   ])
           ).
/** <module> RDF 1.1 API

This library provides a new API   on  top of library(semweb/rdf_db). The
new API follows the  RDF  1.1  terminology   and  notation  as  much  as
possible. It runs on top of the old API, which implies that applications
can use the new API in one file and   the other in another one. Once the
new API is considered stable and robust the old API will be deprecated.

In a nutshell, the following issues are addressed:

  - Literals are now represented by Value^^Type or Text@Lang.  Plain
    literals no longer exist. Value is a Prolog representation of
    the value for known types.  In particular:
      - xsd:double, xsd:float and xsd:decimal are represented by a Prolog
        float
      - Integer types are represented by a Prolog integer
      - The date/time types are presented by Prolog terms
  - Literal matching and comparison operations are represented as
    Prolog _constraints_.  This replaces the literal(+Search,-Value)
    construct used by library(semweb/rdf_db). For example, the following
    query returns literals with prefix "ams", exploiting the RDF literal
    index.

      ==
      { prefix(Name, "ams") },
      rdf(S,P,Name).
      ==
  - Graphs are always identified by the graph name only, i.e., the
    notation Graph:Line is no longer supported.  If a graph name is an IRI
    then RDF prefix notation can now be used.
  - The enumeration and type-testing predicates are now more closely based
    on the RDF 1.1 specification and use consistent naming.

@author Jan Wielemaker
@author Wouter Beek
@see https://github.com/SWI-Prolog/packages-semweb/wiki/Proposal-for-Semweb-library-redesign
@version 2016
*/

:- multifile
    in_ground_type_hook/3,                  % +Type, +Input, -Lexical:atom
    out_type_hook/3,                        % +Type, -Output, +Lexical:atom
    invalid_lexical_form_hook/3.            % +Type, +Lexical, -Prolog

:- meta_predicate
    parse_partial_xml(3,+,-).

:- rdf_meta
    rdf(r,r,o),
    rdf(r,r,o,r),
    rdf_assert(r,r,o),
    rdf_assert(r,r,o,r),
    rdf_has(r,r,o),
    rdf_has(r,r,o,-),
    rdf_update(r,r,o,t),
    rdf_update(r,r,o,r,t),
    rdf_reachable(r,r,o),
    rdf_reachable(r,r,o,+,-),
    rdf_retractall(r,r,o),
    rdf_retractall(r,r,o,r),
    {}(t),
    rdf_where(t),
    rdf_canonical_literal(o,-),
    rdf_lexical_form(o,-),
    rdf_compare(-,o,o),
    rdf_iri(r),
    rdf_is_iri(r),
    rdf_is_literal(o),
    rdf_is_name(o),
    rdf_is_object(o),
    rdf_is_predicate(r),
    rdf_is_subject(r),
    rdf_is_term(o),
    rdf_term(o),
    rdf_literal(o),
    rdf_name(o),
    rdf_object(o),
    rdf_estimate_complexity(r,r,o,-),
    rdf_assert_list(t,r),
    rdf_assert_list(t,r,r),
    rdf_last(r,o),
    rdf_list(r),
    rdf_list(r,-),
    rdf_length(r,-),
    rdf_member(o,r),
    rdf_nextto(o,o),
    rdf_nth0(?,r,o),
    rdf_nth1(?,r,o),
    rdf_retract_list(r).


%!  rdf(?S, ?P, ?O) is nondet.
%!  rdf(?S, ?P, ?O, ?G) is nondet.
%
%   True if an RDF triple <S,P,O> exists, optionally in the graph G.
%   The object O is either a resource  (atom)   or  one of the terms
%   listed below. The described types apply for  the case where O is
%   unbound. If O is instantiated it   is converted according to the
%   rules described with rdf_assert/3.
%
%   Triples consist of the following three terms:
%
%     - Blank nodes are encoded by atoms that start with `_:`.
%     - IRIs appear in two notations:
%       - Full IRIs are encoded by atoms that do not start with
%         `_:`.  Specifically, an IRI term is not required to follow
%         the IRI standard grammar.
%       - Abbreviated IRI notation that allows IRI prefix aliases
%         that are registered by rdf_register_prefix/[2,3] to be
%         used.  Their notation is `Alias:Local`, where Alias and
%         Local are atoms.  Each abbreviated IRI is expanded by the
%         system to a full IRI.
%     - Literals appear in two notations:
%       - String@Lang
%       A language-tagged string, where String is a Prolog string
%       and Lang is an atom.
%       - Value^^Type
%       A type qualified literal.  For unknown types, Value is a
%       Prolog string. If type is known, the Prolog representations
%       from the table below are used.
%
%       | **Datatype IRI**      | **Prolog term**                 |
%       |:----------------------|:--------------------------------|
%       | xsd:float             | float                           |
%       | xsd:double            | float                           |
%       | xsd:decimal           | float                     (1)   |
%       | xsd:integer           | integer                         |
%       | XSD integer sub-types | integer                         |
%       | xsd:boolean           | `true` or `false`               |
%       | xsd:date              | date(Y,M,D)                     |
%       | xsd:dateTime          | date_time(Y,M,D,HH,MM,SS) (2,3) |
%       | xsd:gDay              | integer                         |
%       | xsd:gMonth            | integer                         |
%       | xsd:gMonthDay         | month_day(M,D)                  |
%       | xsd:gYear             | integer                         |
%       | xsd:gYearMonth        | year_month(Y,M)                 |
%       | xsd:time              | time(HH,MM,SS)            (2)   |
%
%   Notes:
%
%     (1) The current implementation of `xsd:decimal` values
%         as floats is formally incorrect.  Future versions
%         of SWI-Prolog may introduce decimal as a subtype
%         of rational.
%
%     (2) `SS` fields denote the number of seconds.  This can
%         either be an integer or a float.
%
%     (3) The `date_time` structure can have a 7th field that
%         denotes the timezone offset *in seconds* as an
%         integer.
%
%   In addition, a _ground_  object  value   is  translated  into  a
%   properly typed RDF literal using rdf_canonical_literal/2.
%
%   There is a fine distinction  in   how  duplicate  statements are
%   handled in rdf/[3,4]: backtracking over  rdf/3 will never return
%   duplicate triples that appear  in   multiple  graphs. rdf/4 will
%   return such duplicate triples, because their graph term differs.
%
%   @arg S is the subject term.  It is either a blank node or IRI.
%   @arg P is the predicate term.  It is always an IRI.
%   @arg O is the object term.  It is either a literal, a blank
%        node or IRI (except for `true` and `false` that denote the
%        values of datatype XSD boolean).
%   @arg G is the graph term.  It is always an IRI.
%
%   @see [Triple pattern querying](http://www.w3.org/TR/sparql11-query/#sparqlTriplePatterns)
%   @see xsd_number_string/2 and xsd_time_string/3 are used to
%        convert between lexical representations and Prolog terms.

rdf(S,P,O) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf(S,P,O0),
    post_object(O,O0).

rdf(S,P,O,G) :-
    pre_object(O,O0,S,P),
    pre_graph(G,G0),
    rdf_db:rdf(S,P,O0,G0),
    post_graph(G, G0),
    post_object(O,O0).

%!  rdf_has(?S, +P, ?O) is nondet.
%!  rdf_has(?S, +P, ?O, -RealP) is nondet.
%
%   Similar to rdf/3 and rdf/4, but   P  matches all predicates that
%   are defined as an rdfs:subPropertyOf of   P. This predicate also
%   recognises   the   predicate   properties     `inverse_of`   and
%   `symmetric`. See rdf_set_predicate/2.

rdf_has(S,P,O) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_has(S,P,O0),
    post_object(O,O0).

rdf_has(S,P,O,RealP) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_has(S,P,O0,RealP),
    post_object(O,O0).


%!  rdf_update(+S, +P, +O, ++Action) is det.
%!  rdf_update(+S, +P, +O, +G, ++Action) is det.
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
%
%   The argument matching Action must  be   ground.  If this argument is
%   equivalent to the current value, no  action is performed. Otherwise,
%   the requested action is  performed  on   all  matching  triples. For
%   example,  all  resources  typed  `rdfs:Class`   can  be  changed  to
%   `owl:Class` using
%
%     ```
%     ?- rdf_update(_, rdf:type, rdfs:'Class',
%                   object(owl:'Class')).
%     ```
%
%   @error instantiation_error if Action or the matching argument is
%          not ground.
%   @error domain_error(rdf_update_action, Action) if Action is not
%          one of the above terms.

rdf_update(S, P, O, Action) :-
    rdf_update(S, P, O, _, Action).
rdf_update(S, P, O, G, Action) :-
    must_be(ground, Action),
    (   update_column(Action, S,P,O,G, On)
    ->  must_be(ground, On),
        arg(1, Action, Old),
        (   On == Old
        ->  true
        ;   rdf_transaction(rdf_update_(S, P, O, G, Action), update)
        )
    ;   domain_error(rdf_update_action, Action)
    ).

update_column(subject(_),   S,_,_,_, S).
update_column(predicate(_), _,P,_,_, P).
update_column(object(_),    _,_,O,_, O).
update_column(graph(_),     _,_,_,G, G).

rdf_update_(S1, P, O, G, subject(S2)) :-
    !,
    forall(rdf(S1, P, O, G),
           ( rdf_retractall(S1, P, O, G),
             rdf_assert(S2, P, O, G)
           )).
rdf_update_(S, P1, O, G, predicate(P2)) :-
    !,
    forall(rdf(S, P1, O, G),
           ( rdf_retractall(S, P1, O, G),
             rdf_assert(S, P2, O, G)
           )).
rdf_update_(S, P, O1, G, object(O2)) :-
    !,
    forall(rdf(S, P, O1, G),
           ( rdf_retractall(S, P, O1, G),
             rdf_assert(S, P, O2, G)
           )).
rdf_update_(S, P, O, G1, graph(G2)) :-
    !,
    forall(rdf(S, P, O, G1),
           ( rdf_retractall(S, P, O, G1),
             rdf_assert(S, P, O, G2)
           )).


%!  rdf_reachable(?S, +P, ?O) is nondet.
%!  rdf_reachable(?S, +P, ?O, +MaxD, -D) is nondet.
%
%   True when O can be reached from S using the transitive closure
%   of P. The predicate uses (the internals of) rdf_has/3 and thus
%   matches both rdfs:subPropertyOf and the `inverse_of` and
%   `symmetric` predicate properties. The version rdf_reachable/5
%   maximizes the steps considered and returns the number of steps
%   taken.
%
%   If both S and O are given,   these predicates are `semidet`. The
%   number of steps D is  minimal   because  the implementation uses
%   _breadth first_ search.

rdf_reachable(S,P,O) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_reachable(S,P,O0),
    post_object(O,O0).

rdf_reachable(S,P,O,MaxD,D) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_reachable(S,P,O0,MaxD,D),
    post_object(O,O0).


%!  rdf_assert(+S, +P, +O) is det.
%!  rdf_assert(+S, +P, +O, +G) is det.
%
%   Assert a new triple. If O is a literal, certain Prolog terms are
%   translated  to  typed  RDF  literals.    These  conversions  are
%   described with rdf_canonical_literal/2.
%
%   If a type  is  provided   using  Value^^Type  syntax, additional
%   conversions are performed. All types accept   either  an atom or
%   Prolog string holding a valid RDF lexical value for the type and
%   xsd:float and xsd:double accept a Prolog integer.

rdf_assert(S,P,O) :-
    rdf_default_graph(G),
    rdf_assert(S,P,O,G).

rdf_assert(S,P,O,G) :-
    must_be(ground, O),
    pre_ground_object(O,O0),
    rdf_db:rdf_assert(S,P,O0,G).

%!  rdf_retractall(?S, ?P, ?O) is nondet.
%!  rdf_retractall(?S, ?P, ?O, ?G) is nondet.
%
%   Remove all matching  triples  from   the  database.  Matching is
%   performed using the same  rules  as   rdf/3.  The  call does not
%   instantiate any of its arguments.

rdf_retractall(S,P,O) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_retractall(S,P,O0).

rdf_retractall(S,P,O,G) :-
    pre_object(O,O0,S,P),
    pre_graph(G,G0),
    rdf_db:rdf_retractall(S,P,O0,G0).


%!  rdf_compare(-Diff, +Left, +Right) is det.
%
%   True if the RDF terms Left and   Right  are ordered according to
%   the comparison operator Diff.  The ordering is defines as:
%
%     - Literal < BNode < IRI
%     - For literals
%       - Numeric < non-numeric
%       - Numeric literals are ordered by value.  If both are
%         equal, floats are ordered before integers.
%       - Other data types are ordered lexicographically.
%     - BNodes and IRIs are ordered lexicographically.
%
%   Note that this ordering is a complete ordering of RDF terms that
%   is consistent with the partial ordering defined by SPARQL.
%
%   @arg Diff is one of `<`, `=` or `>`

rdf_compare(Diff, Left, Right) :-
    pre_ground_object(Left, Left0),
    pre_ground_object(Right, Right0),
    rdf_db:rdf_compare(Diff, Left0, Right0).


%!  {}(+Where) is semidet.
%!  rdf_where(+Where) is semidet.
%
%   Formulate constraints on RDF terms,  notably literals. These are
%   intended to be used as illustrated   below.  RDF constraints are
%   pure: they may be placed before, after or inside a graph pattern
%   and, provided the code contains no  _commit_ operations (!, ->),
%   the  semantics  of  the  goal   remains  the  same.  Preferably,
%   constraints are placed _before_ the graph  pattern as they often
%   help the RDF database to  exploit   its  literal indexes. In the
%   example below, the database can choose between using the subject
%   and/or predicate hash or the ordered literal table.
%
%     ==
%         { Date >= "2000-01-01"^^xsd:date },
%         rdf(S, P, Date)
%     ==
%
%   The following constraints are currently defined:
%
%     - >, >=, ==, =<, <
%       The comparison operators are defined between numbers (of any
%       recognised type), typed literals of the same type and
%       langStrings of the same language.
%     - prefix(String, Pattern)
%     - substring(String, Pattern)
%     - word(String, Pattern)
%     - like(String, Pattern)
%     - icase(String, Pattern)
%       Text matching operators that act on both typed literals
%       and langStrings.
%     - lang_matches(Term, Pattern)
%       Demands a full RDF term (Text@Lang) or a plain `Lang` term
%       to match the language pattern Pattern.
%
%   The  predicates  rdf_where/1  and  {}/1    are   identical.  The
%   rdf_where/1  variant  is  provided   to    avoid   ambiguity  in
%   applications where {}/1 is used for other purposes. Note that it
%   is also possible to write `rdf11:{...}`.

{}(Where) :-
    rdf_where(Where).

rdf_where(Var) :-
    var(Var),
    !,
    instantiation_error(Var).
rdf_where((A,B)) :-
    !,
    rdf_where(A),
    rdf_where(B).
rdf_where(Constraint) :-
    rdf_constraint(Constraint, Goal),
    !,
    call(Goal).
rdf_where(Constraint) :-
    existence_error(rdf_constraint, Constraint).

% Comparison operators
rdf_constraint(Term >= Value,
               add_value_constraint(Term, >=, Value)).
rdf_constraint(Term >  Value,
               add_value_constraint(Term, >,  Value)).
rdf_constraint(Term == Value,
               add_value_constraint(Term, ==,  Value)).
rdf_constraint(Term <  Value,
               add_value_constraint(Term, <,  Value)).
rdf_constraint(Term =< Value,
               add_value_constraint(Term, =<, Value)).
% String selection
rdf_constraint(prefix(Term, Pattern),
               add_text_constraint(Term, prefix(PatternA))) :-
    atom_string(PatternA, Pattern).
rdf_constraint(substring(Term, Pattern),
               add_text_constraint(Term, substring(PatternA))) :-
    atom_string(PatternA, Pattern).
rdf_constraint(word(Term, Pattern),
               add_text_constraint(Term, word(PatternA))) :-
    atom_string(PatternA, Pattern).
rdf_constraint(like(Term, Pattern),
               add_text_constraint(Term, like(PatternA))) :-
    atom_string(PatternA, Pattern).
rdf_constraint(icase(Term, Pattern),
               add_text_constraint(Term, icase(PatternA))) :-
    atom_string(PatternA, Pattern).
% Lang selection
rdf_constraint(lang_matches(Term, Pattern),
               add_lang_constraint(Term, lang_matches(Pattern))).

add_text_constraint(Var, Cond) :-
    var(Var),
    !,
    (   get_attr(Var, rdf11, Cond0)
    ->  put_attr(Var, rdf11, [Cond|Cond0])
    ;   put_attr(Var, rdf11, [Cond])
    ).
add_text_constraint(Text^^_Type, Cond) :-
    !,
    add_text_constraint(Text, Cond).
add_text_constraint(Text@_Lang, Cond) :-
    !,
    add_text_constraint(Text, Cond).
add_text_constraint(Var, Cond) :-
    eval_condition(Cond, Var).

%!  add_lang_constraint(?Term, +Constraint)
%
%   Add a constraint on the language of a literal

add_lang_constraint(Var, Constraint) :-
    var(Var),
    !,
    (   get_attr(Var, rdf11, Cond0)
    ->  put_attr(Var, rdf11, [Constraint|Cond0])
    ;   put_attr(Var, rdf11, [Constraint])
    ).
add_lang_constraint(_Text@Lang, Constraint) :-
    !,
    add_lang_constraint(Lang, Constraint).
add_lang_constraint(_Text^^_Type, _Constraint) :-
    !,
    fail.
add_lang_constraint(Term, Constraint) :-
    eval_condition(Constraint, Term).

%!  add_value_constraint(?Term, +Constraint, +Value)
%
%   Apply a value constraint to the RDF Term.

add_value_constraint(Term, Constraint, ValueIn) :-
    constraint_literal_value(ValueIn, Value),
    add_value_constraint_cann(Value, Constraint, Term).

constraint_literal_value(Value, Value^^_Type) :-
    number(Value),
    !.
constraint_literal_value(Value, Literal) :-
    rdf_canonical_literal(Value, Literal).

add_value_constraint_cann(RefVal^^Type, Constraint, Term) :-
    var(Term), var(Type),
    !,
    add_text_constraint(Term, value(Constraint, RefVal, Type)).
add_value_constraint_cann(RefVal^^Type, Constraint, Val^^Type2) :-
    !,
    Type = Type2,
    add_text_constraint(Val, value(Constraint, RefVal, Type)).
add_value_constraint_cann(RefVal@Lang, Constraint, Val@Lang) :-
    !,
    add_text_constraint(Val, value(Constraint, RefVal, lang(Lang))).
add_value_constraint_cann(RefVal^^Type, Constraint, Val) :-
    !,
    ground(Val),
    Val \= _@_,
    eval_condition(value(Constraint, RefVal, Type), Val).

put_cond(Var, []) :-
    !,
    del_attr(Var, rdf11).
put_cond(Var, List) :-
    put_attr(Var, rdf11, List).

eval_condition(Cond, Literal) :-
    text_condition(Cond),
    !,
    text_of(Literal, Text),
    text_condition(Cond, Text).
eval_condition(Cond, Literal) :-
    lang_condition(Cond),
    !,
    lang_of(Literal, Lang),
    lang_condition(Cond, Lang).
eval_condition(value(Comp, Ref, _Type), Value) :-
    (   number(Ref)
    ->  number(Value),
        compare_numeric(Comp, Ref, Value)
    ;   compare_std(Comp, Ref, Value)
    ).

compare_numeric(<,  Ref, Value) :- Value  < Ref.
compare_numeric(=<, Ref, Value) :- Value =< Ref.
compare_numeric(==, Ref, Value) :- Value =:= Ref.
compare_numeric(>=, Ref, Value) :- Value >= Ref.
compare_numeric( >, Ref, Value) :- Value >  Ref.

compare_std(<,  Ref, Value) :- Value  @< Ref.
compare_std(=<, Ref, Value) :- Value @=< Ref.
compare_std(==, Ref, Value) :- Value ==  Ref.
compare_std(>=, Ref, Value) :- Value @>= Ref.
compare_std( >, Ref, Value) :- Value @>  Ref.

text_condition(prefix(_)).
text_condition(substring(_)).
text_condition(word(_)).
text_condition(like(_)).
text_condition(icase(_)).

text_of(Literal, Text) :-
    atomic(Literal),
    !,
    Text = Literal.
text_of(Text@_Lang, Text).
text_of(Text^^_Type, Text).

text_condition(prefix(Pattern), Text) :-
    rdf_match_label(prefix, Pattern, Text).
text_condition(substring(Pattern), Text) :-
    rdf_match_label(substring, Pattern, Text).
text_condition(word(Pattern), Text) :-
    rdf_match_label(word, Pattern, Text).
text_condition(like(Pattern), Text) :-
    rdf_match_label(like, Pattern, Text).
text_condition(icase(Pattern), Text) :-
    rdf_match_label(icase, Pattern, Text).

lang_condition(lang_matches(_)).

lang_of(_Text@Lang0, Lang) :-
    !,
    Lang = Lang0.
lang_of(Lang, Lang) :-
    atom(Lang).

lang_condition(lang_matches(Pattern), Lang) :-
    rdf_db:lang_matches(Lang, Pattern).

%!  literal_condition(+Object, -Cond) is semidet.
%
%   True when some of the constraints   on  Object can be translated
%   into an equivalent query  of   the  form  literal(Cond, _Value).
%   Translated constraints are removed from object.

literal_condition(Object, Cond) :-
    get_attr(Object, rdf11, Cond0),
    best_literal_cond(Cond0, Cond, Rest),
    put_cond(Object, Rest).

%!  best_literal_cond(+Conditions, -Best, -Rest) is semidet.
%
%   Extract the constraints that can be translated into the _Search_
%   of literal(Search, Value).
%
%   @tbd    Select the best rather than the first.

best_literal_cond(Conditions, Best, Rest) :-
    sort(Conditions, Unique),
    best_literal_cond2(Unique, Best, Rest).

best_literal_cond2(Conds, Best, Rest) :-
    select(Cond, Conds, Rest0),
    rdf10_cond(Cond, Best, Rest0, Rest),
    !.

rdf10_cond(value(=<, URef, UType), Cond, Rest0, Rest) :-
    (   select(value(>=, LRef, LType), Rest0, Rest)
    ->  true
    ;   memberchk(value(>, LRef, LType), Rest0)
    ->  Rest = Rest0
    ),
    !,
    in_constaint_type(LType, SLType, LRef, LRef0),
    in_constaint_type(UType, SUType, URef, URef0),
    Cond = between(type(SLType, LRef0), type(SUType, URef0)).
rdf10_cond(value(<, URef, UType), Cond, Rest0, Rest) :-
    (   select(value(>=, LRef, LType), Rest0, Rest1)
    ->  true
    ;   memberchk(value(>, LRef, LType), Rest0)
    ->  Rest1 = Rest0
    ),
    !,
    Rest = [value(<, URef, UType)|Rest1],
    in_constaint_type(LType, SLType, LRef, LRef0),
    in_constaint_type(UType, SUType, URef, URef0),
    Cond = between(type(SLType, LRef0), type(SUType, URef0)).
rdf10_cond(value(Cmp, Ref, Type), Pattern, Rest, Rest) :-
    !,
    rdf10_compare(Cmp, Ref, Type, Pattern).
rdf10_cond(lang_matches(_), _, _, _) :- !, fail.
rdf10_cond(Cond, Cond, Rest, Rest).

rdf10_compare(Cmp, Ref, Type, Pattern) :-
    nonvar(Type), Type = lang(Lang),
    !,
    atom_string(Ref0, Ref),
    rdf10_lang_cond(Cmp, Ref0, Lang, Pattern).
rdf10_compare(Cmp, Ref, Type, Pattern) :-
    in_constaint_type(Type, SType, Ref, Ref0),
    rdf10_type_cond(Cmp, Ref0, SType, Pattern).

rdf10_lang_cond( <, Ref, Lang, lt(lang(Lang,Ref))).
rdf10_lang_cond(=<, Ref, Lang, le(lang(Lang,Ref))).
rdf10_lang_cond(==, Ref, Lang, eq(lang(Lang,Ref))).
rdf10_lang_cond(>=, Ref, Lang, ge(lang(Lang,Ref))).
rdf10_lang_cond(>,  Ref, Lang, gt(lang(Lang,Ref))).

rdf10_type_cond( <, Ref, Type, lt(type(Type,Ref))).
rdf10_type_cond(=<, Ref, Type, le(type(Type,Ref))).
rdf10_type_cond(==, Ref, Type, eq(type(Type,Ref))).
rdf10_type_cond(>=, Ref, Type, ge(type(Type,Ref))).
rdf10_type_cond( >, Ref, Type, gt(type(Type,Ref))).


%!  in_constaint_type(?Type, -SType, ++Val, -Val0)

in_constaint_type(Type, SType, Val, Val0) :-
    nonvar(Type), ground(Val),
    !,
    SType = Type,
    in_ground_type(Type, Val, Val0).
in_constaint_type(Type, SType, Val, Val0) :-
    var(Type), number(Val),
    !,
    (   integer(Val)
    ->  rdf_equal(SType, xsd:integer),
        in_ground_type(xsd:integer, Val, Val0)
    ;   float(Val)
    ->  rdf_equal(SType, xsd:double),
        in_ground_type(xsd:double, Val, Val0)
    ;   assertion(fail)
    ).


%!  literal_class(+Term, -Class)
%
%   Classify Term as literal  and  if   possible  as  lang  or typed
%   literal on the basis of the constraints that apply to it.

literal_class(Term, Class) :-
    get_attr(Term, rdf11, Conds),
    select(Cond, Conds, Rest),
    lang_condition(Cond),
    !,
    Term = Text@Lang,
    put_attr(Lang, rdf11, [Cond]),
    put_cond(Text, Rest),
    (   var(Text)
    ->  true
    ;   atom_string(Text2, Text)
    ),
    Class = lang(Lang, Text2).

%!  attr_unify_hook(+AttributeValue, +Value)

attr_unify_hook(Cond, Value) :-
    get_attr(Value, rdf11, Cond2),
    !,
    append(Cond, Cond2, CondJ),
    sort(CondJ, Unique),
    put_cond(Value, Unique).
attr_unify_hook(Cond, Text^^_Type) :-
    var(Text),
    !,
    put_cond(Text, Cond).
attr_unify_hook(Cond, Text@Lang) :-
    var(Text), var(Lang),
    !,
    partition(lang_condition, Cond, LangCond, TextCond),
    put_cond(Text, TextCond),
    put_cond(Lang, LangCond).
attr_unify_hook(Cond, Value) :-
    sort(Cond, Unique),
    propagate_conditions(Unique, Value).

propagate_conditions([], _).
propagate_conditions([H|T], Val) :-
    propagate_condition(H, Val),
    propagate_conditions(T, Val).

propagate_condition(value(Comp, Ref, Type), Value) :-
    !,
    (   Value = Plain^^VType
    ->  VType = Type
    ;   Plain = Value
    ),
    cond_compare(Comp, Ref, Plain).
propagate_condition(lang_matches(Pattern), Value) :-
    !,
    (   Value = _@Lang
    ->  true
    ;   Lang = Value
    ),
    rdf_db:lang_matches(Lang, Pattern).
propagate_condition(Cond, Value) :-
    Cond =.. [Name|Args],
    Constraint =.. [Name,Value|Args],
    rdf_constraint(Constraint, Continuation),
    call(Continuation).

cond_compare(>,  Ref, Value) :- Value @>  Ref.
cond_compare(>=, Ref, Value) :- Value @>= Ref.
cond_compare(==, Ref, Value) :- Value ==  Ref.
cond_compare(=<, Ref, Value) :- Value @=< Ref.
cond_compare( <, Ref, Value) :- Value  @< Ref.


%!  rdf_default_graph(-Graph) is det.
%!  rdf_default_graph(-Old, +New) is det.
%
%   Query/set the notion of the  default   graph.  The notion of the
%   default graph is local to a  thread. Threads created inherit the
%   default graph from their creator. See set_prolog_flag/2.

:- create_prolog_flag(rdf_default_graph, default,
                      [ type(atom),
                        keep(true)
                      ]).

rdf_default_graph(Graph) :-
    current_prolog_flag(rdf_default_graph, Graph).
rdf_default_graph(Old, New) :-
    current_prolog_flag(rdf_default_graph, Old),
    (   New == Old
    ->  true
    ;   set_prolog_flag(rdf_default_graph, New)
    ).


pre_graph(G, _G0) :-
    var(G),
    !.
pre_graph(G, G) :-
    atom(G),
    !.
pre_graph(G, _) :-
    type_error(rdf_graph, G).

post_graph(G, G0:_) :-
    !,
    G = G0.
post_graph(G, G).


%   left for code calling this directly

pre_object(Atom, URI) :-
    pre_object(Atom, URI, _, _).

%!  pre_object(+APIObject, -DBObject, +APISubject, +APIPredicate)

pre_object(Atom, URI, _, _) :-
    atom(Atom),
    \+ boolean(Atom),
    !,
    URI = Atom.
pre_object(Var, Var1, Subj, Pred) :-
    var(Var),
    !,
    (   literal_condition(Var, Cond)
    ->  Var1 = literal(Cond, _)
    ;   literal_class(Var, Value)
    ->  Var1 = literal(Value)
    ;   (   Var == Subj
        ->  Var1 = Subj
        ;   true
        ),
        (   Var == Pred
        ->  Var1 = Pred
        ;   true
        )
    ).
pre_object(Val@Lang, Var1, _, _) :-
    !,
    (   literal_condition(Val, Cond)
    ->  Var1 = literal(Cond, lang(Lang, _))
    ;   literal_class(Val@Lang, Class)
    ->  Var1 = literal(Class)
    ;   in_lang_string(Val, Val0),
        Var1 = literal(lang(Lang, Val0))
    ).
pre_object(Val^^Type, Var1, _, _) :-
    !,
    (   literal_condition(Val, Cond)
    ->  Var1 = literal(Cond, type(Type, _))
    ;   in_type(Type, Val, Type0, Val0),
        (   var(Type0), var(Val0)
        ->  Var1 = literal(_)
        ;   Var1 = literal(type(Type0, Val0))
        )
    ).
pre_object(Obj, Val0, _, _) :-
    ground(Obj),
    !,
    pre_ground_object(Obj, Val0).
pre_object(Obj, _, _, _) :-
    type_error(rdf_object, Obj).


%!  pre_ground_object(+Object, -RDF) is det.
%
%   Convert between a Prolog value and an RDF value for rdf_assert/3
%   and friends.  Auto-conversion:
%
%     - Integer
%     Converted to Integer^^xsd:integer
%     - Float
%     Converted to Float^^xsd:double
%     - String
%     Converted to String^^xsd:string
%     - true
%     Converted to true^^xsd:boolean
%     - false
%     Converted to false^^xsd:boolean
%     - date(Y,M,D)
%     Converted to date(Y,M,D)^^xsd:date
%     - date_time(Y,M,D,HH,MM,SS)
%     Converted to date_time(Y,M,D,HH,MM,SS)^^xsd:dateTime
%     - date_time(Y,M,D,HH,MM,SS,TZ)
%     Converted to date_time(Y,M,D,HH,MM,SS,TZ)^^xsd:dateTime
%     - month_day(M,D)
%     Converted to month_day(M,D)^^xsd:gMonthDay
%     - year_month(Y,M)
%     Converted to year_month(Y,M)^^xsd:gYearMonth
%     - time(HH,MM,SS)
%     Converted to time(HH,MM,SS)^^xsd:time
%     - Text@Lang
%     Converted to Text@Lang.  Uses canonical (lowercase) lang.
%     Text is converted into an atom.
%     - Value^^Type
%     Typed conversion.  The translation of Value depends on
%     Type:
%       - Numeric types
%       - Boolean
%       - Date types
%     - Atom
%     All atoms except for `true` and `false` are considered
%     URIs.

:- rdf_meta
    pre_ground_object(+, o).

% Interpret Prolog integer as xsd:integer.
pre_ground_object(Int, Object) :-
    integer(Int),
    !,
    rdf_equal(Object, literal(type(xsd:integer, Atom))),
    atom_number(Atom, Int).
% Interpret Prolog floating-point value as xsd:double.
pre_ground_object(Float, Object) :-
    float(Float),
    !,
    rdf_equal(Object, literal(type(xsd:double, Atom))),
    xsd_number_string(Float, String),
    atom_string(Atom, String).
% Interpret SWI string as xsd:string.
pre_ground_object(String, Object) :-
    string(String),
    !,
    rdf_equal(Object, literal(type(xsd:string, Atom))),
    atom_string(Atom, String).
% Interpret `false' and `true' as the Boolean values.
pre_ground_object(false, literal(type(xsd:boolean, false))) :- !.
pre_ground_object(true, literal(type(xsd:boolean, true))) :- !.
% Interpret date(Y,M,D) as xsd:date,
%           date_time(Y,M,D,HH,MM,SS) as xsd:dateTime,
%           date_time(Y,M,D,HH,MM,SS,TZ) as xsd:dateTime,
%           month_day(M,D) as xsd:gMonthDay,
%           year_month(Y,M) as xsd:gYearMonth, and
%           time(HH,MM,SS) as xsd:time.
pre_ground_object(Term, literal(type(Type, Atom))) :-
    xsd_date_time_term(Term),
    !,
    xsd_time_string(Term, Type, String),
    atom_string(Atom, String).
pre_ground_object(Val@Lang,  literal(lang(Lang0, Val0))) :-
    !,
    downcase_atom(Lang, Lang0),
    in_lang_string(Val, Val0).
pre_ground_object(Val^^Type, literal(type(Type0, Val0))) :-
    !,
    in_type(Type, Val, Type0, Val0).
pre_ground_object(Atom, URI) :-
    atom(Atom),
    !,
    URI = Atom.
pre_ground_object(literal(Lit0), literal(Lit)) :-
    old_literal(Lit0, Lit),
    !.
pre_ground_object(Value, _) :-
    type_error(rdf_object, Value).

xsd_date_time_term(date(_,_,_)).
xsd_date_time_term(date_time(_,_,_,_,_,_)).
xsd_date_time_term(date_time(_,_,_,_,_,_,_)).
xsd_date_time_term(month_day(_,_)).
xsd_date_time_term(year_month(_,_)).
xsd_date_time_term(time(_,_,_)).

old_literal(Lit0, Lit) :-
    old_literal(Lit0),
    !,
    Lit = Lit0.
old_literal(Atom, Lit) :-
    atom(Atom),
    rdf_equal(xsd:string, XSDString),
    Lit = type(XSDString, Atom).

old_literal(type(Type, Value)) :-
    atom(Type), atom(Value).
old_literal(lang(Lang, Value)) :-
    atom(Lang), atom(Value).

in_lang_string(Val, Val0) :-
    atomic(Val),
    !,
    atom_string(Val0, Val).
in_lang_string(_, _).

in_type(Type, Val, Type, Val0) :-
    nonvar(Type), ground(Val),
    !,
    in_ground_type(Type, Val, Val0).
in_type(VarType, Val, VarType, Val0) :-
    ground(Val),
    \+ catch(xsd_number_string(_, Val), _, fail),
    !,
    atom_string(Val0, Val).
in_type(_, _, _, _).

:- rdf_meta
    in_ground_type(r,?,?),
    in_date_component(r, +, +, -).

%!  in_ground_type(+Type, +Input, -Lexical:atom) is det.
%
%   Translate the Prolog date Input according   to Type into its RDF
%   lexical form. The lecical form  is   represented  as an atom. In
%   future versions this is likely to become a string.

in_ground_type(Type, Input, Lex) :-
    \+ string(Input),
    in_ground_type_hook(Type, Input, Lex),
    !.
in_ground_type(IntType, Val, Val0) :-
    xsd_numerical(IntType, Domain, PrologType),
    !,
    in_number(PrologType, Domain, IntType, Val, Val0).
in_ground_type(xsd:boolean, Val, Val0) :-
    !,
    (   in_boolean(Val, Val0)
    ->  true
    ;   type_error(rdf_boolean, Val)
    ).
in_ground_type(rdf:langString, _Val0, _) :-
    !,
    domain_error(rdf_data_type, rdf:langString).
in_ground_type(DateTimeType, Val, Val0) :-
    xsd_date_time_type(DateTimeType),
    !,
    in_date_time(DateTimeType, Val, Val0).
in_ground_type(rdf:'XMLLiteral', Val, Val0) :-
    !,
    in_xml_literal(xml, Val, Val0).
in_ground_type(rdf:'HTML', Val, Val0) :-
    !,
    in_xml_literal(html, Val, Val0).
in_ground_type(_Unknown, Val, Val0) :-
    atom_string(Val0, Val).

%!  in_date_time(+Type, +Input, -Lexical) is det.
%
%   Accepts either a term as  accepted   by  xsd_time_string/3  or a
%   valid string for the corresponding XSD type.

:- rdf_meta
    in_date_time(r,+,-).

in_date_time(Type, Text, Text0) :-
    atom(Text),
    !,
    xsd_time_string(_, Type, Text),
    Text0 = Text.
in_date_time(Type, Text, Text0) :-
    string(Text),
    !,
    xsd_time_string(_, Type, Text),
    atom_string(Text0, Text).
in_date_time(xsd:dateTime, Stamp, Text0) :-
    number(Stamp),
    !,
    format_time(atom(Text0), '%FT%T%:z', Stamp).
in_date_time(Type, Term, Text0) :-
    !,
    xsd_time_string(Term, Type, String),
    atom_string(Text0, String).


%!  in_boolean(?NonCanonical, ?Canonical)
%
%   True when Canonical is the canonical boolean for NonCanonical.

in_boolean(true,    true).
in_boolean(false,   false).
in_boolean("true",  true).
in_boolean("false", false).
in_boolean(1,       true).
in_boolean(0,       false).

boolean(false).
boolean(true).

%!  in_number(+PrologType, +Domain, +XSDType, +Value, -Lexical)
%
%   Lexical is the lexical representation for Value.
%
%   @error  type_error(PrologType, Value)
%   @error  domain_error(XSDType, Value)

in_number(integer, Domain, XSDType, Val, Val0) :-
    integer(Val),
    !,
    check_integer_domain(Domain, XSDType, Val),
    atom_number(Val0, Val).
in_number(integer, Domain, XSDType, Val, Val0) :-
    atomic(Val),
    atom_number(Val, Num),
    integer(Num),
    !,
    check_integer_domain(Domain, XSDType, Num),
    atom_number(Val0, Num).
in_number(double, _Domain, _, Val, Val0) :-
    number(Val),
    !,
    ValF is float(Val),
    xsd_number_string(ValF, ValS),
    atom_string(Val0, ValS).
in_number(double, _Domain, _, Val, Val0) :-
    atomic(Val),
    xsd_number_string(Num, Val),
    ValF is float(Num),
    !,
    xsd_number_string(ValF, ValS),
    atom_string(Val0, ValS).
in_number(decimal, _Domain, _, Val, Val0) :-
    number(Val),
    !,
    ValF is float(Val),
    atom_number(Val0, ValF).
in_number(decimal, _Domain, _, Val, Val0) :-
    atomic(Val),
    xsd_number_string(Num, Val),
    ValF is float(Num),
    !,
    atom_number(Val0, ValF).
in_number(PrologType, _, _, Val, _) :-
    type_error(PrologType, Val).

check_integer_domain(PLType, _, Val) :-
    is_of_type(PLType, Val),
    !.
check_integer_domain(_, XSDType, Val) :-
    domain_error(XSDType, Val).

error:has_type(nonpos, T):-
    integer(T),
    T =< 0.

%check_integer_domain(between(Low, High), XSDType, Val) :-
%       (   between(Low, High, Val)
%       ->  true
%       ;   domain_error(XSDType, Val)
%       ).
%check_integer_domain(integer, _, _).

%!  xsd_numerical(?URI, ?TypeCheck, ?PrologType)

:- rdf_meta
    xsd_numerical(r, ?, ?).

xsd_numerical(xsd:byte,               between(-128,127),               integer).
xsd_numerical(xsd:double,             float,                           double).
xsd_numerical(xsd:decimal,            float,                           decimal).
xsd_numerical(xsd:float,              float,                           double).
xsd_numerical(xsd:int,                between(-2147483648,2147483647), integer).
xsd_numerical(xsd:integer,            integer,                         integer).
xsd_numerical(xsd:long,               between(-9223372036854775808,
                                               9223372036854775807),   integer).
xsd_numerical(xsd:negativeInteger,    negative_integer,                integer).
xsd_numerical(xsd:nonNegativeInteger, nonneg,                          integer).
xsd_numerical(xsd:nonPositiveInteger, nonpos,                          integer).
xsd_numerical(xsd:positiveInteger,    positive_integer,                integer).
xsd_numerical(xsd:short,              between(-32768,32767),           integer).
xsd_numerical(xsd:unsignedByte,       between(0,255),                  integer).
xsd_numerical(xsd:unsignedInt,        between(0,4294967295),           integer).
xsd_numerical(xsd:unsignedLong,       between(0,18446744073709551615), integer).
xsd_numerical(xsd:unsignedShort,      between(0,65535),                integer).

%!  xsd_date_time_type(?URI)
%
%   True when URI is an XSD date or time type.

:- rdf_meta
    xsd_date_time_type(r).

xsd_date_time_type(xsd:date).
xsd_date_time_type(xsd:dateTime).
xsd_date_time_type(xsd:gDay).
xsd_date_time_type(xsd:gMonth).
xsd_date_time_type(xsd:gMonthDay).
xsd_date_time_type(xsd:gYear).
xsd_date_time_type(xsd:gYearMonth).
xsd_date_time_type(xsd:time).


%!  in_xml_literal(+Type, +Val, -Val0) is det.
%
%   Translate an XMLLiteral or HTML literal to its canonical textual
%   representation. Input is either text or a Prolog XML DOM.
%
%   @tbd    Deal with partial content?

in_xml_literal(Type, Val, Val0) :-
    xml_is_dom(Val),
    !,
    write_xml_literal(Type, Val, Val0).
in_xml_literal(xml, Val, Val0) :-
    parse_partial_xml(load_xml, Val, DOM),
    write_xml_literal(xml, DOM, Val0).
in_xml_literal(html, Val, Val0) :-
    parse_partial_xml(load_html, Val, DOM),
    write_xml_literal(html, DOM, Val0).

parse_partial_xml(Parser, Val, DOM) :-
    setup_call_cleanup(
        new_memory_file(MF),
        (   setup_call_cleanup(
                open_memory_file(MF, write, Out),
                format(Out, "<xml>~w</xml>", [Val]),
                close(Out)),
            setup_call_cleanup(
                open_memory_file(MF, read, In),
                call(Parser, stream(In), [element(xml, _, DOM)], []),
                close(In))
        ),
        free_memory_file(MF)).


write_xml_literal(xml, DOM, Text) :-
    with_output_to(atom(Text),
                   xml_write_canonical(current_output, DOM, [])).
write_xml_literal(html, DOM, Text) :-
    with_output_to(atom(Text),
                   html_write(current_output, DOM,
                              [ header(false),
                                layout(false)
                              ])).

%!  rdf_canonical_literal(++In, -Literal) is det.
%
%   Transform  a  relaxed  literal  specification   as  allowed  for
%   rdf_assert/3 into its canonical form. The following Prolog terms
%   are translated:
%
%   | **Prolog Term**               | **Datatype IRI** |
%   |:------------------------------|:-----------------|
%   | float                         | xsd:double       |
%   | integer                       | xsd:integer      |
%   | string                        | xsd:string       |
%   | `true` or `false`             | xsd:boolean      |
%   | date(Y,M,D)                   | xsd:date         |
%   | date_time(Y,M,D,HH,MM,SS)     | xsd:dateTime     |
%   | date_time(Y,M,D,HH,MM,SS,TZ)  | xsd:dateTime     |
%   | month_day(M,D)                | xsd:gMonthDay    |
%   | year_month(Y,M)               | xsd:gYearMonth   |
%   | time(HH,MM,SS)                | xsd:time         |
%
%   For example:
%
%     ```
%     ?- rdf_canonical_literal(42, X).
%     X = 42^^'http://www.w3.org/2001/XMLSchema#integer'.
%     ```

rdf_canonical_literal(In, Literal) :-
    ground(In),
    !,
    pre_ground_object(In, DBTerm),
    post_object(Literal, DBTerm).
rdf_canonical_literal(In, _) :-
    must_be(ground, In).

%!  rdf_lexical_form(++Literal, -Lexical:compound) is det.
%
%   True when Lexical is the lexical   form for the literal Literal.
%   Lexical is of one of the forms below. The ntriples serialization
%   is obtained by transforming String into a proper ntriples string
%   using double quotes and escaping where   needed and turning Type
%   into a proper IRI reference.
%
%     - String^^Type
%     - String@Lang

%       For example,
%
%       ==
%       ?- rdf_lexical_form(2.3^^xsd:double, L).
%       L = "2.3E0"^^'http://www.w3.org/2001/XMLSchema#double'.
%       ==

rdf_lexical_form(Literal, Lexical) :-
    pre_ground_object(Literal, literal(Lit0)),
    !,
    text_of0(Lit0, Lexical).
rdf_lexical_form(Literal, _) :-
    type_error(rdf_literal, Literal).

text_of0(type(TypeA, LexicalA), LexicalS^^TypeA) :-
    atom_string(LexicalA, LexicalS).
text_of0(lang(LangA, LexicalA), LexicalS@LangA) :-
    atom_string(LexicalA, LexicalS).


                 /*******************************
                 *       POST PROCESSING        *
                 *******************************/

:- rdf_meta
    post_object(o,o),
    out_type(r,-,+).

post_object(Val, _) :-
    ground(Val),
    !.                 % already specified and matched
post_object(URI, URI0) :-
    atom(URI0),
    !,
    URI = URI0.
post_object(Val@Lang, literal(lang(Lang, Val0))) :-
    nonvar(Lang),          % lang(Lang,Text) returns var(Lang) if no lang
    !,
    atom_string(Val0, Val).
post_object(Val^^Type, literal(type(Type, Val0))) :-
    !,
    out_type(Type, Val, Val0).
post_object(Val^^xsd:string, literal(Plain)) :-
    !,
    atomic(Plain),
    atom_string(Plain, Val).
post_object(Val@Lang, literal(_, lang(Lang, Val0))) :-
    nonvar(Lang),
    !,
    atom_string(Val0, Val).
post_object(Val^^Type, literal(_, type(Type, Val0))) :-
    !,
    out_type(Type, Val, Val0).
post_object(Val^^xsd:string, literal(_, Plain)) :-
    atomic(Plain),
    atom_string(Plain, Val).

out_type(xsd:string, Val, Val0) :-     % catches unbound type too
    !,
    atom_string(Val0, Val).
out_type(Type, Val, Val0) :-
    out_type_hook(Type, Val, Val0),
    !.
out_type(IntType, Val, Val0) :-
    xsd_numerical(IntType, _Domain, _BasicType),
    !,
    xsd_number_string(Val, Val0).
out_type(DateTimeType, Val, Val0) :-
    xsd_date_time_type(DateTimeType),
    !,
    out_date_time(DateTimeType, Val, Val0).
out_type(xsd:boolean, Val, Val0) :-
    !,
    Val = Val0.
out_type(rdf:'XMLLiteral', XML, DOM) :-
    xml_is_dom(DOM),
    !,
    with_output_to(string(XML),
                   xml_write(DOM, [header(false)])).
out_type(_Unknown, Val, Val0) :-
    atom_string(Val0, Val).


%!  out_date_time(+DateTimeType, -Val, +Val0) is det.
%
%   Translate an XSD lexical form for   a date/time related datatype
%   into the cannical form as defined by xsd_time_string/3.

out_date_time(Type, Prolog, Lexical) :-
    catch(xsd_time_string(Prolog, Type, Lexical),
          error(_,_),
          invalid_lexical_form_hook(Type, Lexical, Prolog)).


%!  invalid_lexical_form_hook(+Type, +Lexical, -Prolog)
%
%   This hook is called if translation of the lexical form to the Prolog
%   representation fails due to a syntax  error.   By  default it is not
%   defined, causing such invalid triples to be silently ignored.


                 /*******************************
                 *          ENUMERATION         *
                 *******************************/

%!  rdf_term(?Term) is nondet.
%
%   True if Term appears in the RDF database. Term is either an IRI,
%   literal or blank node and may  appear   in  any  position of any
%   triple. If Term is ground,  it   is  pre-processed as the object
%   argument of rdf_assert/3 and the predicate is _semidet_.

rdf_term(N) :-
    ground(N),
    !,
    pre_object(N, N0, _, _),
    visible_term(N0).
rdf_term(N) :-
    gen_term(N).

gen_term(N) :-
    resource(N),
    visible_term(N).
gen_term(O) :-                          % performs double conversion!
    rdf_literal(O),
    (rdf(_,_,O) -> true).

%!  rdf_literal(?Term) is nondet.
%
%   True if Term is a  known  literal.   If  Term  is  ground, it is
%   pre-processed as the object  argument   of  rdf_assert/3 and the
%   predicate is _semidet_.

rdf_literal(Term) :-
    ground(Term),
    !,
    pre_ground_object(Term, Object),
    (rdf_db:rdf(_,_,Object)->true).
rdf_literal(Term) :-
    pre_object(Term,literal(Lit0), _, _),
    rdf_db:rdf_current_literal(Lit0),
    (rdf_db:rdf(_,_,literal(Lit0))->true),
    post_object(Term, literal(Lit0)).

%!  rdf_bnode(?BNode) is nondet.
%
%   True if BNode is a currently known  blank node. The predicate is
%   _semidet_ if BNode is ground.

rdf_bnode(BNode) :-
    atom(BNode),
    !,
    current_bnode(BNode).
rdf_bnode(BNode) :-
    rdf_db:rdf_resource(BNode),
    current_bnode(BNode).

current_bnode(BNode) :-
    rdf_is_bnode(BNode),
    visible_node(BNode).            % Assumes BNodes cannot be predicates

%!  rdf_iri(?IRI) is nondet.
%
%   True if IRI is a current IRI.  The predicate is _semidet_ if IRI
%   is ground.

rdf_iri(IRI) :-
    atom(IRI),
    !,
    \+ rdf_is_bnode(IRI),
    visible_term(IRI).
rdf_iri(IRI) :-
    resource(IRI),
    \+ rdf_is_bnode(IRI),
    visible_term(IRI).

%!  rdf_name(?Name) is nondet.
%
%   True if Name is a  current  IRI   or  literal.  The predicate is
%   _semidet_ if Name is ground.

rdf_name(Name) :-
    atom(Name), \+ boolean(Name),
    !,
    \+ rdf_is_bnode(Name),
    visible_term(Name).
rdf_name(Name) :-
    ground(Name),
    !,
    pre_ground_object(Name, Name0),
    (rdf_db:rdf(_,_,Name0)->true).
rdf_name(Name) :-
    rdf_iri(Name).
rdf_name(Name) :-
    rdf_literal(Name).

%!  rdf_subject(?S) is nondet.
%
%   True when S is a currently known   _subject_, i.e. it appears in
%   the subject position of some visible   triple.  The predicate is
%   _semidet_ if S is ground.


%!  rdf_predicate(?P) is nondet.
%
%   True when P is a currently known   predicate, i.e. it appears in
%   the predicate position of some visible  triple. The predicate is
%   _semidet_ if P is ground.

rdf_predicate(P) :-
    atom(P),
    !,
    (rdf(_,P,_) -> true).
rdf_predicate(P) :-
    rdf_db:rdf_current_predicate(P),
    (rdf(_,P,_) -> true).

%!  rdf_object(?O) is nondet.
%
%   True when O is a currently known  object, i.e. it appears in the
%   object position of some visible triple. If Term is ground, it is
%   pre-processed as the object  argument   of  rdf_assert/3 and the
%   predicate is _semidet_.

rdf_object(O) :-
    ground(O),
    !,
    (   atom(O), \+ boolean(O)
    ->  (rdf_db:rdf(_,_,O) -> true)
    ;   rdf_literal(O)
    ).
rdf_object(O) :-
    rdf_db:rdf_resource(O),
    (rdf_db:rdf(_,_,O) -> true).
rdf_object(O) :-
    rdf_literal(O).

%!  rdf_node(?T) is nondet.
%
%   True when T appears in the subject or object position of a known
%   triple, i.e., is a node in the RDF graph.

rdf_node(N) :-
    var(N),
    !,
    gen_node(N).
rdf_node(N) :-
    pre_ground_object(N, N0),
    visible_node(N0).

gen_node(N) :-
    rdf_db:rdf_resource(N),
    visible_node(N).
gen_node(O) :-                          % performs double conversion!
    rdf_literal(O),
    (rdf(_,_,O) -> true).

%!  resource(?R)
%
%   True if R is a node that is not a literal. Note that RDF-DB does
%   not necessarily include predicates in the set of resources. Also
%   note that the resource may not really exist or be visible.

resource(R) :-
    var(R),
    !,
    gen_resource(R).
resource(R) :-
    rdf_db:rdf_resource(R),
    !.
resource(R) :-
    rdf_db:rdf_current_predicate(R),
    !.

gen_resource(R) :-
    rdf_db:rdf_resource(R).
gen_resource(R) :-
    rdf_db:rdf_current_predicate(R),
    \+ rdf_db:rdf_resource(R).

visible_node(Term) :-
    atom(Term),
    !,
    (   rdf_db:rdf(Term,_,_)
    ;   rdf_db:rdf(_,_,Term)
    ),
    !.
visible_node(Term) :-
    rdf_db:rdf(_,_,Term).

visible_term(Term) :-
    atom(Term),
    !,
    (   rdf_db:rdf(Term,_,_)
    ;   rdf_db:rdf(_,Term,_)
    ;   rdf_db:rdf(_,_,Term)
    ),
    !.
visible_term(Term) :-
    rdf_db:rdf(_,_,Term).

%!  rdf_create_bnode(--BNode)
%
%   Create a new BNode. A  blank  node   is  an  atom  starting with
%   =|_:|=. Blank nodes generated by this  predicate are of the form
%   =|_:genid|= followed by a unique integer.

rdf_create_bnode(BNode) :-
    var(BNode),
    !,
    rdf_db:rdf_bnode(BNode).
rdf_create_bnode(BNode) :-
    uninstantiation_error(BNode).


                 /*******************************
                 *         TYPE CHECKING        *
                 *******************************/

%!  rdf_is_iri(@IRI) is semidet.
%
%   True if IRI is an RDF IRI term.
%
%   For performance reasons, this does not check for compliance to
%   the syntax defined in [[RFC
%   3987][http://www.ietf.org/rfc/rfc3987.txt]].  This checks
%   whether the term is (1) an atom and (2) not a blank node
%   identifier.
%
%   Success of this goal does not imply that the IRI is present in
%   the database (see rdf_iri/1 for that).

rdf_is_iri(IRI) :-
    atom(IRI),
    \+ rdf_is_bnode(IRI).

%!  rdf_is_bnode(@Term) is semidet.
%
%   True if Term is an RDF blank node identifier.
%
%   A blank node is represented by an atom that starts with
%   =|_:|=.
%
%   Success of this goal does not imply that the blank node is
%   present in the database (see rdf_bnode/1 for that).
%
%   For backwards compatibility, atoms that are represented with
%   an atom that starts with =|__|= are also considered to be a
%   blank node.


%!  rdf_is_literal(@Term) is semidet.
%
%   True if Term is an RDF literal term.
%
%   An RDF literal term is of the form `String@LanguageTag` or
%   `Value^^Datatype`.
%
%   Success of this goal does not imply that the literal is
%   well-formed or that it is present in the database (see
%   rdf_literal/1 for that).

rdf_is_literal(Literal) :-
    literal_form(Literal),
    !,
    ground(Literal).

literal_form(_@_).
literal_form(_^^_).


%!  rdf_is_name(@Term) is semidet.
%
%   True if Term is an RDF Name, i.e., an IRI or literal.
%
%   Success of this goal does not imply that the name is
%   well-formed or that it is present in the database (see
%   rdf_name/1 for that).

rdf_is_name(T) :- rdf_is_iri(T), !.
rdf_is_name(T) :- rdf_is_literal(T).


%!  rdf_is_object(@Term) is semidet.
%
%   True if Term can appear in the object position of a triple.
%
%   Success of this goal does not imply that the object term in
%   well-formed or that it is present in the database (see
%   rdf_object/1 for that).
%
%   Since any RDF term can appear in the object position, this is
%   equaivalent to rdf_is_term/1.

rdf_is_object(T) :- rdf_is_subject(T), !.
rdf_is_object(T) :- rdf_is_literal(T).


%!  rdf_is_predicate(@Term) is semidet.
%
%   True if Term can appear in the   predicate position of a triple.
%
%   Success of this goal does not imply that the predicate term is
%   present in the database (see rdf_predicate/1 for that).
%
%   Since only IRIs can appear in the predicate position, this is
%   equivalent to rdf_is_iri/1.

rdf_is_predicate(T) :- rdf_is_iri(T).


%!  rdf_is_subject(@Term) is semidet.
%
%   True if Term can appear in  the   subject  position of a triple.
%
%   Only blank nodes and IRIs can appear in the subject position.
%
%   Success of this goal does not imply that the subject term is
%   present in the database (see rdf_subject/1 for that).
%
%   Since blank nodes are represented by atoms that start with
%   `_:` and an IRIs are atoms as well, this is equivalent to
%   atom(Term).

rdf_is_subject(T) :- atom(T).

%!  rdf_is_term(@Term) is semidet.
%
%   True if Term can be used as an RDF term, i.e., if Term is
%   either an IRI, a blank node or an RDF literal.
%
%   Success of this goal does not imply that the RDF term is
%   present in the database (see rdf_term/1 for that).

rdf_is_term(N) :- rdf_is_subject(N), !.
rdf_is_term(N) :- rdf_is_literal(N).


                 /*******************************
                 *          COLLECTIONS         *
                 *******************************/

%!  rdf_list(?RDFTerm) is semidet.
%
%   True if RDFTerm is a proper RDF   list.  This implies that every
%   node in the list has an  `rdf:first` and `rdf:rest` property and
%   the list ends in `rdf:nil`.
%
%   If RDFTerm is unbound, RDFTerm is   bound  to each _maximal_ RDF
%   list. An RDF list is _maximal_  if   there  is  no triple rdf(_,
%   rdf:rest, RDFList).

rdf_list(L) :-
    var(L),
    !,
    rdf_has(L, rdf:first, _),
    \+ rdf_has(_, rdf:rest, L),
    rdf_list_g(L).
rdf_list(L) :-
    rdf_list_g(L),
    !.

:- rdf_meta
    rdf_list_g(r).

rdf_list_g(rdf:nil) :- !.
rdf_list_g(L) :-
    once(rdf_has(L, rdf:first, _)),
    rdf_has(L, rdf:rest, Rest),
    (   rdf_equal(rdf:nil, Rest)
    ->  true
    ;   rdf_list_g(Rest)
    ).


%!  rdf_list(+RDFList, -PrologList) is det.
%
%   True when PrologList represents the   rdf:first  objects for all
%   cells in RDFList. Note that  this   can  be non-deterministic if
%   cells have multiple rdf:first or rdf:rest triples.

rdf_list(RDFList, Prolog) :-
    rdf_is_subject(RDFList),
    !,
    rdf_list_to_prolog(RDFList, Prolog).
rdf_list(RDFList, _Prolog) :-
    type_error(rdf_subject, RDFList).

:- rdf_meta
    rdf_list_to_prolog(r,-).

rdf_list_to_prolog(rdf:nil, Prolog) :-
    !,
    Prolog = [].
rdf_list_to_prolog(RDF, [H|T2]) :-
    (   rdf_has(RDF, rdf:first, H0),
        rdf_has(RDF, rdf:rest, T1)
    *-> H = H0,
        rdf_list_to_prolog(T1, T2)
    ;   type_error(rdf_list, RDF)
    ).


%!  rdf_length(+RDFList, -Length:nonneg) is nondet.
%
%   True when Length is the number of  cells in RDFList. Note that a
%   list cell may have multiple rdf:rest   triples, which makes this
%   predicate  non-deterministic.  This  predicate  does  not  check
%   whether the list cells have   associated values (rdf:first). The
%   list must end in rdf:nil.

rdf_length(RDFList, Len) :-
    rdf_is_subject(RDFList),
    !,
    rdf_length(RDFList, 0, Len).

:- rdf_meta
    rdf_length(r,+,-).

rdf_length(rdf:nil, Len, Len) :- !.
rdf_length(RDF, Len0, Len) :-
    (   rdf_has(RDF, rdf:rest, T)
    *-> Len1 is Len0+1,
        rdf_length(T, Len1, Len)
    ;   type_error(rdf_list, RDF)
    ).


%!  rdf_member(?Member, +RDFList) is nondet.
%
%   True when Member is a member of RDFList

rdf_member(M, L) :-
    ground(M),
    !,
    (   rdf_member2(M, L)
    ->  true
    ).
rdf_member(M, L) :-
    rdf_member2(M, L).

rdf_member2(M, L) :-
    rdf_has(L, rdf:first, M).
rdf_member2(M, L) :-
    rdf_has(L, rdf:rest, L1),
    rdf_member2(M, L1).


%! rdf_nextto(?X, ?Y) is nondet.
%! rdf_nextto(?X, ?Y, ?RdfList) is nondet.
%
%       True if Y directly follows X in RdfList.

rdf_nextto(X, Y) :-
    distinct(X-Y, rdf_nextto(X, Y, _)).


rdf_nextto(X, Y, L) :-
    var(X), ground(Y),
    !,
    rdf_nextto(Y, X, L).
rdf_nextto(X, Y, L) :-
    rdf_has(L, rdf:first, X),
    rdf_has(L, rdf:rest, T),
    rdf_has(T, rdf:first, Y).


%!  rdf_nth0(?Index, +RDFList, ?X) is nondet.
%!  rdf_nth1(?Index, +RDFList, ?X) is nondet.
%
%   True when X is the Index-th element (0-based or 1-based) of
%   RDFList.  This predicate is deterministic if Index is given and
%   the list has no multiple rdf:first or rdf:rest values.

rdf_nth0(I, L, X) :-
    rdf_nth(0, I, L, X).

rdf_nth1(I, L, X) :-
    rdf_nth(1, I, L, X).

rdf_nth(Offset, I, L, X) :-
    rdf_is_subject(L),
    !,
    (   var(I)
    ->  true
    ;   must_be(nonneg, I)
    ),
    rdf_nth_(I, Offset, L, X).
rdf_nth(_, _, L, _) :-
    type_error(rdf_subject, L).

rdf_nth_(I, I0, L, X) :-
    (   I0 == I
    ->  !
    ;   I0 = I
    ),
    rdf_has(L, rdf:first, X).
rdf_nth_(I, I0, L, X) :-
    rdf_has(L, rdf:rest, T),
    I1 is I0+1,
rdf_nth_(I, I1, T, X).


%!  rdf_last(+RDFList, -Last) is det.
%
%   True when Last is the last element  of RDFList. Note that if the
%   last cell has multiple rdf:first triples, this predicate becomes
%   nondet.

rdf_last(L, Last) :-
    rdf_is_subject(L),
    !,
    rdf_has(L, rdf:rest, T),
    (   rdf_equal(T, rdf:nil)
    ->  rdf_has(L, rdf:first, Last)
    ;   rdf_last(T, Last)
    ).
rdf_last(L, _) :-
    type_error(rdf_subject, L).


%!  rdf_estimate_complexity(?S, ?P, ?O, -Estimate) is det.

rdf_estimate_complexity(S, P, O, Estimate) :-
    pre_object(O,O0,S,P),
    rdf_db:rdf_estimate_complexity(S,P,O0,Estimate).


%!  rdf_assert_list(+PrologList, ?RDFList) is det.
%!  rdf_assert_list(+PrologList, ?RDFList, +Graph) is det.
%
%   Create an RDF list from the   given Prolog List. PrologList must
%   be a proper Prolog list and  all   members  of  the list must be
%   acceptable as object for rdf_assert/3. If RDFList is unbound and
%   PrologList is not empty, rdf_create_bnode/1   is  used to create
%   RDFList.

rdf_assert_list(Prolog, RDF) :-
    rdf_default_graph(G),
    rdf_assert_list(Prolog, RDF, G).

rdf_assert_list(Prolog, RDF, G) :-
    must_be(list, Prolog),
    rdf_transaction(rdf_assert_list_(Prolog, RDF, G)).

rdf_assert_list_([], Nil, _) :-
    rdf_equal(rdf:nil, Nil).
rdf_assert_list_([H|T], L2, G) :-
    (var(L2) -> rdf_create_bnode(L2) ; true),
    rdf_assert(L2, rdf:type, rdf:'List', G),
    rdf_assert(L2, rdf:first, H, G),
    (   T == []
    ->  rdf_assert(L2, rdf:rest, rdf:nil, G)
    ;   rdf_create_bnode(T2),
        rdf_assert(L2, rdf:rest, T2, G),
        rdf_assert_list_(T, T2, G)
    ).


%!  rdf_retract_list(+RDFList) is det.
%
%   Retract the rdf:first, rdf:rest  and rdf:type=rdf:'List' triples
%   from all nodes  reachable  through   rdf:rest.  Note  that other
%   triples that exist on the nodes are left untouched.

rdf_retract_list(L) :-
    rdf_is_subject(L),
    !,
    rdf_transaction(rdf_retract_list_(L)).
rdf_retract_list(L) :-
    type_error(rdf_subject, L).

:- rdf_meta
    rdf_retract_list_(r).

rdf_retract_list_(rdf:nil) :- !.
rdf_retract_list_(L) :-
    rdf_retractall(L, rdf:first, _),
    forall(rdf_has(L, rdf:rest, L1),
           rdf_retract_list_(L1)),
    rdf_retractall(L, rdf:rest, _),
    rdf_retractall(L, rdf:type, rdf:'List').
