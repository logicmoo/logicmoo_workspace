/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(rdf_prefixes,
          [ rdf_prefix/2,               % :Alias, +URI
            rdf_current_prefix/2,       % :Alias, ?URI
            rdf_register_prefix/2,      % +Alias, +URI
            rdf_register_prefix/3,      % +Alias, +URI, +Options
            rdf_unregister_prefix/1,    % +Alias
            register_file_prefixes/1,   % +Pairs

            rdf_current_ns/2,           % :Alias, ?URI
            rdf_register_ns/2,          % +Alias, +URI
            rdf_register_ns/3,          % +Alias, +URI, +Options
            rdf_global_id/2,            % ?NS:Name, :Global
            rdf_global_object/2,        % +Object, :NSExpandedObject
            rdf_global_term/2,          % +Term, :WithExpandedNS

            (rdf_meta)/1,               % +Heads
            op(1150, fx, (rdf_meta))
          ]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(lists),[member/2]).
:- autoload(library(option),[option/3]).
:- autoload(library(pairs),[map_list_to_pairs/3,pairs_values/2]).

:- meta_predicate
    rdf_current_prefix(:, -),
    rdf_current_ns(:, -),
    rdf_global_id(?, :),
    rdf_global_term(+, :),
    rdf_global_object(+, :).

/** <module> RDF prefixes management

This module defines the expansion of  `Prefix:Local` terms to full IRIs.
This library is typically not  intended  for   the  end-user.  It may be
included into other RDF and XML  libraries   and  relevant  parts may be
re-exported.
*/

:- predicate_options(rdf_register_ns/3, 3,
                     [ force(boolean),
                       keep(boolean)
                     ]).
:- predicate_options(rdf_register_prefix/3, 3,
                     [ force(boolean),
                       keep(boolean)
                     ]).


		 /*******************************
		 *            HOOKS		*
		 *******************************/

%!  rdf_empty_prefix_cache(+Alias, +URI)
%
%   Multifile hook called if the binding Alias   -> URI is modified. May
%   be used to update derived caches.

:- multifile
    rdf_empty_prefix_cache/2.

% the ns/2 predicate is historically  defined   in  `rdf_db`. We'll keep
% that for compatibility reasons.
:- multifile rdf_db:ns/2.
:- dynamic   rdf_db:ns/2.               % ID, URL

%!  rdf_current_prefix(:Alias, ?URI) is nondet.
%
%   Query   predefined   prefixes   and    prefixes   defined   with
%   rdf_register_prefix/2   and   local   prefixes    defined   with
%   rdf_prefix/2. If Alias is unbound and one   URI is the prefix of
%   another, the longest is returned first.   This  allows turning a
%   resource into a prefix/local couple using the simple enumeration
%   below. See rdf_global_id/2.
%
%     ==
%     rdf_current_prefix(Prefix, Expansion),
%     atom_concat(Expansion, Local, URI),
%     ==

rdf_current_prefix(Module:Alias, URI) :-
    nonvar(Alias),
    !,
    rdf_current_prefix(Module, Alias, URI),
    !.
rdf_current_prefix(Module:Alias, URI) :-
    rdf_current_prefix(Module, Alias, URI).

rdf_current_prefix(system, Alias, URI) :-
    !,
    rdf_db:ns(Alias, URI).
rdf_current_prefix(Module, Alias, URI) :-
    default_module(Module, M),
    (   M == system
    ->  rdf_db:ns(Alias, URI)
    ;   '$flushed_predicate'(M:'rdf prefix'(_,_)),
        call(M:'rdf prefix'(Alias,URI))
    ).

%!  rdf_prefix(:Alias, +URI) is det.
%
%   Register a _local_ prefix.  This   declaration  takes precedence
%   over globally defined prefixes   using  rdf_register_prefix/2,3.
%   Module local prefixes are notably required   to deal with SWISH,
%   where users need to  be  able   to  have  independent  namespace
%   declarations.

rdf_prefix(Alias, URI) :-
    throw(error(context_error(nodirective, rdf_prefix(Alias, URI)), _)).

system:term_expansion((:- rdf_prefix(AliasSpec, URI)), Clauses) :-
    prolog_load_context(module, Module),
    strip_module(Module:AliasSpec, TM, Alias),
    must_be(atom, Alias),
    must_be(atom, URI),
    (   rdf_current_prefix(TM:Alias, URI)
    ->  Clauses = []
    ;   TM == Module
    ->  Clauses = 'rdf prefix'(Alias, URI)
    ;   Clauses = TM:'rdf prefix'(Alias, URI)
    ).

%!  rdf_db:ns(?Alias, ?URI) is nondet.
%
%   Dynamic and multifile predicate that   maintains  the registered
%   namespace aliases.
%
%   @deprecated New code  must  modify   the  namespace  table using
%   rdf_register_ns/3 and query using rdf_current_ns/2.

rdf_db:ns(dc,      'http://purl.org/dc/elements/1.1/').
rdf_db:ns(dcterms, 'http://purl.org/dc/terms/').
rdf_db:ns(eor,     'http://dublincore.org/2000/03/13/eor#').
rdf_db:ns(foaf,    'http://xmlns.com/foaf/0.1/').
rdf_db:ns(owl,     'http://www.w3.org/2002/07/owl#').
rdf_db:ns(rdf,     'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_db:ns(rdfs,    'http://www.w3.org/2000/01/rdf-schema#').
rdf_db:ns(serql,   'http://www.openrdf.org/schema/serql#').
rdf_db:ns(skos,    'http://www.w3.org/2004/02/skos/core#').
rdf_db:ns(void,    'http://rdfs.org/ns/void#').
rdf_db:ns(xsd,     'http://www.w3.org/2001/XMLSchema#').

%!  rdf_register_prefix(+Prefix, +URI) is det.
%!  rdf_register_prefix(+Prefix, +URI, +Options) is det.
%
%   Register Prefix as an abbreviation for URI. Options:
%
%           * force(Boolean)
%           If =true=, replace existing namespace alias. Please note
%           that replacing a namespace is dangerous as namespaces
%           affect preprocessing. Make sure all code that depends on
%           a namespace is compiled after changing the registration.
%
%           * keep(Boolean)
%           If =true= and Alias is already defined, keep the
%           original binding for Prefix and succeed silently.
%
%   Without options, an attempt  to  redefine   an  alias  raises  a
%   permission error.
%
%   Predefined prefixes are:
%
%   | **Alias** | **IRI prefix**                              |
%   | dc        | http://purl.org/dc/elements/1.1/            |
%   | dcterms   | http://purl.org/dc/terms/                   |
%   | eor       | http://dublincore.org/2000/03/13/eor#       |
%   | foaf      | http://xmlns.com/foaf/0.1/                  |
%   | owl       | http://www.w3.org/2002/07/owl#              |
%   | rdf       | http://www.w3.org/1999/02/22-rdf-syntax-ns# |
%   | rdfs      | http://www.w3.org/2000/01/rdf-schema#       |
%   | serql     | http://www.openrdf.org/schema/serql#        |
%   | skos      | http://www.w3.org/2004/02/skos/core#        |
%   | void      | http://rdfs.org/ns/void#                    |
%   | xsd       | http://www.w3.org/2001/XMLSchema#           |


rdf_register_prefix(Alias, URI) :-
    rdf_register_prefix(Alias, URI, []).

rdf_register_prefix(Alias, URI, Options) :-
    must_be(atom, Alias),
    must_be(atom, URI),
    (   rdf_current_prefix(system:Alias, URI)
    ->  true
    ;   register_global_prefix(Alias, URI, Options)
    ).

%!  register_global_prefix(+Alias, +URI, +Options)
%
%   Register a global prefix.

register_global_prefix(Alias, URI, Options) :-
    rdf_db:ns(Alias, _),
    !,
    (   option(force(true), Options, false)
    ->  retractall(rdf_db:ns(Alias, _)),
        rdf_register_prefix(Alias, URI, Options),
        forall(rdf_empty_prefix_cache(Alias, URI), true)
    ;   option(keep(true), Options, false)
    ->  true
    ;   throw(error(permission_error(register, namespace, Alias),
                    context(_, 'Already defined')))
    ).
register_global_prefix(Alias, URI, _) :-
    findall(P-U, prefix_conflict(URI, P, U), Pairs),
    order_prefixes([Alias-URI|Pairs], Ordered),
    forall(member(P-U, Pairs), retract(rdf_db:ns(P,U))),
    forall(member(P-U, Ordered), assert(rdf_db:ns(P,U))).

prefix_conflict(URI, P, U) :-
    rdf_db:ns(P,U),
    (   sub_atom(URI, 0, _, _, U)
    ->  true
    ;   sub_atom(U, 0, _, _, URI)
    ).

order_prefixes(Pairs, Sorted) :-
    map_list_to_pairs(prefix_uri_length, Pairs, ByLen),
    sort(1, >=, ByLen, SortedByLen),
    pairs_values(SortedByLen, Sorted).

prefix_uri_length(_-URI, Len) :-
    atom_length(URI, Len).

%!  rdf_unregister_prefix(+Alias) is det.
%
%   Delete a prefix global registration.

rdf_unregister_prefix(Alias) :-
    must_be(atom, Alias),
    retractall(rdf_db:ns(Alias, _)).


%!  rdf_current_ns(:Prefix, ?URI) is nondet.
%
%   @deprecated  Use rdf_current_prefix/2.

rdf_current_ns(Prefix, URI) :-
    rdf_current_prefix(Prefix, URI).

%!  rdf_register_ns(:Prefix, ?URI) is det.
%!  rdf_register_ns(:Prefix, ?URI, +Options) is det.
%
%   Register an RDF prefix.
%
%   @deprecated  Use rdf_register_prefix/2 or rdf_register_prefix/3.

rdf_register_ns(Prefix, URI) :-
    rdf_register_prefix(Prefix, URI).
rdf_register_ns(Prefix, URI, Options) :-
    rdf_register_prefix(Prefix, URI, Options).


%!  register_file_prefixes(+Map:list(pair)) is det.
%
%   Register a namespace as encounted in   the  namespace list of an
%   RDF document. We only register if  both the abbreviation and URL
%   are not already known. Is there a   better  way? This code could
%   also do checks on the consistency   of  RDF and other well-known
%   namespaces.
%
%   @tbd    Better error handling

register_file_prefixes([]) :- !.
register_file_prefixes([Decl|T]) :-
    !,
    register_file_prefixes(Decl),
    register_file_prefixes(T).
register_file_prefixes([]=_) :- !.      % xmlns= (overall default)
register_file_prefixes(NS=URL) :-       % compatibility
    !,
    register_file_prefixes(NS-URL).
register_file_prefixes(NS-URL) :-
    (   rdf_db:ns(NS, URL)
    ->  true
    ;   rdf_db:ns(NS, _)
    ->  true                            % redefined abbreviation
    ;   rdf_db:ns(_, URL)
    ->  true                            % redefined URL
    ;   rdf_register_prefix(NS, URL)
    ).


%!  rdf_global_id(?IRISpec, :IRI) is semidet.
%
%   Convert between Prefix:Local and full IRI   (an atom). If IRISpec is
%   an atom, it  is  simply  unified   with  IRI.  This  predicate fails
%   silently if IRI is an RDF literal.
%
%   Note that this predicate is a meta-predicate on its output argument.
%   This is necessary to get the module context while the first argument
%   may be of the form (:)/2. The above mode description is correct, but
%   should be interpreted as (?,?).
%
%   @error existence_error(rdf_prefix, Prefix)
%   @see   rdf_equal/2 provides a compile time alternative
%   @see   The rdf_meta/1 directive asks for compile time expansion
%          of arguments.
%   @bug   Error handling is incomplete.  In its current implementation
%	   the same code is used for compile-time expansion and to
%	   facilitate runtime conversion and checking.  These use cases
%	   have different requirements.

rdf_global_id(Id, Module:Global) :-
    rdf_global_id(Id, Global, Module).

rdf_global_id(NS:Local, Global, Module) :-
    global(NS, Local, Global, Module),
    !.
rdf_global_id(Global, Global, _).


%!  rdf_global_object(+Object, :GlobalObject) is semidet.
%!  rdf_global_object(-Object, :GlobalObject) is semidet.
%
%   Same as rdf_global_id/2,  but  intended   for  dealing  with the
%   object part of a  triple,  in   particular  the  type  for typed
%   literals. Note that the predicate  is   a  meta-predicate on the
%   output argument. This is necessary  to   get  the module context
%   while the first argument may be of the form (:)/2.
%
%   @error  existence_error(rdf_prefix, Prefix)

rdf_global_object(Object, Module:GlobalObject) :-
    rdf_global_object(Object, GlobalObject, Module).

rdf_global_object(Var, Global, _M) :-
    var(Var),
    !,
    Global = Var.
rdf_global_object(Prefix:Local, Global, M) :-
    global(Prefix, Local, Global, M),
    !.
rdf_global_object(literal(type(Prefix:Local, Value)),
                  literal(type(Global, Value)), M) :-
    global(Prefix, Local, Global, M),
    !.
rdf_global_object(^^(Value,Prefix:Local),
                  ^^(Value,Global), M) :-
    global(Prefix, Local, Global, M),
    !.
rdf_global_object(literal(Query0, type(Prefix:Local, Value)),
                  literal(Query1, type(Global, Value)), M) :-
    global(Prefix, Local, Global, M),
    !,
    rdf_global_term(Query0, Query1, M).
rdf_global_object(literal(Query0, Value),
                  literal(Query1, Value), M) :-
    !,
    rdf_global_term(Query0, Query1, M).
rdf_global_object(Global, Global, _).

global(Prefix, Local, Global, Module) :-
    (   atom(Global)
    ->  rdf_current_prefix(Module:Prefix, Full),
        atom_concat(Full, Local, Global)
    ;   atom(Prefix), atom(Local), var(Global)
    ->  (   rdf_current_prefix(Module:Prefix, Full)
        *-> atom_concat(Full, Local, Global)
        ;   current_prolog_flag(xref, true)
        ->  Global = Prefix:Local
        ;   existence_error(rdf_prefix, Prefix)
        )
    ).


%!  rdf_global_term(+TermIn, :GlobalTerm) is det.
%
%   Performs rdf_global_id/2 on predixed IRIs and rdf_global_object/2 on
%   RDF literals, by recursively  analysing  the   term.  Note  that the
%   predicate is a meta-predicate  on  the   output  argument.  This  is
%   necessary to get the module context while  the first argument may be
%   of the form (:)/2.
%
%   Terms of the form `Prefix:Local`  that   appear  in TermIn for which
%   `Prefix` is not defined are not replaced. Unlike rdf_global_id/2 and
%   rdf_global_object/2, no error is raised.

rdf_global_term(TermIn, Module:TermOut) :-
    rdf_global_term(TermIn, TermOut, Module).

rdf_global_term(Var, Var, _M) :-
    var(Var),
    !.
rdf_global_term(Prefix:Local, Global, Module) :-
    atom(Prefix), atom(Local),
    rdf_current_prefix(Module:Prefix, Full),
    !,
    atom_concat(Full, Local, Global).
rdf_global_term([H0|T0], [H|T], M) :-
    !,
    rdf_global_term(H0, H, M),
    rdf_global_term(T0, T, M).
rdf_global_term(Term0, Term, M) :-
    compound(Term0),
    !,
    Term0 =.. [H|L0],
    rdf_global_term(L0, L, M),
    Term =.. [H|L].
rdf_global_term(Term, Term, _).

%!  rdf_global_graph(+TermIn, -GlobalTerm, +Module) is det.
%
%   Preforms rdf_global_id/2 on rdf/4, etc graph arguments

rdf_global_graph(Prefix:Local, Global, Module) :-
    atom(Prefix), atom(Local),
    !,
    global(Prefix, Local, Global, Module).
rdf_global_graph(G, G, _).


                 /*******************************
                 *            EXPANSION         *
                 *******************************/

:- multifile
    system:term_expansion/2,
    system:goal_expansion/2.

system:term_expansion((:- rdf_meta(Heads)), Clauses) :-
    prolog_load_context(module, M),
    phrase(mk_clauses(Heads, M), Clauses).

mk_clauses((A,B), M) -->
    mk_clause(A, M),
    mk_clauses(B, M).
mk_clauses(A, M) -->
    mk_clause(A, M).

mk_clause(Head0, M0) -->
    { strip_module(M0:Head0, Module, Head),
      valid_rdf_meta_head(Head),
      functor(Head, Name, Arity),
      functor(Unbound, Name, Arity),
      qualify(Module, 'rdf meta specification'/2, Decl)
    },
    [ (:- multifile(Decl)),
      Module:'rdf meta specification'(Unbound, Head)
    ].

qualify(Module, Decl, Decl) :-
    prolog_load_context(module, Module),
    !.
qualify(Module, Decl, Module:Decl).


valid_rdf_meta_head(Head) :-
    callable(Head),
    !,
    Head =.. [_|Args],
    valid_args(Args).
valid_rdf_meta_head(Head) :-
    throw(error(type_error(callable, Head), _)).

valid_args([]).
valid_args([H|T]) :-
    valid_arg(H),
    !,
    valid_args(T).

valid_arg(:).                           % meta argument
valid_arg(+).                           % non-var
valid_arg(-).                           % var
valid_arg(?).                           % either var or non-var
valid_arg(@).                           % not modified
valid_arg(r).                           % RDF resource
valid_arg(o).                           % RDF object
valid_arg(t).                           % term with RDF resources
valid_arg(g).                           % graph argument
valid_arg(A) :-
    throw(error(type_error(rdf_meta_argument, A), _)).

%!  rdf_meta(+Heads)
%
%   This  directive  defines  the  argument    types  of  the  named
%   predicates, which will force compile   time  namespace expansion
%   for these predicates. Heads is a coma-separated list of callable
%   terms. Defined argument properties are:
%
%     $ : :
%     Argument is a goal. The goal is processed using expand_goal/2,
%     recursively applying goal transformation on the argument.
%
%     $ + :
%     The argument is instantiated at entry. Nothing is changed.
%
%     $ - :
%     The argument is not instantiated at entry. Nothing is changed.
%
%     $ ? :
%     The argument is unbound or instantiated at entry. Nothing is
%     changed.
%
%     $ @ :
%     The argument is not changed.
%
%     $ r :
%     The argument must be a resource. If it is a term
%     _prefix_:_local_ it is translated.
%
%     $ o :
%     The argument is an object or resource. See
%     rdf_global_object/2.
%
%     $ t :
%     The argument is a term that must be translated. Expansion will
%     translate all occurences of _prefix_:_local_ appearing
%     anywhere in the term. See rdf_global_term/2.
%
%   As it is subject to term_expansion/2, the rdf_meta/1 declaration
%   can only be used as a directive. The directive must be processed
%   before the definition of  the  predicates   as  well  as  before
%   compiling code that  uses  the   rdf  meta-predicates.  The atom
%   =rdf_meta=  is  declared   as   an    operator   exported   from
%   library(semweb/rdf_db). Files using rdf_meta/1  must explicitely
%   load this library.
%
%   Beginning with SWI-Prolog 7.3.17, the   low-level  RDF interface
%   (rdf/3,  rdf_assert/3,  etc.)  perform    runtime  expansion  of
%   `Prefix:Local` terms. This eliminates the   need  for rdf_meta/1
%   for  simple  cases.  However,  runtime   expansion  comes  at  a
%   significant overhead and having two  representations for IRIs (a
%   plain atom and  a  term   `Prefix:Local`)  implies  that  simple
%   operations such as comparison of IRIs   no  longer map to native
%   Prolog operations such as `IRI1 == IRI2`.

rdf_meta(Heads) :-
    throw(error(context_error(nodirective, rdf_meta(Heads)), _)).

%!  rdf_meta_specification(+General, +Module, -Spec) is semidet.
%
%   True when Spec is the RDF meta specification for Module:General.
%
%   @arg    General is the term Spec with all arguments replaced with
%           variables.

rdf_meta_specification(Unbounded, Module, Spec) :-
    '$flushed_predicate'(Module:'rdf meta specification'(_,_)),
    call(Module:'rdf meta specification'(Unbounded, Spec)).

system:goal_expansion(G, Expanded) :-
    \+ predicate_property(G, iso),
    prolog_load_context(module, LM),
    predicate_property(LM:G, implementation_module(IM)),
    rdf_meta_specification(G, IM, Spec),
    rdf_expand(G, Spec, Expanded, LM).

system:term_expansion(Module:Fact, Expanded) :-
    atom(Module),
    rdf_meta_specification(Fact, Module, Spec),
    rdf_expand(Fact, Spec, ExpandedFact, Module),
    Fact \== ExpandedFact,
    Expanded = (Module:ExpandedFact).
system:term_expansion(Fact, Expanded) :-
    prolog_load_context(module, Module),
    rdf_meta_specification(Fact, Module, Spec),
    rdf_expand(Fact, Spec, Expanded, Module),
    Fact \== Expanded.
system:term_expansion((Module:Head :- Body), (Expanded :- Body)) :-
    atom(Module),
    rdf_meta_specification(Head, Module, Spec),
    rdf_expand(Head, Spec, ExpandedHead, Module),
    Head \== ExpandedHead,
    Expanded = (Module:ExpandedHead).
system:term_expansion((Head :- Body), (Expanded :- Body)) :-
    prolog_load_context(module, Module),
    rdf_meta_specification(Head, Module, Spec),
    rdf_expand(Head, Spec, Expanded, Module),
    Head \== Expanded.

rdf_expand(G, Spec, Expanded, M) :-
    functor(G, Name, Arity),
    functor(Expanded, Name, Arity),
    rdf_expand_args(0, Arity, G, Spec, Expanded, M).

rdf_expand_args(Arity, Arity, _, _, _, _) :- !.
rdf_expand_args(I0, Arity, Goal, Spec, Expanded, M) :-
    I is I0 + 1,
    arg(I, Goal, GA),
    arg(I, Spec, SA),
    arg(I, Expanded, EA),
    rdf_expand_arg(SA, GA, EA, M),
    rdf_expand_args(I, Arity, Goal, Spec, Expanded, M).

rdf_expand_arg(r, A, E, M) :-
    mk_global(A, E, M),
    !.
rdf_expand_arg(o, A, E, M) :-
    rdf_global_object(A, E, M),
    !.
rdf_expand_arg(t, A, E, M) :-
    rdf_global_term(A, E, M),
    !.
rdf_expand_arg(g, A, E, M) :-
    rdf_global_graph(A, E, M),
    !.
rdf_expand_arg(:, A, E, _M) :-
    !,
    expand_goal(A, E).
rdf_expand_arg(_, A, A, _M).

%!  mk_global(+Src, -Resource, +Module)
%
%   Realised rdf_global_id(+, -), but adds compiletime checking,
%   notably to see whether a namespace is not yet defined.

mk_global(X, X, _) :-
    var(X),
    !.
mk_global(X, X, _) :-
    atom(X),
    !.
mk_global(Prefix:Local, Global, Module) :-
    must_be(atom, Prefix),
    must_be(atom, Local),
    (   rdf_current_prefix(Module:Prefix, Full)
    ->  atom_concat(Full, Local, Global)
    ;   current_prolog_flag(xref, true)
    ->  Global = Prefix:Local
    ;   existence_error(rdf_prefix, Prefix)
    ).

