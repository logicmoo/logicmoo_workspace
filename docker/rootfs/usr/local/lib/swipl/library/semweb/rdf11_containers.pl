/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Wouter Beek
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(rdf11_containers,
          [ rdf_alt/3,                            % ?Alt, -Default, -Others
            rdf_assert_alt/3,                     % ?Alt, +Default, +Others
            rdf_assert_alt/4,                     % ?Alt, +Default, +Others, +G
            rdf_assert_bag/2,                     % ?Bag, +Set
            rdf_assert_bag/3,                     % ?Bag, +Set, +G
            rdf_assert_seq/2,                     % ?Seq, +List
            rdf_assert_seq/3,                     % ?Seq, +List, +G
            rdf_bag/2,                            % ?Bag, -List
            rdf_seq/2,                            % ?Seq, -List
            rdfs_container/2,                     % ?Container, -List
            rdfs_container_membership_property/1, % ?Property
            rdfs_container_membership_property/2, % ?Property, ?Number
            rdfs_member/2,                        % ?Elem, ?Container
            rdfs_nth0/3                           % ?N, ?Container, ?Elem
          ]).
:- use_module(library(semweb/rdf_prefixes),
              [ (rdf_meta)/1, op(_,_,rdf_meta)
              ]).
:- use_module(rdf11,
              [ rdf_default_graph/1, rdf_transaction/1, rdf_create_bnode/1,
                rdf_assert/4, rdf_equal/2, rdf_is_subject/1, rdf_has/3
              ]).

:- autoload(library(apply),[maplist/3]).
:- autoload(library(error),[must_be/2,type_error/2]).
:- autoload(library(lists),[member/2]).
:- autoload(library(pairs),[group_pairs_by_key/2,pairs_values/2]).

/** <module> RDF 1.1 Containers

Implementation of the conventional human interpretation of RDF 1.1 containers.

RDF containers are open enumeration structures as opposed to RDF collections or
RDF lists which are closed enumeration structures.
The same resource may appear in a container more than once.
A container may be contained in itself.

---

@author Wouter Beek
@author Jan Wielemaker
@compat RDF 1.1
@see http://www.w3.org/TR/2014/REC-rdf-schema-20140225/#ch_containervocab
@version 2016/01
*/

:- rdf_meta
    rdf_alt(r, -, -),
    rdf_assert_alt(r, r, t),
    rdf_assert_alt(r, r, t, r),
    rdf_assert_bag(r, t),
    rdf_assert_bag(r, t, r),
    rdf_assert_seq(r, t),
    rdf_assert_seq(r, t, r),
    rdf_bag(r, -),
    rdf_seq(r, -),
    rdfs_assert_container(r, t),
    rdfs_assert_container(r, t, r),
    rdfs_assert_container(r, r, t, r),
    rdfs_container(r, -),
    rdfs_container_membership_property(r),
    rdfs_container_membership_property(r,?),
    rdfs_member(r, -).


%!  rdf_alt(+Alt, ?Default, ?Others) is nondet.
%
%   True when Alt is an instance of `rdf:Alt` with first member
%   Default and remaining members Others.
%
%   Notice that this construct adds no machine-processable semantics
%   but is conventionally used to indicate   to  a human reader that
%   the numerical ordering of the container membership properties of
%   Container is intended to  only   be  relevant  in distinguishing
%   between the first and all non-first members.
%
%   Default denotes the default option to  take when choosing one of
%   the alternatives container in  Container.   Others  denotes  the
%   non-default options that can be chosen from.

rdf_alt(Alt, Default, Others) :-
    rdfs_container(Alt, [Default|Others]).

%!  rdf_assert_alt(?Alt, +Default, +Others:list) is det.
%!  rdf_assert_alt(?Alt, +Default, +Others:list, +Graph) is det.
%
%   Create an rdf:Alt with the given Default and Others. Default and
%   the  members  of  Others  must  be    valid   object  terms  for
%   rdf_assert/3.

rdf_assert_alt(Alt, H, T) :-
    rdf_default_graph(G),
    rdf_assert_alt(Alt, H, T, G).

rdf_assert_alt(Alt, H, T, G) :-
    rdfs_assert_container(Alt, rdf:'Alt', [H|T], G).


%!  rdf_bag(+Bag, -List:list) is nondet.
%
%   True when Bag is an rdf:Bag and   set  is the set values related
%   through container membership properties to Bag.
%
%   Notice that this construct adds no machine-processable semantics
%   but is conventionally used to indicate   to  a human reader that
%   the numerical ordering of the container membership properties of
%   Container is intended to not be significant.

rdf_bag(Bag, List) :-
    rdfs_container(Bag, List).


%!  rdf_assert_bag(?Bag, +Set:list) is det.
%!  rdf_assert_bag(?Bag, +Set:list, +Graph) is det.
%
%   Create an rdf:Bag from the given set   of values. The members of
%   Set must be valid object terms for rdf_assert/3.

rdf_assert_bag(Bag, L) :-
    rdf_default_graph(G),
    rdf_assert_bag(Bag, L, G).

rdf_assert_bag(Bag, L, G) :-
    rdfs_assert_container(Bag, rdf:'Bag', L, G).


%!  rdf_seq(+Seq, -List:list) is nondet.
%
%   True when Seq is an instance of rdf:Seq   and  List is a list of
%   associated values, ordered according to the container membership
%   property used.
%
%   Notice that this construct adds no machine-processable semantics
%   but is conventionally used to indicate   to  a human reader that
%   the numerical ordering of the container membership properties of
%   Container is intended to be significant.

rdf_seq(Seq, L) :-
    rdfs_container(Seq, L).


%!  rdf_assert_seq(?Seq, +List) is det.
%!  rdf_assert_seq(?Seq, +List, +Graph) is det.

rdf_assert_seq(Seq, L) :-
    rdf_default_graph(G),
    rdf_assert_seq(Seq, L, G).
rdf_assert_seq(Seq, L, G) :-
    rdfs_assert_container(Seq, rdf:'Seq', L, G).


%!  rdfs_assert_container(?Container, +Class, +Elems, +Graph)

rdfs_assert_container(Container, Class, Elems, G) :-
    must_be(list, Elems),
    rdf_transaction(rdfs_assert_container_(Container, Class, Elems, G)).

rdfs_assert_container_(Container, Class, Elems, G) :-
    (var(Container) -> rdf_create_bnode(Container) ; true),
    rdf_assert(Container, rdf:type, Class, G),
    rdfs_assert_members(Elems, 1, Container, G).

rdfs_assert_members([], _, _, _).
rdfs_assert_members([H|T], N1, Resource, G) :-
    !,
    rdf_equal(rdf:'_', Prefix),
    atom_concat(Prefix, N1, P),
    rdf_assert(Resource, P, H, G),
    N2 is N1 + 1,
    rdfs_assert_members(T, N2, Resource, G).


%!  rdfs_container(+Container, -List) is nondet.
%
%   True when List is the  list   of  objects  attached to Container
%   using a container membership property  (rdf:_0, rdf:_1, ...). If
%   multiple objects are connected to the   Container using the same
%   membership  property,  this   predicate    selects   one   value
%   non-deterministically.

rdfs_container(Container, List) :-
    rdf_is_subject(Container),
    !,
    findall(N-Elem, rdfs_member0(Container, N, Elem), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, GroupedPairs),
    pairs_values(GroupedPairs, Groups),
    maplist(member, List, Groups).
rdfs_container(Container, _) :-
    type_error(rdf_subject, Container).


%!  rdfs_container_membership_property(?Property) is nondet.
%
%   True when Property is a   container membership property (rdf:_1,
%   rdf:_2, ...).

rdfs_container_membership_property(P) :-
    rdfs_container_membership_property(P, _).


%!  rdfs_container_membership_property(?Property, ?Number:nonneg) is nondet.
%
%   True when Property is the Nth container membership property.
%
%   Success of this goal does not imply that Property is present
%   in the database.

rdfs_container_membership_property(P, N) :-
    var(P),
    !,
    between(1, inf, N),
    rdf_equal(rdf:'_', Prefix),
    atom_concat(Prefix, N, P).
rdfs_container_membership_property(P, N) :-
    atom(P),
    rdf_equal(rdf:'_', Prefix),
    string_concat(Prefix, NumS, P),
    number_string(N, NumS),
    integer(N),
    N >= 0.


%!  rdfs_member(?Elem, ?Container) is nondet.
%
%   True if rdf(Container, P, Elem) is true   and P is a container
%   membership property.

rdfs_member(Elem, Container) :-
    rdfs_member0(Container, _, Elem).

%!  rdfs_nth0(?N, ?Container, ?Elem) is nondet.
%
%   True if rdf(Container, P, Elem)  is  true   and  P  is the N-th
%   (0-based) container membership property.

rdfs_nth0(N, Container, Elem) :-
    rdfs_member0(Container, N, Elem).


%!  rdfs_member0(?Container, ?N, ?Elem) is nondet.
%
%   What is the most efficient way to enumerate rdfs_member(-,-)?
%
%   1. If we enumerate over all   container membership properties (= the
%   current implementation) then it  takes  N   steps  before  we get to
%   triple `<Container, rdf:_N, Elem>`, for arbitrary N.
%
%   2. The alternative is  to  enumerate   over  all  triples  and check
%   whether the predicate term is a container membership property.
%
%   3. The choice between (1) and (2)   depends on whether the number of
%   currently loaded triples in larger/smaller   than the largest number
%   that  appears  in  a  container   membership  property.  This  means
%   enumerating over all predicate terms using rdf_predicate/1.

rdfs_member0(Container, N, Elem) :-
    (nonvar(Container) ; nonvar(Elem)),
    !,
    rdf_has(Container, P, Elem),
    rdfs_container_membership_property(P, N).
rdfs_member0(Container, N, Elem) :-
    rdfs_container_membership_property(P, N),
    rdf_has(Container, P, Elem).
