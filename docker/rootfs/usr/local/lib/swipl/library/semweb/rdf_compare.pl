/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2020, University of Amsterdam
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

:- module(rdf_compare,
          [ rdf_equal_graphs/3          % +Graph1, +Graph2, -Substitutions
          ]).
:- use_module(library(semweb/rdf_db),[lang_equal/2,rdf_is_bnode/1]).
:- autoload(library(apply),[partition/4,maplist/3]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(lists),[select/3]).

/** <module> Compare RDF graphs

This library provides predicates that compare   RDF  graphs. The current
version only provides one predicate:   rdf_equal_graphs/3  verifies that
two graphs are identical after proper labeling of the blank nodes.

Future versions of this library may   contain  more advanced operations,
such as diffing two graphs.
*/


%!  rdf_equal_graphs(+GraphA, +GraphB, -Substition) is semidet.
%
%   True if GraphA  and  GraphB  are   the  same  under  Substition.
%   Substition is a list of BNodeA = BNodeB, where BNodeA is a blank
%   node that appears in GraphA and  BNodeB   is  a  blank node that
%   appears in GraphB.
%
%   @param GraphA is a list of rdf(S,P,O) terms
%   @param GraphB is a list of rdf(S,P,O) terms
%   @param Substition is a list if NodeA = NodeB terms.
%   @tbd    The current implementation is rather naive.  After
%           dealing with the subgraphs that contain no bnodes,
%           it performs a fully non-deterministic substitution.

rdf_equal_graphs(A, B, Substitutions) :-
    sort(A, SA),
    sort(B, SB),
    partition(contains_bnodes, SA, VA, GA),
    partition(contains_bnodes, SB, VB, GB),
    (   GA == GB
    ->  true
    ;   maplist(compare_triple, GA, GB)
    ),
    compare_list(VA, VB, [], Substitutions),
    !.

contains_bnodes(rdf(S,P,O)) :-
    (   node_id(S)
    ;   node_id(P)
    ;   node_id(O)
    ),
    !.

compare_list([], [], S, S).
compare_list([H1|T1], In2, S0, S) :-
    select(H2, In2, T2),
    compare_triple(H1, H2, S0, S1),
    compare_list(T1, T2, S1, S).

compare_triple(T1, T2) :-
    compare_triple(T1,T2,[],[]).

compare_triple(rdf(Subj1,P1,O1), rdf(Subj2, P2, O2), S0, S) :-
    compare_field(Subj1, Subj2, S0, S1),
    compare_field(P1, P2, S1, S2),
    compare_field(O1, O2, S2, S).

compare_field(X, X, S, S) :- !.
compare_field(literal(X), xml(X), S, S) :- !. % TBD
compare_field(literal(lang(L1,X)), literal(lang(L2,X)), S, S) :-
    !,
    lang_equal(L1, L2).
compare_field(X, Id, S, S) :-
    memberchk(X=Id, S),
    !.
compare_field(X, Y, S, [X=Y|S]) :-
    \+ memberchk(X=_, S),
    node_id(X),
    node_id(Y),
    debug(rdf_compare, 'Assume ~w = ~w~n', [X, Y]).

node_id(node(_)) :- !.
node_id(X) :-
    rdf_is_bnode(X).

