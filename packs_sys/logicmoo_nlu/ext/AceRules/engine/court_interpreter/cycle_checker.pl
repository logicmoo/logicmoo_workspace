% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(cycle_checker, [
		     store_edges/1,          % +InstantiatedRule
		     reset_cycle_checker/0,
		     adg_is_acyclic/0
		    ]).

:- use_module('../op_defs').
:- use_module('../list_utils').

/** <module> Cycle checker for courteous interpreter

This module observes the execution of a courteous program. At the end, this module
can determine whether a cycle occurred or not. (The courteous interpreter determines
even in the case of cycles.)

@author Tobias Kuhn
@version 2007-02-06
*/


:- dynamic(edge/2).


%% reset_cycle_checker
%
% Clears the stored edges.

reset_cycle_checker :-
    retractall(edge(_, _)).


%% store_edges(+InstantiatedRule)
%
% Stores the edges for the variable-free rule. From each condition of the body, there
% is an edge to the head of the rule.

store_edges([]).

store_edges([(_,Head,Body)|Rest]) :-
    store_edges_for_rule(Head, Body),
    store_edges(Rest).


store_edges_for_rule(_Head, []).

store_edges_for_rule(Head, [BodyElement|BodyRest]) :-
    ground(Head),
    ground(BodyElement),
    !,
    remove_negation(Head, HeadN),
    remove_negation(BodyElement, BodyElementN),
    assert_edge(BodyElementN, HeadN),
    store_edges_for_rule(Head, BodyRest).


remove_negation(- Term, Term) :-
    !.

remove_negation(Term, Term).


assert_edge(From, To) :-
    edge(From, To),
    !.

assert_edge(From, To) :-
    assert(edge(From, To)).


%% adg_is_acyclic
%
% Succeeds if the stored edges do not contain a cycle. It throws an error otherwise.

adg_is_acyclic :-
    findall(edge(A,B), edge(A,B), Edges),
    findall(E, (edge(E, _) ; edge(_, E)), Vertices),
    remove_dublicates(Vertices, VerticesND),
    adg_is_acyclic(VerticesND, Edges).

adg_is_acyclic([], []) :-
    !.

adg_is_acyclic(Vertices, Edges) :-
    member(Vertex, Vertices),
    findall(A, member(edge(A, Vertex), Edges), []),
    !,
    remove(Vertices, Vertex, VerticesNew),
    findall(edge(Vertex, V), member(edge(Vertex, V), Edges), EdgesToRemove),
    remove_all(Edges, EdgesToRemove, EdgesNew),
    adg_is_acyclic(VerticesNew, EdgesNew).

% error
adg_is_acyclic(_, _) :-
    throw(ar_error('court-interpreter.cycle-checker.CyclicADG', 'The program contains cycles.')).
