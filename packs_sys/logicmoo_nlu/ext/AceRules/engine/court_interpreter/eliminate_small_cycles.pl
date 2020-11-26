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


:- module(eliminate_small_cycles, [
		eliminate_small_cycles/2  % +RulesIn, -RulesOut
	]).

:- use_module('../op_defs').
:- use_module('../utils').
:- use_module('../list_utils').

/** <module> Small cycle eliminator

*|
This module is currently inactivated.
|*

This module eliminates cycles with 2 or less nodes. These cycles are removed
in a way that leads to meaningful results. With this cycle elimination, some cyclic
programs can be executed in courteous mode (which normally does not allow cycles).
Cycles that share some nodes and cycles that consist of 3 or more nodes cannot
be removed. Due to the problems listed below, this module is currectly
inactivated (i.e. not used).


---+++ Problems

The main problem is that only a small set of possible cylces can be removed and it
is hard to tell the user which cycles are allowed and which are not.

The semantic consequences of this cycle elimination are not fully understood.

Since auxiliary rules are introduced, the cycle elimination makes the trace
output less understandable.

@author Tobias Kuhn
@version 2007-02-08
*/


%% eliminate_small_cycles(+RulesIn, -RulesOut)
%
% Eliminates the small cycles of the rule set. Auxiliary rules are introduced.

eliminate_small_cycles(RulesIn, RulesOut) :-
    generate_graph(RulesIn, Graph),
    find_small_cycles(Graph, CycleGraph),
    check_cycles(CycleGraph),
    get_cycle_rules(CycleGraph, CycleRulesPre),
    get_setof(R, member(R,CycleRulesPre), CycleRules),
    get_cycle_nodes(CycleGraph, CycleNodesPre),
    get_setof(N, member(N,CycleNodesPre), CycleNodes),
    transform_cycle_rules(CycleRules, CycleNodes, CycleRulesT),
    remove_all_exact(RulesIn, CycleRules, NoncycleRules),
    transform_noncycle_rules(NoncycleRules, CycleNodes, NoncycleRulesT),
    generate_aux_rules(CycleNodes, AuxRules),
    append(NoncycleRulesT, CycleRulesT, RulesTemp),
    append(RulesTemp, AuxRules, RulesOut).


generate_graph([], []).

generate_graph([Fact|RulesIn], Graph) :-
    Fact = (_Label, _FactHead, []),
    !,
    generate_graph(RulesIn, Graph).

generate_graph([Rule|RulesIn], Graph) :-
    Rule = ('', Head, Body),
    !,
    bind_vars(Body, BodyG),
    bind_vars(Head, HeadG),
    store_edges(HeadG, BodyG, Rule, Edges),
    generate_graph(RulesIn, GraphRest),
    append(Edges, GraphRest, Graph).

generate_graph([_LabeledRule|RulesIn], Graph) :-
    !,
    generate_graph(RulesIn, Graph).


find_small_cycles(Graph, CycleGraph) :-
    get_setof(
        edge(From, To, Rule),
        Rule2^(member(edge(From, To, Rule), Graph), member(edge(To, From, Rule2), Graph)),
        CycleGraph).


check_cycles(CycleGraph) :-
    check_cycles_aux(CycleGraph, []).

check_cycles_aux([], _).

check_cycles_aux([edge(Node, Node, _)|_], CollectedNodes) :-
    member(Node, CollectedNodes),
    !,
    throw(ar_error('court-interpreter.eliminate-small-cycles.CyclicADG', 'The program contains cycles.')).

check_cycles_aux([edge(Node, Node, _)|GraphRest], CollectedNodes) :-
    !,
    check_cycles_aux(GraphRest, [Node|CollectedNodes]).

check_cycles_aux([edge(From, To, _)|_], CollectedNodes) :-
    ( member(From, CollectedNodes) ; member(To, CollectedNodes) ),
    !,
    throw(ar_error('court-interpreter.eliminate-small-cycles.CyclicADG', 'The program contains cycles.')).

check_cycles_aux([edge(From, To, _)|GraphRest], CollectedNodes) :-
    !,
    remove(GraphRest, edge(To, From, _), GraphRest2),
    check_cycles_aux(GraphRest2, [From,To|CollectedNodes]).


get_cycle_rules([], []).

get_cycle_rules([edge(_From, _To, Rule)|GraphRest], [Rule|RulesRest]) :-
    get_cycle_rules(GraphRest, RulesRest).


get_cycle_nodes([], []).

get_cycle_nodes([edge(From, To, _Rule)|GraphRest], [From, To|NodesRest]) :-
    get_cycle_nodes(GraphRest, NodesRest).


transform_cycle_rules([], _, []).

transform_cycle_rules([Rule|RulesIn], CycleNodes, [RuleT|RulesOut]) :-
    Rule = ('', Head, Body),
    transform_body(Body, CycleNodes, BodyT),
    RuleT = ('', Head, BodyT),
    transform_cycle_rules(RulesIn, CycleNodes, RulesOut).


transform_body([], _, []).

transform_body([ - Element|BodyIn], CycleNodes, [ - aux(Element)|BodyOut]) :-
    has_matching_member(Element, CycleNodes),
    !,
    transform_body(BodyIn, CycleNodes, BodyOut).

transform_body([ ~ (- Element)|BodyIn], CycleNodes, [ ~ (- aux(Element))|BodyOut]) :-
    has_matching_member(Element, CycleNodes),
    !,
    transform_body(BodyIn, CycleNodes, BodyOut).

transform_body([ ~ Element|BodyIn], CycleNodes, [ ~ aux(Element)|BodyOut]) :-
    has_matching_member(Element, CycleNodes),
    !,
    transform_body(BodyIn, CycleNodes, BodyOut).

transform_body([Element|BodyIn], CycleNodes, [aux(Element)|BodyOut]) :-
    has_matching_member(Element, CycleNodes),
    !,
    transform_body(BodyIn, CycleNodes, BodyOut).

transform_body([Element|BodyIn], CycleNodes, [Element|BodyOut]) :-
    !,
    transform_body(BodyIn, CycleNodes, BodyOut).



transform_noncycle_rules([], _, []).

transform_noncycle_rules([Rule|RulesIn], CycleNodes, [RuleT|RulesOut]) :-
    Rule = (Label, - Head, Body),
    has_matching_member(Head, CycleNodes),
    !,
    RuleT = (Label, - aux(Head), Body),
    transform_noncycle_rules(RulesIn, CycleNodes, RulesOut).

transform_noncycle_rules([Rule|RulesIn], CycleNodes, [RuleT|RulesOut]) :-
    Rule = (Label, Head, Body),
    has_matching_member(Head, CycleNodes),
    !,
    RuleT = (Label, aux(Head), Body),
    transform_noncycle_rules(RulesIn, CycleNodes, RulesOut).

transform_noncycle_rules([Rule|RulesIn], CycleNodes, [Rule|RulesOut]) :-
    transform_noncycle_rules(RulesIn, CycleNodes, RulesOut).


generate_aux_rules([], []).

generate_aux_rules([N|CycleNodes], [Rule1, Rule2|AuxRules]) :-
    unbind_vars(N, U),
    Rule1 = (aux, U, [aux(U)]),
    Rule2 = (aux, - U, [- aux(U)]),
    generate_aux_rules(CycleNodes, AuxRules).


store_edges(_Head, [], _Rule, []).

store_edges(Head, [BodyElement|BodyRest], Rule, [Edge|GraphRest]) :-
    get_positive(Head, HeadP),
    get_positive(BodyElement, BodyElementP),
    Edge = edge(BodyElementP, HeadP, Rule),
    store_edges(Head, BodyRest, Rule, GraphRest).


bind_vars(In, '$') :-
    var(In),
    !.

bind_vars(v(_), '$') :-
    !.

bind_vars([], []) :-
	!.

bind_vars([InH|InT], [OutH|OutT]) :-
	!,
	bind_vars(InH, OutH),
	bind_vars(InT, OutT).

bind_vars(Term, Term) :-
	Term =.. [Term],
	!.

bind_vars(Term1, Term2) :-
	!,
	Term1 =.. List1,
	bind_vars(List1, List2),
	Term2 =.. List2.


unbind_vars('$', _) :-
    !.

unbind_vars([], []) :-
	!.

unbind_vars([InH|InT], [OutH|OutT]) :-
	!,
	unbind_vars(InH, OutH),
	unbind_vars(InT, OutT).

unbind_vars(Term, Term) :-
	Term =.. [Term],
	!.

unbind_vars(Term1, Term2) :-
	!,
	Term1 =.. List1,
	unbind_vars(List1, List2),
	Term2 =.. List2.


has_matching_member(Element, List) :-
    bind_vars(Element, ElementG),
    member(ElementG, List).


get_positive(- Term, Term) :-
    !.

get_positive(~ (- Term), Term) :-
    !.

get_positive(~ Term, Term) :-
    !.

get_positive(Term, Term).
