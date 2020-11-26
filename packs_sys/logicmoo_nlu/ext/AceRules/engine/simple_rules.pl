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


:- module(simple_rules, [
		simple_rules/2  % +Rules, -SimpleRules
	]).

:- use_module(op_defs).
:- use_module(list_utils).
:- use_module(logger).

/** <module> Simple rules

This module generates a simple and concise rule representation on the basis of
the internal rule representation of AceRules.

The simple rules are not completely equivalent to the internal representation:

* "Something", "everybody", etc. are represented in the same way as nouns. That
  means that they do not unify with other objects (as they do in the internal
  format).
* maybe more...

@author Tobias Kuhn
@version 2008-11-24
*/


%% simple_rules(+Rules, -SimpleRules)
%
% Transforms the rules in internal format into the simple rule format.

simple_rules(Rules, SimpleRules) :-
    retractall(var_data(_,_)),
    copy_term(Rules, RulesCopy),  % just to be sure...
    simple_rules_x(RulesCopy, RulesTemp),
    transform_data_values(RulesTemp, SimpleRules).


simple_rules_x([], []) :-
    !.

simple_rules_x([(_Label, Head, [])|RestIn], RestOut) :-
    transform_list([Head], []),
    !,
    simple_rules_x(RestIn, RestOut).

simple_rules_x([(Label, Head, Body)|RestIn], [(Label, HeadT, BodyT)|RestOut]) :-
    transform_list([Head], [HeadT]),
    transform_list(Body, BodyT),
    !,
    simple_rules_x(RestIn, RestOut).

simple_rules_x([Rule|RestIn], [Rule|RestOut]) :-
    simple_rules_x(RestIn, RestOut).


%% transform_list(+ListIn, -ListOut)
%
% Transforms a list of conditions into the simple rule format.

transform_list([], []).

transform_list([-Pred|RestIn], [-PredT|RestOut]) :-
    !,
    transform_list([Pred], [PredT]),
    transform_list(RestIn, RestOut).

transform_list([~Pred|RestIn], [~PredT|RestOut]) :-
    !,
    transform_list([Pred], [PredT]),
    transform_list(RestIn, RestOut).

transform_list([can([Pred])|RestIn], [can(PredT)|RestOut]) :-
    !,
    transform_list([Pred], [PredT]),
    transform_list(RestIn, RestOut).

transform_list([can(ListIn)|RestIn], [can(ListOut)|RestOut]) :-
    !,
    transform_list(ListIn, ListOut),
    transform_list(RestIn, RestOut).

transform_list([must([Pred])|RestIn], [must(PredT)|RestOut]) :-
    !,
    transform_list([Pred], [PredT]),
    transform_list(RestIn, RestOut).

transform_list([must(ListIn)|RestIn], [must(ListOut)|RestOut]) :-
    !,
    transform_list(ListIn, ListOut),
    transform_list(RestIn, RestOut).

transform_list([group([Pred])|RestIn], [PredT|RestOut]) :-
    !,
    transform_list([Pred], [PredT]),
    transform_list(RestIn, RestOut).

transform_list([group(Group)|RestIn], [group(GroupT)|RestOut]) :-
    !,
    transform_list(Group, GroupT),
    transform_list(RestIn, RestOut).

transform_list([object(R, Noun, _, _, _, _)|RestIn], [PredOut|RestOut]) :-
    is_var_or_skolem(Noun),
    !,
    transform_ref(R, RT),
    PredOut =.. [something, RT],
    transform_list(RestIn, RestOut).

transform_list([object(_, na, _, _, _, _)|RestIn], RestOut) :-  % group objects
    !,
    transform_list(RestIn, RestOut).

transform_list([object(_, _, named, _, _, _)|RestIn], RestOut) :-
    !,
    transform_list(RestIn, RestOut).

transform_list([object(R, Value, data, _, _, _)|RestIn], RestOut) :-
    var(R),
    !,
    R = Value,
    transform_list(RestIn, RestOut).

transform_list([object(R, Value, data, _, _, _)|RestIn], RestOut) :-
    !,
    transform_ref(R, RT),
    assert_var_data(RT, Value),
    transform_list(RestIn, RestOut).

transform_list([object(R, Noun, _, _, _, _)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(R, RT),
    PredOut =.. [Noun, RT],
    transform_list(RestIn, RestOut).

transform_list([property(R, Adj, _D)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(R, RT),
    PredOut =.. [Adj, RT],
    transform_list(RestIn, RestOut).

transform_list([property(A, Adj, _D, B)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_ref(B, BT),
    PredOut =.. [Adj, AT, BT],
    transform_list(RestIn, RestOut).

transform_list([property(A, Adj, B, _D, _T, C)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_ref(B, BT),
    transform_ref(C, CT),
    PredOut =.. [Adj, AT, BT, CT],
    transform_list(RestIn, RestOut).

transform_list([of_relation(object(A,Noun,_,_,_,_),B)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_ref(B, BT),
    PredOut = of(Noun, AT, BT),
    transform_list(RestIn, RestOut).

transform_list([pred_mod(Verb, A, Mods)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_mods(Mods, ModsT),
    PredOut =.. [Verb, AT, ModsT],
    transform_list(RestIn, RestOut).

transform_list([pred_mod(Verb, A, B, Mods)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_ref(B, BT),
    transform_mods(Mods, ModsT),
    PredOut =.. [Verb, AT, BT, ModsT],
    transform_list(RestIn, RestOut).

transform_list([pred_mod(Verb, A, B, C, Mods)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_ref(B, BT),
    transform_ref(C, CT),
    transform_mods(Mods, ModsT),
    PredOut =.. [Verb, AT, BT, CT, ModsT],
    transform_list(RestIn, RestOut).

transform_list([be_adj(A, Adj, _, Mods)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(A, AT),
    transform_mods(Mods, ModsT),
    PredOut =.. [Adj, AT, ModsT],
    transform_list(RestIn, RestOut).

transform_list([has_part(G, P)|RestIn], [PredOut|RestOut]) :-
    !,
    transform_ref(G, GT),
    transform_ref(P, PT),
    PredOut = has_part(GT, PT),
    transform_list(RestIn, RestOut).

transform_list([Pred|RestIn], [Pred|RestOut]) :-
    transform_list(RestIn, RestOut).


%% transform_mods(+Modifiers, -Desc, -Refs)
%
% Transforms a list of modifiers into a description and the list
% of referents (for the PPs). The description is a list of atoms
% that contains the names of the modifiers separated by underscores.

transform_mods(ModsIn, ModsOut) :-
    transform_mods_x(ModsIn, ModsTemp),
    reverse(ModsTemp, ModsOut).

transform_mods_x([], []).

transform_mods_x([modifier_adv(Adv, _D)|Mods], [Adv|RestOut]) :-
    !,
    transform_mods_x(Mods, RestOut).

transform_mods_x([modifier_pp(Prep, R)|Mods], [PrepTerm|RestOut]) :-
    transform_ref(R, RT),
    PrepTerm =.. [Prep, RT],
    transform_mods_x(Mods, RestOut).


%% transform_ref(+RefIn, -RefOut)
%
% Transforms a (possibly skolemized) discourse referent into a
% shorter form. Concretely, these transformations are done:
%
% ==
% X         => X
% v('John') => 'John'
% v(3)      => v(3)
% gv(5)     => gv(5)
% ==

transform_ref(V, VT) :-
    var(V),
    !,
    VT = V.

transform_ref(v(A), A) :-
    atom(A),
    !.

transform_ref(R, R).


%% is_var_or_skolem(+Term)
%
% Succeeds if the term is a variable or a skolemized variable.

is_var_or_skolem(V) :-
    var(V),
    !.

is_var_or_skolem(v(_)).

is_var_or_skolem(gv(_)).



:- dynamic(var_data/2).

assert_var_data(Var, _) :-
    var_data(Var, _),
    !,
    log('simple-rules.UnificationError'),
    throw(error('simple-rules.UnificationError', 'Unification failed for simple rules generation.')).

assert_var_data(Var, Data) :-
    assert(var_data(Var, Data)).


%% transform_data_values(+TermIn, -TermOut)

transform_data_values(Var, Var) :-
    var(Var),
    !.

transform_data_values(TermIn, Data) :-
    var_data(TermIn, Data),
    !.

transform_data_values([], []) :-
	!.

transform_data_values([H1|T1], [H2|T2]) :-
	!,
	transform_data_values(H1, H2),
	transform_data_values(T1, T2).

transform_data_values(Term, Term) :-
	Term =.. [Term],
	!.

transform_data_values(Term1, Term2) :-
	!,
	Term1 =.. List1,
	transform_data_values(List1, List2),
	Term2 =.. List2.
