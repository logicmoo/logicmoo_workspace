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


:- module(drs_checker, [
			check_drs/2  % +DRS, +Level
		       ]).

:- use_module('../op_defs').

/** <module> DRS checker

This program checks the structure of a DRS, in order to process them correctly
with AceRules. There are three increasingly restrictive levels: 1, 2, and 3.

---+++ Level 1
* The DRS contains no disjunctions.
* Negated sub-DRSes contain no nested implications or negations.
* The if-parts of implications contain no nested implications.
* The then-parts of implications contain no nested implications or negations.
* Modality boxes have a flat content.

---+++ Additionally for level 2
* Negated sub-DRSes contain an atom.
* The then-parts of implications contain an atom.
* No modality boxes.

---+++ Additionally for level 3
* Unbound variables occur only in implications.
* Every variable that occurs in the then-part of an implication has to occur
  in the if-part of the implication.
* No modality boxes.

@author Tobias Kuhn
@version 2007-03-19
*/


%% check_drs(+DRS, +Level)
%
% Checks if the DRS is valid on the specified level, otherwise the predicate
% fails.

check_drs(drs(_, Conditions), 1) :-
	valid_conditions_1(Conditions).

check_drs(drs(_, Conditions), 2) :-
	valid_conditions_2(Conditions).

check_drs(drs(_, Conditions), 3) :-
	valid_conditions_3(Conditions).


%% valid_conditions_1(+Conditions)
%
% Succeeds if the conditions are valid top-level conditions for level 1.

valid_conditions_1([]).

valid_conditions_1([Fact|Rest]) :-
    valid_fact_mod(Fact),
    !,
	valid_conditions_1(Rest).

valid_conditions_1([-drs(_, NegConditions)|Rest]) :-
	flat_mod(NegConditions),
	!,
	valid_conditions_1(Rest).

% error
valid_conditions_1([-drs(_, _)|_]) :-
    throw(ar_error('parser.drs-check-1.NotFlatInsideOfNeg', 'Negation can not be applied on complex structures (such as other negations or if-then-structures).')).

% error
valid_conditions_1([~drs(_, _)|_]) :-
    throw(ar_error('parser.drs-check-1.NafInToplevel', 'Negation as failure can only be used in the "if"-part of rules.')).

% A -> (B -> -C) will be transformed into AB -> -C
valid_conditions_1([drs(_, IfConditions1) => drs([], [drs(_, IfConditions2) => drs([], [-drs(_, NegThenConditions)])])|Rest]) :-
	wide_literals(IfConditions1),
	wide_literals(IfConditions2),
	flat_mod(NegThenConditions),
	!,
	valid_conditions_1(Rest).

valid_conditions_1([drs(_, IfConditions) => drs([], [-drs(_, NegThenConditions)])|Rest]) :-
	wide_literals(IfConditions),
	flat_mod(NegThenConditions),
	!,
	valid_conditions_1(Rest).

% error
valid_conditions_1([drs(_, IfConditions) => drs([], [-drs(_, _)])|_]) :-
	wide_literals(IfConditions),
	!,
	throw(ar_error('parser.drs-check-1.NotFlatInsideOfNeg', 'Negation can not be applied on complex structures (such as other negations or if-then-structures).')).

% error
valid_conditions_1([drs(_, _) => drs([], [-drs(_, _)])|_]) :-
	!,
	throw(ar_error('parser.drs-check-1.NotLiteralsInIf', 'The "if"-part of a rule is not allowed to contain complex structures other than unnested negations.')).

% A -> (B -> C) will be transformed into AB -> C
valid_conditions_1([drs(_, IfConditions1) => drs([], [drs(_, IfConditions2) => drs(_, ThenConditions)])|Rest]) :-
	wide_literals(IfConditions1),
	wide_literals(IfConditions2),
	flat_mod(ThenConditions),
	!,
	valid_conditions_1(Rest).

valid_conditions_1([drs(_, IfConditions) => drs(_, ThenConditions)|Rest]) :-
	wide_literals(IfConditions),
	flat_mod(ThenConditions),
	!,
	valid_conditions_1(Rest).

% error
valid_conditions_1([drs(_, IfConditions) => drs(_, _)|_]) :-
	wide_literals(IfConditions),
	!,
	throw(ar_error('parser.drs-check-1.NotFlatInThen', 'The "then"-part of a rule is not allowed to contain complex structures (such as negations or if-then-structures).')).

% error
valid_conditions_1([drs(_, _) => drs(_, _)|_]) :-
	!,
	throw(ar_error('parser.drs-check-1.NotLiteralsInIf', 'The "if"-part of a rule is not allowed to contain complex structures other than unnested negations.')).

% error
valid_conditions_1([_:drs(_, _)|_]) :-
	throw(ar_error('parser.drs-check-1.ThatSubordNotSupported', 'Sentence subordination with "that" is not allowed.')).


%% valid_conditions_2(+Conditions)
%
% Succeeds if the conditions are valid top-level conditions for level 2.

valid_conditions_2([]).

valid_conditions_2([Fact|Rest]) :-
    valid_fact(Fact),
	valid_conditions_2(Rest).

valid_conditions_2([-drs(_, [Fact])|Rest]) :-
    valid_fact(Fact),
	valid_conditions_2(Rest).

valid_conditions_2([~drs(_, [Fact])|Rest]) :-
    valid_fact(Fact),
	valid_conditions_2(Rest).

valid_conditions_2([drs(_, IfConditions) => drs([], [-drs(_, [Fact])])|Rest]) :-
	!,
	valid_fact(Fact),
	strict_literals(IfConditions),
	valid_conditions_2(Rest).

valid_conditions_2([drs(_, IfConditions) => drs(_, [Fact])|Rest]) :-
    valid_fact(Fact),
	strict_literals(IfConditions),
	valid_conditions_2(Rest).


%% valid_conditions_3(+Conditions)
%
% Succeeds if the conditions are valid top-level conditions for level 3.

valid_conditions_3([]).

valid_conditions_3([Fact|Rest]) :-
    valid_fact(Fact),
	valid_conditions_3(Rest).

valid_conditions_3([-drs(_, [Fact])|Rest]) :-
    valid_fact(Fact),
	contains_no_vars(Fact),
	valid_conditions_3(Rest).

valid_conditions_3([~drs(_, [Fact])|Rest]) :-
    valid_fact(Fact),
	contains_no_vars(Fact),
	valid_conditions_3(Rest).

valid_conditions_3([drs(_, IfConditions) => drs([], [-drs(_, [Then])])|Rest]) :-
	!,
	valid_fact(Then),
	strict_literals(IfConditions),
	contains_no_unrestricted_vars(IfConditions, Then),
	valid_conditions_3(Rest).

valid_conditions_3([drs(_, IfConditions) => drs(_, [Then])|Rest]) :-
    valid_fact(Then),
	strict_literals(IfConditions),
	contains_no_unrestricted_vars(IfConditions, Then),
	valid_conditions_3(Rest).


%% wide_literals(+Conditions)
%
% Succeeds if the conditions contain only facts and negations of conjunctions of facts.
% This is used for level 1. After grouping, such "wide literals" become "strict literals"
% (see below).

wide_literals([]).

wide_literals([Fact|Rest]) :-
    valid_fact_mod(Fact),
    !,
	wide_literals(Rest).

wide_literals([-drs(_,Conditions)|Rest]) :-
    !,
	flat_mod(Conditions),
	wide_literals(Rest).

wide_literals([~drs(_, [-drs(_,Conditions)])|Rest]) :-
    !,
	flat_mod(Conditions),
	wide_literals(Rest).

wide_literals([~drs(_,Conditions)|Rest]) :-
    !,
	flat_mod(Conditions),
	wide_literals(Rest).

% error
wide_literals([_:drs(_, _)|_]) :-
	throw(ar_error('parser.drs-check-1.ThatSubordNotSupported', 'Sentence subordination with "that" is not allowed.')).


%% strict_literals(+Conditions, +Level)
%
% Succeeds if the conditions contain only facts and negations of facts. This is used for the
% levels 2 and 3.

strict_literals([]).

strict_literals([Fact|Rest]) :-
    valid_fact(Fact),
    !,
	strict_literals(Rest).

strict_literals([-drs(_,[Fact])|Rest]) :-
    valid_fact(Fact),
	strict_literals(Rest).

strict_literals([~drs(_,[-drs(_,[Fact])])|Rest]) :-
    valid_fact(Fact),
    strict_literals(Rest).

strict_literals([~drs(_,[Fact])|Rest]) :-
    valid_fact(Fact),
	strict_literals(Rest).

strict_literals([-drs(_,[Fact])|Rest]) :-
    valid_fact(Fact),
	strict_literals(Rest).

strict_literals([~drs(_,[-drs(_,[Fact])])|Rest]) :-
    valid_fact(Fact),
    strict_literals(Rest).

strict_literals([~drs(_,[Fact])|Rest]) :-
    valid_fact(Fact),
	strict_literals(Rest).


%% flat(+Conditions)
%
% Succeeds if the conditions contain only conjunctions.

flat([]).

flat([Fact|Rest]) :-
    valid_fact(Fact),
    !,
	flat(Rest).


%% flat_mod(+Conditions)
%
% Succeeds if the conditions contain only conjunctions and modality boxes (with flat content).

flat_mod([]).

flat_mod([Fact|Rest]) :-
    valid_fact_mod(Fact),
    !,
	flat_mod(Rest).

% error
flat_mod([_:drs(_, _)|_]) :-
	throw(ar_error('parser.drs-check-1.ThatSubordNotSupported', 'Sentence subordination with "that" is not allowed.')).


%% valid_fact(+Term)
%
% Succeeds if the term is an atomic fact.

valid_fact(_-_).


%% valid_fact_mod(+Term)
%
% Succeeds if the term is an atomic fact or a modality box (with flat content).

% error
valid_fact_mod(proper_part_of(_,_)-_) :-
	throw(ar_error('parser.drs-check-1.DistPluralNotSupported', 'Distributive plural ("each of") is not supported.')).

valid_fact_mod(_-_).

valid_fact_mod(Term) :-
    Term =.. [Mod, drs(_, Conditions)],
    is_modal_operator(Mod),
    flat(Conditions),
    !.

valid_fact_mod(Term) :-
    Term =.. [Mod, drs(_, _)],
    is_modal_operator(Mod),
	throw(ar_error('parser.drs-check-1.InvalidModality', 'Modality (can/must/should/may) cannot be applied to complex structures.')).


%% contains_no_vars(+Term)
%
% Succeeds if the term contains no unbound variables.

contains_no_vars(Term) :-
	copy_term(Term, Copy),
	numbervars(Copy, 0, _),
	Term == Copy.


%% contains_no_unrestricted_vars(+BaseTerm, +Term)
%
% Succeeds if every unbound variable in Term does also occur in BaseTerm.

contains_no_unrestricted_vars(BaseTerm, Term) :-
	copy_term([BaseTerm, Term], [BaseCopy, Copy]),
	bind_positive_vars(BaseCopy),
	contains_no_vars(Copy).


bind_positive_vars([]).

bind_positive_vars([- drs(_,_)|Rest]) :-
    !,
    bind_positive_vars(Rest).

bind_positive_vars([~ drs(_,_)|Rest]) :-
    !,
    bind_positive_vars(Rest).

bind_positive_vars([Term|Rest]) :-
	numbervars(Term, 0, _),
    bind_positive_vars(Rest).
