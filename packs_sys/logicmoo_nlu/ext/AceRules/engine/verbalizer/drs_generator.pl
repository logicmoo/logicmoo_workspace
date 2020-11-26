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


:- module(drs_generator, [
		generate_drs/2  %  +Facts, -DRS
	]).

:- use_module('../skolemizer/skolemizer').
:- use_module('../skolemizer/unskolemizer').
:- use_module('../op_defs').

/** <module> DRS generator

This module creates a preliminary DRS structure for a set of facts.

@author Tobias Kuhn
@version 2007-03-08
*/


%% generate_drs(+Facts, -DRS)
%
% Generates a DRS structure for the facts. No variable lists are generated.
% The DRS can still contain grouped predicates.

generate_drs(Facts, drs([], Cond)) :-
	transform_facts(Facts, Cond).


%% transform_fact(+Facts, -CondList)
%
% Transforms the facts into a list of conditions. These conditions contain
% negated sub-DRSes for negated facts.

transform_facts([], []).

transform_facts([-group(Group)|RestFacts], [-drs([],GroupCond)|RestCond]) :-
	!,
	unskolemize(gv, Group, Group2),
	skolemize(v, Group2),
	transform_facts(Group2, GroupCond),
	transform_facts(RestFacts, RestCond).

transform_facts([-NegFact|RestFacts], [-drs([],[NegFact-1])|RestCond]) :-
	!,
	transform_facts(RestFacts, RestCond).

transform_facts([group(Group)|RestFacts], Cond) :-
	!,
	unskolemize(gv, Group, Group2),
	skolemize(v, Group2),
	append(Group2, RestFacts, NewFacts),
	transform_facts(NewFacts, Cond).

transform_facts([Fact|RestFacts], [Cond|RestCond]) :-
	Fact =.. [Mod, ModFacts],
	is_modal_operator(Mod),
	!,
	transform_facts(ModFacts, ModConds),
	Cond =.. [Mod, drs([],ModConds)],
	transform_facts(RestFacts, RestCond).

transform_facts([Fact|RestFacts], [Fact-1|RestCond]) :-
	!,
	transform_facts(RestFacts, RestCond).
