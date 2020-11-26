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


:- module(rule_creator, [
			 create_rules/3  % +DRS, +LabelMap, -Rules
			]).

:- use_module('../op_defs').
:- use_module('../utils').

/** <module> Rule creator

Transforms a DRS that has a valid rule structure into the internal rule representation
used for AceRules.

@author Tobias Kuhn
@version 2007-02-07
*/


%% create_rules(+DRS, +LabelMap, -Rules)
%
% Creates the rule representation on the basis of the DRS and the label map. The label map was
% generated in the meta-preprocessing step.
%
% @see meta_preprocess.pl

create_rules(drs(_, Conds), LabelMap, Rules) :-
	create(Conds, LabelMap, Rules).


create([], _, []).

create([Cond-N/_|RestConds], LabelMap, [(Label, Cond, [])|RestRules]) :-
	!,
	member(N-Label, LabelMap),
	create(RestConds, LabelMap, RestRules).

create([NegCond|RestConds], LabelMap, [(Label, - Cond, [])|RestRules]) :-
	NegCond = (-drs(_, [Cond-N/_])),
	!,
	member(N-Label, LabelMap),
	create(RestConds, LabelMap, RestRules).

create([Cond|RestConds], LabelMap, [(Label, Then, IfConditionsT)|RestRules]) :-
	Cond = (drs(_, IfConditions) => drs(_, [Then-N/_])),
	!,
	member(N-Label, LabelMap),
	transform_cond(IfConditions, IfConditionsT),
	create(RestConds, LabelMap, RestRules).

create([Cond|RestConds], LabelMap, [(Label, - Then, IfConditionsT)|RestRules]) :-
	Cond = (drs(_, IfConditions) => drs([], [-drs(_, [Then-N/_])])),
	!,
	member(N-Label, LabelMap),
	transform_cond(IfConditions, IfConditionsT),
	create(RestConds, LabelMap, RestRules).


transform_cond([], []).

transform_cond([Term-_|Rest], [Term|RestT]) :-
	transform_cond(Rest, RestT).

transform_cond([-drs(_, [Term-_])|Rest], [- Term|RestT]) :-
	transform_cond(Rest, RestT).

transform_cond([~drs(_, [-drs(_, [Term-_])])|Rest], [~ (- Term)|RestT]) :-
    !,
	transform_cond(Rest, RestT).

transform_cond([~drs(_, [Term-_])|Rest], [~ Term|RestT]) :-
	transform_cond(Rest, RestT).
