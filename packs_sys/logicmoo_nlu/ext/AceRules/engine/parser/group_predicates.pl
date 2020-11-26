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


:- module(group_predicates, [
			     group_predicates/2	  % +DRSIn, -DRSOut
			    ]).

:- use_module('../op_defs').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module(collect_templates).

/** <module> Group predicates

This module groups the predicates of the DRS. Before using this module, the grouping templates have
to be collected (see collect_templates.pl).

The details of the predicate grouping are explained in grouping.txt.
For the background of the predicate grouping, see grouping_background.txt.

@author Tobias Kuhn
@version 2007-02-07
@see collect_templates.pl
@see check_grouping.pl
*/


%% group_predicates(+DRSIn, -DRSOut)
%
% Groups the predicates of the DRS on the basis of the templates that have been collected.

group_predicates(drs(_, CondIn), drs([], CondOut)) :-
	group_deep(CondIn, CondTemp),
	group_shallow(CondTemp, CondOut).

group_deep([], []).

group_deep([Fact-X|RestIn], [Fact-X|RestOut]) :-
	group_deep(RestIn, RestOut).

group_deep([-drs(_, Cond)|RestIn], [-drs([], Group)|RestOut]) :-
    group_shallow(Cond, Group),
	group_deep(RestIn, RestOut).

group_deep([~drs(_, [-drs(_, Cond)])|RestIn], [~drs([], [-drs([], Group)])|RestOut]) :-
    !,
    group_shallow(Cond, Group),
	group_deep(RestIn, RestOut).

group_deep([~drs(_, Cond)|RestIn], [~drs([], Group)|RestOut]) :-
    group_shallow(Cond, Group),
	group_deep(RestIn, RestOut).

group_deep([ImpNeg1|RestIn], [ImpNeg2|RestOut]) :-
	ImpNeg1 = drs(_, CondIn1) => drs([], [-drs(_, Cond2)]),
	!,
	ImpNeg2 = drs([], CondOut1) => drs([], [-drs([], Group)]),
	group_shallow(Cond2, Group),
	group_deep(CondIn1, CondTemp1),
	group_shallow(CondTemp1, CondOut1),
	group_deep(RestIn, RestOut).

group_deep([Imp1|RestIn], [Imp2|RestOut]) :-
	Imp1 = drs(_, CondIn1) => drs(_, Cond2),
	!,
	Imp2 = drs([], CondOut1) => drs([], Group),
	group_shallow(Cond2, Group),
	group_deep(CondIn1, CondTemp1),
	group_shallow(CondTemp1, CondOut1),
	group_deep(RestIn, RestOut).


group_shallow(PredListIn, PredListOut) :-
	group(PredListIn, PredListTemp),
	!,
	group_shallow(PredListTemp, PredListOut).

group_shallow(PredList, PredList).


group(ListIn, [group(Target)-N/0|ListOut]) :-
	copy_term(ListIn, ListInSk),
	transform_vars_1(ListInSk, ListInT),
	group_template(Template),
	match_template(ListIn, ListInT, Template, ListOut, Target, N).


match_template(List, _ListSk, [], List, [], _).

match_template([E-N/_|ListIn], [S-N/_|ListInSk], Template, ListOut, [E|TargetRest], N) :-
	cut_template_element(Template, S, E, Template2),
	match_template(ListIn, ListInSk, Template2, ListOut, TargetRest, N).

match_template([E|ListIn], [_|ListInSk], Template, [E|ListOut], TargetRest, N) :-
	match_template(ListIn, ListInSk, Template, ListOut, TargetRest, N).


cut_template_element([FirstElement|Template], ElementIn, FirstElement, Template) :-
    \+ \+ FirstElement = ElementIn,
    !.

cut_template_element([FirstElement|TemplateIn], ElementIn, ElementOut, [FirstElement|TemplateOut]) :-
    cut_template_element(TemplateIn, ElementIn, ElementOut, TemplateOut).


transform_vars_1(Var, gv(Var)) :-
    var(Var),
    !.

transform_vars_1([], []) :-
	!.

transform_vars_1([H1|T1], [H2|T2]) :-
	!,
	transform_vars_1(H1, H2),
	transform_vars_1(T1, T2).

transform_vars_1(Term, Term) :-
	Term =.. [Term],
	!.

transform_vars_1(Term1, Term2) :-
	!,
	Term1 =.. List1,
	transform_vars_1(List1, List2),
	Term2 =.. List2.
