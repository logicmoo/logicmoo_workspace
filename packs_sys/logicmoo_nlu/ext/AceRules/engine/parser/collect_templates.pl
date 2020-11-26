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


:- module(collect_templates, [
			    clear_templates/0,
			    group_template/1,     % -Template
			    collect_templates/1   % +DRS
			   ]).

:- use_module('../skolemizer/skolemizer').
:- use_module('../skolemizer/unskolemizer').
:- use_module('../op_defs').
:- use_module('../utils').

/** <module> Group-templates collector

This module collects the templates that are afterwards used to group the predicates of a DRS.

The details of the predicate grouping are explained in grouping.txt.
For the background of the predicate grouping, see grouping_background.txt.

@author Tobias Kuhn
@version 2007-02-07
@see group_predicates.pl
@see check_grouping.pl
*/


%% group_template(-Template)
%
% Stores the templates that have been collected.

:- dynamic(group_template/1).

%% clear_templates
%
% Removes all the stored templates.

clear_templates :-
    retractall(group_template(_)).


%% collect_templates(+DRS)
%
% Collects the templates of the DRS.

collect_templates(drs(_, Conditions)) :-
    copy_term(Conditions, ConditionsCopy),
    unskolemize(v, ConditionsCopy, ConditionsUnsk),
    reset_skolemizer(ct),
	collect_from_condlist(ConditionsUnsk).
	

%% collect_from_condlist(CondList)
%
% Collects the templates from the list of conditions.

collect_from_condlist([]).

collect_from_condlist([Fact-_|Rest]) :-
    skolemize(ct, Fact),
	collect_from_condlist(Rest).

collect_from_condlist([-drs(_, NegConditions)|Rest]) :-
	collect_from_condlist(Rest),
	assert_group_template(NegConditions).

collect_from_condlist([~drs(_, [-drs(_, NafNegConditions)])|Rest]) :-
    !,
	collect_from_condlist(Rest),
	assert_group_template(NafNegConditions).

collect_from_condlist([~drs(_, NafConditions)|Rest]) :-
	collect_from_condlist(Rest),
	assert_group_template(NafConditions).

collect_from_condlist([drs(_, IfConditions) => drs([], [-drs(_, NegThenConditions)])|Rest]) :-
	!,
	collect_from_condlist(IfConditions),
	collect_from_condlist(Rest),
	assert_group_template(NegThenConditions).

collect_from_condlist([drs(_, IfConditions) => drs(_, ThenConditions)|Rest]) :-
	collect_from_condlist(IfConditions),
	collect_from_condlist(Rest),
	assert_group_template(ThenConditions).


%% assert_group_template(Template)
%
% Stores the template using the dynamic predicate group_template/1.

%assert_group_template([_-_]) :-
%    % group with one predicate: ignore!
%    !.

assert_group_template(Group1) :-
    remove_sentence_nr(Group1, Group2),
    transform_vars_1(Group2, Group3),
    unskolemize(ct, Group3, Group4),
    sort_predicates(Group4, Group5),
    (is_group_template(Group5) ->
       % template is already known
       true
    ;
       transform_vars_2(Group5, Group6),
       assert(group_template(Group6))
    ).


%% sort_predicates(+PredListIn, -PredListOut)
%
% NOT YET IMPLEMENTED!
% Sorts the list of predicates. This is needed to ensure that equivalent groups
% can always unify.

sort_predicates(X, X).
    % not yet implemented.


is_group_template(CondList) :-
    group_template(GroupTemplate),
    CondList == GroupTemplate.


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


transform_vars_2(Var, Var) :-
    var(Var),
    !.

transform_vars_2(gv(V), V) :-
    !,
    skolemize(gv, V).

transform_vars_2([], []) :-
	!.

transform_vars_2([HeadIn|TailIn], [HeadOut|TailOut]) :-
	!,
	transform_vars_2(HeadIn, HeadOut),
	transform_vars_2(TailIn, TailOut).

transform_vars_2(Term, Term) :-
	Term =.. [Term],
	!.

transform_vars_2(TermIn, TermOut) :-
	!,
	TermIn =.. ListIn,
	transform_vars_2(ListIn, ListOut),
	TermOut =.. ListOut.
    
