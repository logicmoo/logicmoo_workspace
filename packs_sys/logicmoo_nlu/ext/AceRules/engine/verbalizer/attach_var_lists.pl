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


:- module(attach_var_lists, [
		attach_var_lists/2  % +DRSIn, -DRSOut
	]).

:- use_module('../op_defs').

/** <module> Attach variable lists

Creates the correct variable lists for a DRS. The correct variable lists
can be generated only on the basis of the occurrence of the variables. I.e.
these variables lists are redundant, but the ACE verbalizer needs them.

@author Tobias Kuhn
@version 2007-03-08
*/


%% attach_var_lists(+DRSIn, -DRSOut)
%
% Creates the correct variable lists for DRSIn and stores the DRS that contains
% these variable lists in DRSOut.

attach_var_lists(drs(_, CondIn), drs(TopLevelVars, CondOut)) :-
	retractall(toplevel_var(_)),
	collect_toplevel_vars(CondIn),
	attach_neg_vars(CondIn, CondOut),
	findall(V, toplevel_var(V), TopLevelVars).


%% toplevel_var(-Var)
%
% Stores all the toplevel variables occuring in the DRS.

:- dynamic(toplevel_var/1).


%% neg_var(-Var)
%
% Stores all the variables that occur in negated statements inside the DRS.

:- dynamic(neg_var/1).


%% collect_toplevel_vars(DRSConds)
%
% Asserts all the toplevel variables (with toplevel_var/1) that occur in the DRS
% conditions.

collect_toplevel_vars([]).

collect_toplevel_vars([Term-1|_]) :-
	Term =.. TermList,
	member(SK, TermList),
	SK =.. [v | _],
	\+ toplevel_var(SK),
	assert(toplevel_var(SK)),
	fail.

collect_toplevel_vars([_|Rest]) :-
	collect_toplevel_vars(Rest).


%% attach_neg_vars(+DRSCondsIn, -DRSCondOut)
%
% Attaches the correct variable lists to every negated sub-DRS of the list
% DRSCondsIn and stores the transformed conditions in DRSCondOut.
% The same is done for the modality boxes.

attach_neg_vars([], []).

attach_neg_vars([Term-1|RestIn], [Term-1|RestOut]) :-
	attach_neg_vars(RestIn, RestOut).

attach_neg_vars([-drs(_, CondIn)|RestIn], [-drs(Vars, CondOut)|RestOut]) :-
	retractall(neg_var(_)),
	collect_neg_vars(CondIn),
	findall(V, neg_var(V), Vars),
	attach_neg_vars(CondIn, CondOut),  % there can be nested modality boxes
	attach_neg_vars(RestIn, RestOut).

attach_neg_vars([FirstIn|RestIn], [FirstOut|RestOut]) :-
	FirstIn =.. [Mod, drs(_, Cond)],
	is_modal_operator(Mod),
	retractall(neg_var(_)),
	collect_neg_vars(Cond),
	findall(V, neg_var(V), Vars),
	FirstOut =.. [Mod, drs(Vars, Cond)],
	attach_neg_vars(RestIn, RestOut).


%% collect_neg_vars(+DRSConds)
%
% Asserts all the variables (with neg_var/1) that occur in negated statements in
% the list DRSConds.

collect_neg_vars([]).

collect_neg_vars([Term-1|_]) :-
	Term =.. TermList,
	member(SK, TermList),
	SK =.. [v | _],
	\+ toplevel_var(SK),
	\+ neg_var(SK),
	assert(neg_var(SK)),
	fail.

collect_neg_vars([_|Rest]) :-
	collect_neg_vars(Rest).
