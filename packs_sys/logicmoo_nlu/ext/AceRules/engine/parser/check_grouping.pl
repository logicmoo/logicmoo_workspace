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


:- module(check_grouping, [
		check_grouping/1   % +DRS
	]).

/** <module> Grouping checker

This module checks whether the grouping that was applied on the DRS was correct. Some variables of a group
are not allowed to occur outside of the group. These variables are unified with a term gv(_). If such
terms occur outside of groups (after the grouping has been performed) then the grouping was invalid. An
error message is raised in these cases ("violated atom restriction").

The details of the predicate grouping are explained in grouping.txt.
For the background of the predicate grouping, see grouping_background.txt.

@author Tobias Kuhn
@version 2007-02-07
@see collect_templates.pl
@see group_predicates.pl
*/


%% check_grouping(+Term)
%
% Raises an error message if invalid grouping was applied on the term. Otherwise, the predicate succeeds
% without side-effects.

check_grouping(Var) :-
    var(Var),
    !.

check_grouping(gv(_)) :-
    throw(ar_error('parser.check-grouping.ViolatedAtomRestr', 'The program violates the atom-restriction.')).

check_grouping(group(_)) :-
    !.

check_grouping([]) :-
	!.

check_grouping([H|T]) :-
	!,
	check_grouping(H),
	check_grouping(T).

check_grouping(Term) :-
	Term =.. [Term],
	!.

check_grouping(Term) :-
	!,
	Term =.. List,
	check_grouping(List).
