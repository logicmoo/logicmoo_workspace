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


:- module(priority_handler, [
		priority_handler/2   % +PrioritiesIn, -PrioritiesOut
	]).

/** <module> Priority handler

Creates the transitive closure for a priority structure, and checks whether the resulting
structure is asymetric. An error message is raised if this asymetry restriction is not
fulfilled.

@author Tobias Kuhn
@version 2007-02-07
*/


%% priority_handler(+PrioritiesIn, -PrioritiesOut)
%
% Generates the transitive closure and checks for asymetry for the priority structure.

priority_handler(PrioritiesIn, PrioritiesOut) :-
    retractall(overrides(_, _)),
    assert_priorities(PrioritiesIn),
    create_transitive_closure,
    is_asymetric,
    findall(overrides(A, B), overrides(A, B), PrioritiesOut),
    !.

priority_handler(_, _) :-
    throw(ar_error('parser.priority-handler.InvalidPriorities', 'The priority structure must not have cycles.')).


%% overrides(-Label1, -Label2)
%
% This dynamic predicate stores the priority structure.

:- dynamic(overrides/2).


%% create_transitive_closure
%
% Creates the transitive closure for the priority structure that is represented with
% the dynamic predicate overrides/2.

create_transitive_closure :-
    findall(
    	overrides(A, C),
    	(overrides(A, B), overrides(B, C), \+ overrides(A, C)),
    	New),
    \+ New = [],
    !,
    assert_priorities(New),
   	create_transitive_closure.

create_transitive_closure.


%% is_asymetric
%
% Succeeds if the priority structure that is represented with the dynamic predicate
% overrides/2 is asymetric.

is_asymetric :-
    \+ overrides(X, X).


%% assert_priorities(+Priorities)
%
% Stores the priorities using the dynamic predicate overrides/2.

assert_priorities([]).

assert_priorities([overrides(A, B)|Rest]) :-
    assert_priority(overrides(A, B)),
    assert_priorities(Rest).


%% assert_priorities(+Priority)
%
% Stores the priority using the dynamic predicate overrides/2.


assert_priority(overrides(A, B)) :-
    overrides(A, B),
    !.

assert_priority(overrides(A, B)) :-
    assert(overrides(A, B)).
