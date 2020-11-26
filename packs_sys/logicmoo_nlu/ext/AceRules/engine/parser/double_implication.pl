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


:- module(double_implication, [
		transform_double_implication/2   % +DRSIn, -DRSOut
	]).

:- use_module('../op_defs').

/** <module> Double implication transformer

This module transforms double implications into single implications. Double implications of the form

==
A -> (B -> C)
A -> (B -> -C)
==

are transformed into

==
(A and B) -> C
(A and B) -> -C
==

@author Tobias Kuhn
@version 2007-03-19
*/


%% transform_double_implication(+DRSIn, -DRSOut)
%
% Transforms a DRS containing double implication into an equivalent DRS without double implication.

transform_double_implication(drs(Refs, CondsIn), drs(Refs, CondsOut)) :-
    transform_conditions(CondsIn, CondsOut).


transform_conditions([], []).

transform_conditions([CondIn|RestIn], [CondOut|RestOut]) :-
    CondIn = drs(Refs1, IfConditions1) => drs(_, [drs(Refs2, IfConditions2) => drs([], [-drs(ThenRefs, NegThenConditions)])]),
    !,
    append(Refs1, Refs2, Refs),
    append(IfConditions1, IfConditions2, IfConditions),
    CondOut = drs(Refs, IfConditions) => drs(_, [-drs(ThenRefs, NegThenConditions)]),
    transform_conditions(RestIn, RestOut).

transform_conditions([CondIn|RestIn], [CondOut|RestOut]) :-
    CondIn = drs(Refs1, IfConditions1) => drs(_, [drs(Refs2, IfConditions2) => drs(ThenRefs, ThenConditions)]),
    !,
    append(Refs1, Refs2, Refs),
    append(IfConditions1, IfConditions2, IfConditions),
    CondOut = drs(Refs, IfConditions) => drs(ThenRefs, ThenConditions),
    transform_conditions(RestIn, RestOut).

transform_conditions([E|RestIn], [E|RestOut]) :-
    transform_conditions(RestIn, RestOut).
