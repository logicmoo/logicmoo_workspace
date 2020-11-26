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


:- module(stable_checker, [
		stable_checker/2  % +Rules, +Mode
	]).

:- use_module('../op_defs').

/** <module> Stable checker

This module checks whether a rule set fulfills the restrictions of stable model semantics. These
restrictions depend on whether strong negation is used or not.

@author Tobias Kuhn
@version 2007-02-09
*/


%% stable_checker(+Rules, +Mode)
%
% Succeeds if Rules are correct rule representations for the stable semantics. Mode is stable or
% stable_strong.

stable_checker([], _).

stable_checker([overrides(_,_)|_], _) :-
    throw(ar_error('stable-interpreter.stable-checker.OverridesNotSupported', 'Override statements are not supported.')).

stable_checker([(_, Head, Body)|Rest], Mode) :-
    valid_head(Head, Mode),
    valid_body(Body, Mode),
    stable_checker(Rest, Mode).


valid_head(- _, stable) :-
    throw(ar_error('stable-interpreter.stable-checker.NegationInFactOrHead', 'Negation in facts or in the head of a rule is not allowed.')).

valid_head(_, _).


valid_body([], _).

valid_body([~ (- _)|_], stable) :-
    throw(ar_error('stable-interpreter.stable-checker.DoubleNegation', 'Double negation is not allowed.')).

valid_body([_|Rest], Mode) :-
    valid_body(Rest, Mode).
