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


:- module(debug_output, [
		debug_mode/1,  % -Mode
		debug/0,
		no_debug/0,
		debug/3        % +Type, +Title, +Content
	]).

:- use_module(ape('utils/drs_to_ascii')).
:- use_module(utils).

/** <module> Debug output handler

This modules prints debug information onto the standard error device, if debug-mode is
switched on.

@author Tobias Kuhn
@version 2007-02-06
*/


%% debug_mode(-Mode)
%
% Returns the current debug-mode: 'on' or 'off'.

:- dynamic(debug_mode/1).

debug_mode(off).


%% debug
%
% Switches debug-mode on.

debug :-
    retractall(debug_mode(_)),
    assert(debug_mode(on)).


%% no_debug
%
% Switches debug-mode off.

no_debug :-
    retractall(debug_mode(_)),
    assert(debug_mode(off)).


%% debug(+Type, +Title, +Content)
%
% Writes the debug message, if debug-mode is on.
%
% @param Type One of: term, list, rules, drs.
% @param Title The title of the debug message.
% @param Content The content of the debug message in the form specified by Type.

debug(_, _, _) :-
    debug_mode(off),
    !.

debug(term, Title, Term) :-
    nl(user_error),
    write(user_error, Title),
    nl(user_error),
    print_term(user_error, Term),
    nl(user_error).

debug(list, Title, List) :-
    nl(user_error),
    write(user_error, Title),
    nl(user_error),
    write_terms(user_error, List),
    nl(user_error).

debug(rules, Title, Rules) :-
    nl(user_error),
    write(user_error, Title),
    nl(user_error),
    write_rules(user_error, Rules),
    nl(user_error).

debug(drs, Title, DRS) :-
    nl(user_error),
    write(user_error, Title),
    nl(user_error),
    drs_to_ascii(DRS, DRSA),
    write(user_error, DRSA),
    nl(user_error).
