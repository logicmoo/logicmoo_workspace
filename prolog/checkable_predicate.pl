/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(checkable_predicate, [checkable_predicate/1]).

% An application predicate is a predicate that have at least one clause in the
% application side. We distinguish application from libraries, to extend the
% unused or the wrong dynamic analysis to exported predicates.

:- multifile application_predicate/1.

not_checkable_predicate(P) :-
    predicate_property(P, built_in),
    \+ predicate_property(P, multifile),
    \+ predicate_property(P, dynamic).
not_checkable_predicate(P) :-
    predicate_property(P, exported),
    \+ application_predicate(P).
not_checkable_predicate(P) :-
    predicate_property(P, dynamic),
    \+ application_predicate(P).
not_checkable_predicate(P) :-
    predicate_property(P, imported_from(_)).
not_checkable_predicate(P) :-
    predicate_property(P, foreign).
not_checkable_predicate(P) :-
    predicate_property(P, volatile).

:- meta_predicate checkable_predicate(?).
checkable_predicate(P) :-
    \+ not_checkable_predicate(P).
