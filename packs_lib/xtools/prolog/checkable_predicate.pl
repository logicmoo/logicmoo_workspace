/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(checkable_predicate,
          [checkable_predicate/1,
           is_built_in/1]).

:- meta_predicate
    is_built_in(0 ).

% An application predicate is a predicate that have at least one clause in the
% application side. We distinguish application from libraries, to extend the
% unused or the wrong dynamic analysis to exported predicates.

:- multifile application_predicate/1.

% For some reason we can not use the property built_in in saved states:
is_built_in(P) :-
    predicate_property(P, implementation_module(M)),
    ( M == system
    ->true
    ; atom_concat('$', _, M)
    ).

not_checkable_predicate(P) :-
    is_built_in(P),
    \+ predicate_property(P, multifile),
    \+ predicate_property(P, dynamic).
not_checkable_predicate(P) :-
    predicate_property(P, exported),
    \+ application_predicate(P).
not_checkable_predicate(P) :-
    predicate_property(P, dynamic),
    \+ application_predicate(P).
not_checkable_predicate(P) :-
    predicate_property(P, multifile),
    \+ application_predicate(P).
not_checkable_predicate(P) :-
    predicate_property(P, imported_from(_)).
not_checkable_predicate(P) :-
    predicate_property(P, foreign).
not_checkable_predicate(P) :-
    predicate_property(P, volatile).
not_checkable_predicate(P) :-
    predicate_property(P, public).

:- meta_predicate checkable_predicate(?).
checkable_predicate(P) :-
    predicate_property(P, dynamic),
    \+ predicate_property(P, exported),
    \+ predicate_property(P, public),
    !.
checkable_predicate(P) :-
    \+ not_checkable_predicate(P).
