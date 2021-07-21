/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

:- module(pce_mkcommon,
          [ mkcommon/1
          ]).
:- require([ forall/2
           , member/2
           , memberchk/2
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Used to make common.pl; the file holding all predicates available in all
supported Prolog platforms.  To generate this   file, start each of your
Prolog environments and do:

        ?- tell('myprolog.pl'),
           predicate_property(X, built_in),
           write_canonical(X), put(0'.), nl,
           fail ; told.

Next, load this package into Prolog and type:

        ?- tell('common.pl'), mkcommon(['quintus.pl', 'sicstus.pl'], told.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
    common_term/1.

mkcommon([_First|Files]) :-             % JW: had syntax error
    retractall(common_term(_)),
    read_terms(Files, QpTerms),
    forall(member(T, QpTerms), assert(common_term(T))),
    common(Files),
    setof(T, common_term(T), Terms),
    forall(member(T, Terms),
           (write_canonical(built_in(T)), put(.), nl)).

common([]).
common([F|T]) :-
    read_terms(F, Terms),
    findall(T2, (common_term(T2), \+ memberchk(T2, Terms)), Del),
    forall(member(D, Del), retract(common_term(D))),
    common(T).

read_terms(F, Terms) :-
    seeing(Old), see(F),
    read_terms(Terms),
    seen, see(Old).

read_terms([H|T]) :-
    read(Raw),
    Raw \== end_of_file,
    !,
    (   Raw = built_in(H)
    ->  true
    ;   H = Raw
    ),
    read_terms(T).
read_terms([]).



