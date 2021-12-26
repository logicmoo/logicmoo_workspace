/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

:- module(http_xref, []).

:- dynamic
    prolog:called_by/2.
:- multifile
    prolog:called_by/2.

                                        % HTML-WRITE Library
prolog:called_by(html(L, _, _), Called) :-
    html_called_by(L, Called).
prolog:called_by(page(L1, L2, _, _), Called) :-
    html_called_by(L1, C1),
    html_called_by(L2, C2),
    append(C1, C2, Called).
prolog:called_by(page(L, _, _), Called) :-
    html_called_by(L, Called).
                                        % HTTPD
prolog:called_by(send(_, reply_html(_:Term, _)), Called) :-
    prolog:called_by(send(_, reply_html(_:Term)), Called).
prolog:called_by(send(_, reply_html(_:Term, _, _)), Called) :-
    prolog:called_by(send(_, reply_html(_:Term)), Called).
prolog:called_by(send(_, reply_html(_:Term)), [Called]) :-
    catch(Term =.. L, _, fail),
    append(L, [_,_], L2),
    Called =.. L2.

html_called_by(Term, Called) :-
    findall(C, html_called(Term, C), Called).

html_called(Term, Called) :-
    term_member(\Call, Term),
    catch(Call=..L, _, fail),
    append(L, [_,_], L2),
    Called =.. L2.

term_member(X, X).
term_member(X, T) :-
    compound(T),
    arg(_, T, A),
    term_member(X, A).
