/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2000-2012, University of Amsterdam
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

:- module(emacs_html_mode, []).
:- use_module(library(pce)).
:- require([ send_list/3
           ]).

:- op(100, fx, #).                      % `DTD' attributes
:- op(100, fy, *).
:- op(100, fy, ?).
:- op(100, fy, +).

:- pce_global(@html_paragraph_end_regex,
              make_parent_regex).

make_parent_regex(R) :-
    findall(P, par(P), Ps),
    atomic_list_concat(Ps, '|', P0),
    atomic_list_concat(['\\s*(\n|<(', P0, '))'], P1),
    new(R, regex(P1)),
    send(R, ignore_case, @on).

par('h[1-4]').
par('dl').
par('ul').
par('ol').
par('li').
par('dt').
par('tr').
par('p').
par('hr').
par('table').
par('blockquote').


:- emacs_begin_mode(html, language,
                    "Mode for editing HTML documents",
                    [ open_document     = key('\\C-c\\C-o') + button('HTML')
                    ],
                    [ '<'  = open_bracket('>'),
                      paragraph_end(@html_paragraph_end_regex)
                    ]).

:- pce_global(@html_end_element_regex, new(regex('</\\(\\w+\\)[^>]*>'))).

setup_mode(E) :->
    "Switch editor into fill-mode"::
    send(E, fill_mode, @on).


                 /*******************************
                 *            DOCUMENT          *
                 *******************************/

open_document(M) :->
    "Insert document header"::
    send(M, format,
         '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n\n'),
    send(M, make_element, html),
    send(M, make_element, head),
    send(M, make_element, title),
    get(M, caret, Caret),
    send(M, out),                   % out of the title
    send(M, out),                   % out of the head
    send(M, make_element, body),
    send(M, out),                   % hack
    send(M, format, '\n'),
    send(M, caret, Caret).


                 /*******************************
                 *            ELEMENTS          *
                 *******************************/

make_element(M, E:name) :->
    "Insert element at location"::
    (   cdata_element(E)
    ->  send(M, format, '<%s>', E),
        get(M, caret, Caret),
        send(M, format, '</%s>', E)
    ;   send(M, format, '<%s>\n', E),
        get(M, caret, Caret),
        send(M, format, '\n</%s>', E)
    ),
    send(M, caret, Caret).

out(M) :->
    "Get to the end of this element"::
    get(M, text_buffer, TB),
    get(M, caret, Caret),
    get(@html_end_element_regex, search, TB, Caret, _Start),
    get(@html_end_element_regex, register_end, 0, End),
    send(M, caret, End),
    (   send(M, looking_at, '\\s *$')
    ->  send(M, forward_char)
    ;   true
    ).

                 /*******************************
                 *          ELEMENT DEFS                *
                 *******************************/

cdata_element(E) :-
    element(E, Content, _Attributes),
    memberchk(#cdata, Content),
    !.

element(html,  [head, body],   []).
element(head,  [title, meta*], []).
element(body,  [],             []).
element(title, [#cdata],       []).

:- emacs_end_mode.
