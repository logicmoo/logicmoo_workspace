/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
			 CWI, Amsterdam
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

:- module(emacs_show_help, []).
:- use_module(library(pce), [new/2, pce_open/3, send/2, op(_,_,_)]).
:- autoload(library(sgml), [load_html/3]).
:- autoload(library(lynx/html_text),[html_text/2]).
:- autoload(library(pce_emacs), [start_emacs/0]).

/** <module> Capture help in a PceEmacs buffer

This module captures the  output  of   _Documentation_  in  the PceEmacs
buffer ``*Documentation*``.

@tbd	We should made the output of html_text/2 interceptable at a
	higher level of abstraction such that we can create nice
        coloured fragments.
*/

:- multifile
    prolog_help:show_html_hook/1.

prolog_help:show_html_hook(HTMLString) :-
    thread_self(pce),
    start_emacs,
    send(@emacs, location_history),
    setup_call_cleanup(
        open_string(HTMLString, In),
        load_html(stream(In), DOM, []),
        close(In)),
    new(B, emacs_buffer(@nil, '*Documentation*')),
    send(B, mode, text),
    send(B, clear),
    setup_call_cleanup(
        pce_open(B, write, Out),
        ( set_stream(Out, newline(posix)),
          with_output_to(Out, html_text(DOM, []))
        ),
        close(Out)),
    send(B, modified, @off),
    (   send(B?editors, empty)
    ->  send(B, open, tab)
    ;   send(B?editors, for_all, message(@arg1, caret, 0)),
        get(B?editors?head, frame, Frame),
        send(Frame, tab, B, @on),
        send(Frame, expose)
    ),
    send(@emacs, location_history, title := 'Documentation').



