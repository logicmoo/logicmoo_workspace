/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2011, VU University Amsterdam
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

:- module(emacs_history, []).
:- use_module(library(pce)).
:- require([default/3]).

                 /*******************************
                 *            HISTORY           *
                 *******************************/

:- pce_begin_class(emacs_history_entry, object,
                   "Entry in the PceEmacs history").

source_location(HE, Loc:source_location) :<-
    "Compute location"::
    get(HE, get_hyper, fragment, source_location, Loc).

unlink(HE) :->
    "Remove from history"::
    send(@emacs?history, delete, HE),
    send_super(HE, unlink).

print_name(HR, Label:char_array) :<-
    "Get human-readable representation of the entry"::
    get(HR, get_hyper, fragment, print_name, Label).

equal(HE, HE2:emacs_history_entry) :->
    "True if both entries start and the same location"::
    get(HE, get_hyper, fragment, text_buffer, TB),
    get(HE2, get_hyper, fragment, text_buffer, TB),
    get(HE, get_hyper, fragment, start, Start),
    get(HE2, get_hyper, fragment, start, Start).

:- pce_end_class.

                 /*******************************
                 *           FRAGMENT           *
                 *******************************/

:- pce_begin_class(emacs_history_fragment, fragment,
                   "Location in the PceEmacs history").

variable(title, char_array*, get, "Title for the menu item").

initialise(HF, TB:text_buffer, Start:int, Len:[int], Title:[char_array]) :->
    "Add fragment to PceEmacs history"::
    (   Len == @default
    ->  get(TB, scan, Start, line, 0, end, End),
        FLen is End+1-Start
    ;   FLen = Len
    ),
    send_super(HF, initialise, TB, Start, FLen, history),
    default(Title, @nil, TheTitle),
    send(HF, slot, title, TheTitle),
    new(HE, emacs_history_entry),
    new(_, mutual_dependency_hyper(HF, HE, history, fragment)),
    send(@emacs?history, location, HE).

source_location(HF, Loc:source_location) :<-
    "Compute location"::
    get(HF, text_buffer, TB),
    get(TB, line_number, HF?start, Line),
    get(TB?file, name, File),
    new(Loc, source_location(File, Line)).

print_name(HF, Label:char_array) :<-
    "Get human-readable representation of the entry"::
    (   get(HF, title, Title), Title \== @nil
    ->  Label = Title
    ;   get(HF, string, Label),
        send(Label, strip),
        get(Label, size, Size),
        Size > 0
    ->  true
    ;   get(HF, source_location, source_location(File, Line)),
        file_base_name(File, FileBase),
        new(Label, string('%s:%d', FileBase, Line))
    ).

:- pce_end_class.

