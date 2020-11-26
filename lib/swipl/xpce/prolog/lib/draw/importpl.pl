/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

:- module(draw_importpl,
          [ realise_drawing/2           % +Device, +Term
          ]).
:- use_module(library(pce)).
:- require([ maplist/3
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support library to import drawings from   PceDraw  into applications. To
use this library, make a drawing in PceDraw,   and export it as a Prolog
term, either by dragging the icon  at   the  top-right corner of PceDraw
onto a PceEmacs window running in Prolog mode, or by selecting the `Copy
To ClipBoard' option from the popup  on   this  image and pasting in any
text editor.

The  term  returned  is  what  is  needed  as  the  second  argument  to
realise_drawing/2. The drawing is placed at  the same coordinates as the
initial drawing. If realised the drawing is  realised on a device object
(or figure object), the method ->reference  may   be  used  to place the
reference point of the device at the top-left corner of the bounding box
of the drawing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%       realise_drawing(Device, Drawing)
%
%       Recreates the drawing at the indicates device.  Drawing is a term
%       as created by the PceDraw package library('draw/exportpl.pl').  This
%       file contains a definition of the format of the Drawing term.

realise_drawing(Device, drawing(Objects)) :-
    !,
    draw(Objects, Device).
realise_drawing(Device, Objects) :-
    draw(Objects, Device).

draw([], _).
draw([display(Term, Point)|T], Device) :-
    term_to_object(Term, Object),
    send(Device, display, Object, Point),
    draw(T, Device).
draw([display(Term)|T], Device) :-
    term_to_object(Term, Object),
    send(Device, display, Object),
    draw(T, Device).
draw([connect(Term)|T], Device) :-
    term_to_connection(Term, _Connection),
    draw(T, Device).
draw([compound(Term, Contents, Point)|T], Device) :-
    term_to_object(Term, SubDev),
    realise_drawing(SubDev, Contents),
    send(Device, display, SubDev, Point),
    draw(T, Device).


term_to_object(Term+Attribute, Object) :-
    !,
    term_to_object(Term, Object),
    Attribute =.. [Selector|PlArgs],
    maplist(term_to_object, PlArgs, Args),
    Goal =.. [send, Object, Selector | Args],
    Goal.
term_to_object(Atomic, Atomic) :-
    atomic(Atomic),
    !.
term_to_object(new(Object, Term), Object) :-
    !,
    new(Object, Term).
term_to_object(Term, Object) :-
    new(Object, Term).


term_to_connection(Term+Attribute, Connection) :-
    !,
    term_to_connection(Term, Connection),
    Attribute =.. [Selector|PlArgs],
    maplist(term_to_object, PlArgs, Args),
    Goal =.. [send, Connection, Selector | Args],
    Goal.
term_to_connection(Term, Connection) :-
    Term =.. [Class, From, To, FH, TH],
    make_link(FH, TH, Link),
    NewTerm =.. [Class, From, To, Link],
    new(Connection, NewTerm),
    attach_handle(From, FH),
    attach_handle(To, TH).

attach_handle(Graphical, handle(_, _, _, Name)) :-
    get(Graphical, handle, Name, _),
    !.
attach_handle(Graphical, Handle) :-
    send(Graphical, handle, Handle).

:- dynamic
    link_store/3.

link_store(link, link, @default).

make_link(FH, TH, Link) :-
    handle_kind(FH, FName),
    handle_kind(TH, TName),
    (   link_store(FName, TName, Link)
    ->  true
    ;   new(Link, link(FName, TName)),
        send(Link, lock_object, @on),
        asserta(link_store(FName, TName, Link))
    ).


handle_kind(handle(_X, _Y, Kind, _Name), Kind).

