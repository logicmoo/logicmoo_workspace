/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(pce_tagged_connection, []).
:- use_module(library(pce)).
:- require([ forall/2
           ]).

:- pce_begin_class(tagged_connection, connection,
                   "Connection with centered tag").

variable(tag,   graphical*,     get, "Associated tag").

tag(C, Tag:graphical*) :->
    "Associate (new) tag with the connection"::
    get(C, tag, Old),
    (   Old == Tag
    ->  true
    ;   (   Old \== @nil
        ->  send(Old, free)
        ;   true
        ),
        send(C, slot, tag, Tag),
        update_tag(C, _All)
    ).


unlink(C) :->
    "Destroy tags"::
    send(C, send_super, unlink),
    get(C, tag, Tag),
    (   Tag \== @nil
    ->  free(Tag)
    ;   true
    ).

device(C, Dev:device*) :->
    "Update the tag"::
    send(C, send_super, device, Dev),
    update_tag(C, device).

displayed(C, Val:bool) :->
    "Update the tag"::
    send(C, send_super, displayed, Val),
    update_tag(C, displayed).

points(C, X1:[int], Y1:[int], X2:[int], Y2:[int]) :->
    "Update the tag"::
    send(C, send_super, points, X1, Y1, X2, Y2),
    update_tag(C, center).

tag_attribute(center).
tag_attribute(device).
tag_attribute(displayed).

update_tag(C, _) :-
    get(C, tag, @nil),
    !.
update_tag(C, What) :-
    get(C, tag, Tag),
    forall(tag_attribute(What), send(Tag, What, C?What)).

:- pce_end_class.
