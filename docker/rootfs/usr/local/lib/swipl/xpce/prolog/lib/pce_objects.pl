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

:- module(pce_objects, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines a number of commonly used global objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

                 /*******************************
                 *         EVENT CONTEXT        *
                 *******************************/

:- pce_global(@event_receiver,  new(@event?receiver)).
:- pce_global(@event_char,      new(@event?id)).
:- pce_global(@node,            new(@event_receiver?node)).
:- pce_global(@tree,            new(@event_receiver?device)).


                 /*******************************
                 *         CODE OBJECTS         *
                 *******************************/

:- pce_global(@true,            new(and)).
:- pce_global(@false,           new(or)).


                 /*******************************
                 *           SPATIALS           *
                 *******************************/

:- pce_global(@center,          new(spatial(xref = x + w/2, yref = y + h/2,
                                            xref = x + w/2, yref = y + h/2))).
:- pce_global(@center_x,        new(spatial(xref = x + w/2, @default,
                                            xref = x + w/2, @default))).
:- pce_global(@center_y,        new(spatial(@default, yref = y + h/2,
                                            @default, yref = y + h/2))).

                 /*******************************
                 *    HANDLES AND CONNECTIONS   *
                 *******************************/

:- pce_global(@north, handle(x + w/2, y,       link, north)).
:- pce_global(@south, handle(x + w/2, y + h,   link, south)).
:- pce_global(@east,  handle(x + w,   y + h/2, link, east)).
:- pce_global(@west,  handle(x,       y + h/2, link, west)).
