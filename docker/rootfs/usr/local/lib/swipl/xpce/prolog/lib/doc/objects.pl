/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2000-2011, University of Amsterdam
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

:- module(doc_objects, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  module  defines  the  reusable  objects   used  by  the  rendering
primitives. Please note that, as these are declaraced using pce_global/2
directive, it is possible to define any  of these objects prior to using
the document rendering primitives to overrule any of these settings.

For example:

        :- initialization
           new(@h1_above, new(hbox(0, 30))).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *       GLOBAL OBJECTS         *
                 *******************************/

:- pce_global(@browser,         new(@event?receiver?window)).

:- pce_global(@space_rubber,    new(rubber(1, 100, 1, allow))).
:- pce_global(@section_skip,    new(hbox(0, 20, 10))).
:- pce_global(@subsection_skip, new(hbox(0, 20, 10))).
:- pce_global(@br,              new(hbox(rubber := rubber(linebreak := force)))).
:- pce_global(@hfill_rubber,    new(rubber(3, 100))).
:- pce_global(@hfill,           new(hbox(rubber := @hfill_rubber))).
:- pce_global(@quote_margin,    new(hbox(0, 0, 0, rubber(3, 100, 0)))).
:- pce_global(@quote_rubber,    new(rubber(3, 800, 0))).
:- pce_global(@nbsp,            new(tbox(' '))).
:- pce_global(@symbol_style,    new(style(font := symbol))).

:- pce_global(@table_rubber,    new(rubber(3, 100, 100))).

:- pce_global(@h1_above,        new(hbox(0, 20))).
:- pce_global(@h1_below,        new(hbox(0, 5))).
:- pce_global(@h2_above,        new(hbox(0, 20))).
:- pce_global(@h2_below,        new(hbox(0, 5))).
:- pce_global(@h3_above,        new(hbox(0, 20))).
:- pce_global(@h3_below,        new(hbox(0, 5))).
:- pce_global(@h4_above,        new(hbox(0, 20))).
:- pce_global(@h4_below,        new(hbox(0, 5))).

