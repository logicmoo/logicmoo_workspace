/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2010, University of Amsterdam
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

:- module(draw_extend,
          [ draw_begin_shape/4,
            draw_end_shape/0
          ]).
:- use_module(library(pce)).
:- require([ concat/3
           , ensure_prefix/2
           , forall/2
           , member/2
           ]).


:- pce_begin_class(draw_shape_class, class, "Handle class-level stuff").

variable(hidden_attributes, chain*, get, "Masked attributes").
variable(recognisers,       chain*, get, "Event-handling recognisers").

initialise(Class, Name, Super) :->
    send(Class, send_super, initialise, Name, Super),
    (   get(Class, super_class, SuperClass),
        send(SuperClass, instance_of, draw_shape_class)
    ->  send(Class, slot, hidden_attributes,
             SuperClass?hidden_attributes?copy),
        send(Class, slot, recognisers,
             SuperClass?recognisers)
    ;   send(Class, slot, hidden_attributes, new(chain)),
        send(Class, slot, recognisers, new(chain))
    ).


hidden_attribute(Class, Attr:name) :->
    "Register a hidden attribute"::
    get(Class, hidden_attributes, Hidden),
    send(Hidden, add, Attr).


recogniser(Class, Recogniser:recogniser) :->
    "Register (prepend) a recogniser"::
    get(Class, recognisers, Recognisers),
    send(Recognisers, add, Recogniser).

:- pce_end_class.


draw_begin_shape(Name, Super, Summary, Recognisers) :-
    ensure_prefix(Name, PceName),
    ensure_prefix(Super, PceSuper),
    make_pce_super(PceSuper),
    pce_begin_class(PceName, PceSuper, Summary),
    forall(member(R, Recognisers),
           send(@class, recogniser, R)).

make_pce_super(DrawClass) :-
    get(@pce, convert, DrawClass, class, _),
    !.
make_pce_super(DrawClass) :-
    concat(draw_, PceClass, DrawClass),
    get(@pce, convert, PceClass, class, _),
    new(_NewClass, draw_shape_class(DrawClass, PceClass)).

draw_end_shape :-
    pce_end_class.
