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

:- module(pce_hyper, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library defines some typical  dependency   relations  based on XPCE
hyper objects. Hyper objects relate two objects and are triggered by the
object-management layer if one of the connected object is destroyed.

The partof_hyper will destroy  the  related   `part'  if  the `whole' is
destroyed, while th mutual_dependency_hyper  destroys   the  other side,
regardless of which of the two is destroyed initially.

Example:

        new(_, partof_hyper(Frame, Dialog))

makes sure the Dialog is destroyed if the Frame disappears.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *     WHOLE-PART RELATION      *
                 *******************************/

:- pce_begin_class(partof_hyper, hyper,
                   "<-to is a part of <-from").

unlink_from(H) :->
    "->destroy the <-to part"::
    get(H, to, Part),
    (   object(Part),
        send(Part, has_send_method, destroy)
    ->  send(Part, destroy)
    ;   free(Part)
    ),
    free(H).

:- pce_end_class(partof_hyper).


                 /*******************************
                 *  MUTUALLY DEPENDANT OBJECTS  *
                 *******************************/

:- pce_begin_class(mutual_dependency_hyper, hyper,
                   "<-to and <-from depend on each other").

unlink_from(H) :->
    "->destroy the <-to part"::
    get(H, to, Dependant),
    (   object(Dependant),
        send(Dependant, has_send_method, destroy)
    ->  send(Dependant, destroy)
    ;   free(Dependant)
    ),
    free(H).

unlink_to(H) :->
    "->destroy the <-from part"::
    get(H, from, Dependant),
    (   object(Dependant),
        send(Dependant, has_send_method, destroy)
    ->  send(Dependant, destroy)
    ;   free(Dependant)
    ),
    free(H).

:- pce_end_class(mutual_dependency_hyper).



