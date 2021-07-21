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

:- module(dia_meta, [class_of_type/2]).
:- use_module(library(pce)).
:- require([ chain_list/2
           , member/2
           ]).

:- pce_extend_class(object).

dia_argument_type(Obj, Selector:name, Type:type) :<-
    get(Obj, get_method, Selector, tuple(_, Implementation)),
    get(Implementation, return_type, Type).

:- pce_end_class.


class_of_type(Type, Class) :-
    get(Type, kind, class),
    get(Type, context, Class).
class_of_type(Type, Class) :-
    get(Type, kind, class_object),
    get(Type, context, Class).
class_of_type(Type, @object_class) :-
    get(Type, kind, any).
class_of_type(Type, Class) :-
    get(Type, kind, alias),
    get(Type, context, Type2),
    class_of_type(Type2, Class).
class_of_type(Type, Class) :-
    get(Type, kind, member),
    get(Type, context, Type2),
    class_of_type(Type2, Class).
class_of_type(Type, Class) :-
    get(Type, supers, Supers),
    Supers \== @nil,
    chain_list(Supers, List),
    member(T, List),
    class_of_type(T, Class).
