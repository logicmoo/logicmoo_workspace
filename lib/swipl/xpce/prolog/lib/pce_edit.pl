/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2014, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_edit,
          [ editpce/1
          ]).
:- use_module(library(pce)).

/** <module> Find and edit the source location of an XPCE object
*/

%!  editpce(+Spec)
%
%   Edit an xpce `object' from Spec using PceEmacs. Spec is one of:
%
%     - An xpce object that implements <-source
%     - An xpce object, taking its <-class
%     - The name of a class
%     - A term Object->selector
%     - A term Object<-selector
%
%   @see    edit/1 provides the same functionality.

editpce(Spec) :-
    in_pce_thread(editpce_sync(Spec)).

editpce_sync(Spec) :-
    method(Spec, Obj),
    (   get(Obj, source, Location),
        Location \== @nil
    ->  use_module(library(pce_emacs)),
        Goal = start_emacs, Goal,   % fool xref
        send(@emacs, goto_source_location, Location)
    ;   send(Obj, report, warning, 'Can''t find source')
    ).


method(Object, Object) :-
    object(Object),
    send(Object, has_get_method, source),
    !.
method(Object, Class) :-
    object(Object),
    !,
    get(Object, class, Class).
method(ClassName, Class) :-
    atom(ClassName),
    !,
    get(@pce, convert, ClassName, class, Class).
method(->(Receiver, Selector), Method) :-
    !,
    (   atom(Receiver)
    ->  get(@pce, convert, Receiver, class, Class),
        get(Class, send_method, Selector, Method)
    ;   object(Receiver)
    ->  get(Receiver, send_method, Selector, tuple(_, Method))
    ).
method(<-(Receiver, Selector), Method) :-
    !,
    (   atom(Receiver)
    ->  get(@pce, convert, Receiver, class, Class),
        get(Class, get_method, Selector, Method)
    ;   object(Receiver)
    ->  get(Receiver, get_method, Selector, tuple(_, Method))
    ).


