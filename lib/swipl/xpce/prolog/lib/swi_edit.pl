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

:- module(pce_nedit, []).
:- use_module(library(pce)).
:- use_module(library(pce_meta)).

:- multifile
    prolog_edit:locate/3,           % +Partial, -FullSpec, -Location
    prolog_edit:locate/2,
    prolog_edit:select_location/3.


                 /*******************************
                 *      FINDING LOCATIONS       *
                 *******************************/

prolog_edit:(locate(ClassName, class(ClassName), Location) :-
        locate(class(ClassName), Location)).

prolog_edit:locate(class(ClassName), Location) :-       % class(Name)
    atom(ClassName),
    get(@pce, convert, ClassName, class, Class),
    \+ get(Class, creator, built_in),
    source(Class, Location).
prolog_edit:locate(SourceLoc, [file(File)|Extra]) :-
    object(SourceLoc),
    send(SourceLoc, instance_of, source_location),
    get(SourceLoc, file_name, File),
    (   get(SourceLoc, line_no, Line),
        Line \== @nil
    ->  Extra = [line(Line)]
    ;   Extra = []
    ).
prolog_edit:locate(Object, Location) :-                 % @reference
    source(Object, Location).
prolog_edit:locate(Object, Location) :-                 % @reference
    object(Object),
    get(Object, class, Class),
    source(Class, Location).
prolog_edit:locate(send(Receiver, Selector), Location) :-
    receiver_class(Receiver, Class),
    (   implements(Class, send(Selector), Method),
        source(Method, Location)
    ;   method_source(Class, send(Selector), Location)
    ).
prolog_edit:locate(get(Receiver, Selector), Location) :-
    receiver_class(Receiver, Class),
    (   implements(Class, get(Selector), Method),
        source(Method, Location)
    ;   method_source(Class, get(Selector), Location)
    ).
prolog_edit:(locate(->(Receiver, Selector), Location) :- !,
        locate(send(Receiver, Selector), Location)).
prolog_edit:(locate(<-(Receiver, Selector), Location) :- !,
        locate(get(Receiver, Selector), Location)).

source(Object, [file(Path)|T]) :-
    object(Object),
    send(Object, has_get_method, source),
    get(Object, source, Loc),
    Loc \== @nil,
    get(Loc, file_name, FileName),
    absolute_file_name(FileName, Path),
    get(Loc, line_no, Line),
    (   integer(Line)
    ->  T = [line(Line)]
    ;   T = []
    ).

receiver_class(Object, Class) :-
    object(Object),
    !,
    get(Object, class_name, Class).
receiver_class(Class, Class).

method_source(ClassName, send(Selector), [file(File),line(Line)]) :-
    var(ClassName),
    pce_principal:pce_lazy_send_method(Selector, ClassName, Binder),
    arg(4, Binder, source_location(File, Line)),
    \+ get(@classes, member, ClassName, _).
method_source(ClassName, get(Selector), [file(File),line(Line)]) :-
    var(ClassName),
    pce_principal:pce_lazy_get_method(Selector, ClassName, Binder),
    arg(4, Binder, source_location(File, Line)),
    \+ get(@classes, member, ClassName, _).


                 /*******************************
                 *             SELECT           *
                 *******************************/

%       Use GUI-based selection if the request comes from the GUI!

prolog_edit:select_location(Pairs, _Spec, Location) :-
    Pairs \= [_],                                   % direct match
    object(@event),
    send(@event, instance_of, event),              % GUI initiated
    !,
    (   Pairs == []
    ->  send(@event?receiver, report, error, 'No match'),
        Location = []                               % Cancel
    ;   get(@event?receiver, frame, Frame),
        new(D, dialog('Select object to edit')),
        send(D, append, label(title, 'Click object to edit')),
        length(Pairs, Len),
        LBH is min(Len, 10),
        send(D, append, new(LB, list_browser(@default, 30, LBH))),
        fill_browser(Pairs, 1, LB),
        send(LB, select_message, message(D, return, LB?selection?object)),
        send(D, append,
             new(C, button(cancel, message(D, destroy)))),
        send(C, alignment, right),
        send(D, resize_message, message(D, layout, @arg2)),
        send(D, modal, transient),
        send(D, transient_for, Frame),
        (   get(D, confirm_centered, Frame?area?center, Rval)
        ->  send(D, destroy),
            Location = Rval
        ;   Location = []
        )
    ).

fill_browser([], _, _).
fill_browser([Location-Spec|T], N, LB) :-
    message_to_string(edit(target(Location-Spec, N)), Label),
    send(LB, append,
         dict_item(Label, object := prolog(Location))),
    NN is N + 1,
    fill_browser(T, NN, LB).
