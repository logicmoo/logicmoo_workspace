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

:- module(pce_autoload,
        [ pce_autoload/2
        , pce_autoload_all/0
        ]).

:- use_module(pce_boot(pce_principal),
              [ get/3, send/2, op(_,_,_)
              ]).
:- use_module(pce_boot(pce_realise),
              [ pce_realise_class/1,
                pce_prolog_class/1
              ]).
:- require([ is_absolute_file_name/1
           , atomic_list_concat/2
           , absolute_file_name/3
           ]).

:- dynamic
    autoload/2.
:- public
    autoload/2.

%!  pce_autoload(+ClassName, +FileSpec)
%
%   States class `ClassName' can be created by loading the Prolog
%   file `FileSpec'.  This will actually be done if either the class
%   is actually needed by PCE or pce_autoload_all/0 is called.

pce_autoload(Class, PathAlias) :-       % trap library(), demo(), contrib(), ..
    functor(PathAlias, _, 1),
    !,
    retractall(autoload(Class, _)),
    assert(autoload(Class, PathAlias)).
pce_autoload(Class, Abs) :-
    is_absolute_file_name(Abs),
    !,
    absolute_file_name(Abs, Canonical),
    retractall(autoload(Class, _)),
    assert(autoload(Class, Canonical)).
pce_autoload(Class, Local) :-
    prolog_load_context(directory, Dir),
    atomic_list_concat([Dir, /, Local], File),
    pce_host:property(file_extensions(Exts)),
    absolute_file_name(File,
                       [ extensions(Exts),
                         access(exist)
                       ], Abs),
    retractall(autoload(Class, _)),
    assert(autoload(Class, Abs)).

%!  pce_autoload_all
%
%   Load all    classes  declared  using   the    pce_autoload/2
%   directive.  Useful for debugging purposes.

pce_autoload_all :-
    autoload(Class, File),
    \+ get(@classes, member, Class, _),
    \+ pce_prolog_class(Class),
    ensure_loaded(user:File),
    fail.
pce_autoload_all.


register_handler :-
    send(@pce?exception_handlers, append(
         attribute(undefined_class,
                   message(@prolog, call, trap_autoload, @arg1)))).

:- initialization
    register_handler.

pce_ifhostproperty(prolog(swi),
                   (:- '$hide'(trap_autoload/1)),
                   (notrace(G) :- G)).

trap_autoload(Class) :-
    notrace(do_trap_autoload(Class)).

do_trap_autoload(Class) :-
    pce_realise_class(Class),
    !.
do_trap_autoload(Class) :-
    autoload(Class, File),
    load_files(user:File,
               [ autoload(true)
               ]),
    pce_realise_class(Class).
