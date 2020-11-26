/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2019, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Module PCE.  This module defines the core   of  XPCE.  It is designed in
such a way that it  may  be   compiled  using  the SWI-Prolog qcompile/1
compiler, which makes XPCE an autoloadable module of SWI-Prolog.

Various things are Prolog-implementation specific in this module and
therefore each Prolog system will require a different version of this
module.

This module only defines some  paths,  some   things  to  make  the .qlf
compiler work on it and  finally  it   just  loads  the XPCE modules and
reexports the content of these files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(pce,
          [ new/2, free/1,              % pce_principal predicates

            send/2, send/3, send/4, send/5, send/6, send/7,
            send/8,

            get/3, get/4, get/5, get/6, get/7, get/8,

            send_class/3,
            get_class/4,
            object/1, object/2,

            pce_global/2,               % pce_global
            pce_autoload/2,             % pce_autoload
            pce_autoload_all/0,

            pce_term_expansion/2,
            pce_compiling/1,            % -Class
            pce_compiling/2,            % -Class, -Path
            pce_begin_recording/1,
            pce_end_recording/0,

            pce_register_class/1,
            pce_extended_class/1,
            pce_begin_class_definition/4,
            pce_prolog_class/1,
            pce_prolog_class/2,

            pce_catch_error/2,          % pce_error
            pce_open/3,
            in_pce_thread/1,            % :Goal
            in_pce_thread_sync/1,       % :Goal
            set_pce_thread/0,
            pce_thread/1,               % -Thread
            pce_dispatch/0,

            op(200, fy,  @),
            op(250, yfx, ?),
            op(800, xfx, :=)
          ]).

:- multifile
    on_load/0.

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    in_pce_thread_sync(0).

                /********************************
                *      LOAD COMMON PLATFORM     *
                ********************************/

:- multifile user:file_search_path/2.

user:file_search_path(pce_boot, pce(prolog/boot)).

:- load_files([ pce_boot(pce_expand),
                pce_boot(pce_pl),
                pce_boot(pce_principal),
                pce_boot(pce_error),
                pce_boot(pce_global),
                pce_boot(pce_expansion),
                pce_boot(pce_realise),
                pce_boot(pce_goal_expansion),
                pce_boot(pce_autoload),
                pce_boot(pce_editor),
                pce_boot(pce_keybinding),
                pce_boot(pce_portray),
                'english/pce_messages'
              ],
              [ qcompile(part),         % compile boot files as part of pce.qlf
                silent(true)
              ]).
:- use_module(pce_dispatch).

%!  pce_thread(-Thread) is det.
%
%   True if Thread is the Prolog thread that runs the graphics
%   message loop.
%
%   @see pce_dispatch/1.

:- current_prolog_flag(threads, HasThreads),
   create_prolog_flag(xpce_threaded, HasThreads, [keep(true)]).

:- dynamic
    pce_thread/1.

start_dispatch :-
    (   current_prolog_flag(xpce_threaded, true)
    ->  pce_dispatch([])
    ;   true
    ).

%!  in_pce_thread_sync(:Goal) is semidet.
%
%   Same as in_pce_thread/1, but wait  for   Goal  to  be completed.
%   Success depends on the success of executing Goal. If Goal throws
%   an exception, this exception is re-thrown by in_pce_thread/1.
%
%   Possible bindings of Goal are returned,   but  be aware that the
%   term has been _copied_. If in_pce_thread_sync/1 is called in the
%   thread running pce, it behaves as once/1.

in_pce_thread_sync(Goal) :-
    thread_self(Me),
    pce_thread(Me),
    !,
    Goal,
    !.
in_pce_thread_sync(Goal) :-
    term_variables(Goal, Vars),
    pce_principal:in_pce_thread_sync2(Goal-Vars, Vars).

:- initialization
    start_dispatch.

set_version :-
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    format(string(PlId),
           'SWI-Prolog version ~w.~w.~w', [Major, Minor, Patch]),
    send(@prolog, system, PlId).

:- initialization set_version.

get_pce_version :-
    (   current_prolog_flag(xpce_version, _)
    ->  true
    ;   get(@pce, version, name, Version),
        create_prolog_flag(xpce_version, Version, [])
    ).

:- initialization get_pce_version.

run_on_load :-
    forall(on_load, true).

:- initialization run_on_load.


                 /*******************************
                 *           CONSOLE            *
                 *******************************/

%:- send(@pce, console_label, 'XPCE/SWI-Prolog').


                /********************************
                *       PROLOG LIBRARIES        *
                ********************************/

:- multifile
    user:file_search_path/2.

user:file_search_path(demo,    pce('prolog/demo')).
user:file_search_path(contrib, pce('prolog/contrib')).
user:file_search_path(image,   pce(bitmaps)).


                 /*******************************
                 *            HOOKS             *
                 *******************************/

:- use_module(library(swi_hooks)).

                 /*******************************
                 *         EDIT HOOKS           *
                 *******************************/

%       make sure SWI-Prolog edit/0 loads the XPCE edit hooks.

:- multifile
    prolog_edit:load/0,
    prolog:locate_clauses/2.

prolog_edit:load :-
    ensure_loaded(library(swi_edit)).

                 /*******************************
                 *          LIST HOOKS          *
                 *******************************/

%!  prolog:locate_clauses(Term, Refs)
%
%   Locate a list of clause-references from a method-specification
%   like Class->Method.
%
%   see library(listing).

prolog:locate_clauses(Term, Refs) :-
    (   Term = ->(_,_)
    ;   Term = <-(_,_)
    ),
    !,
    findall(R, method_clause(Term, R), Refs).

match_id(->(Class, Method), Id) :-
    atomic(Class), atomic(Method),
    !,
    atomic_list_concat([Class, (->), Method], Id).
match_id(->(_Class, _Method), _Id).
match_id(<-(Class, Method), Id) :-
    atomic(Class), atomic(Method),
    !,
    atomic_list_concat([Class, (<-), Method], Id).
match_id(<-(_Class, _Method), _Id).

method_clause(->(Class, Send), Ref) :-
    match_id((Class->Send), Id),
    clause(pce_principal:send_implementation(Id, _M, _O), _B, Ref),
    atom(Id),
    atomic_list_concat([Class,Send], '->', Id).
method_clause(<-(Class, Get), Ref) :-
    match_id(<-(Class, Get), Id),
    clause(pce_principal:get_implementation(Id, _M, _O, _R), _B, Ref),
    atom(Id),
    atomic_list_concat([Class,Get], '->', Id).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(Spec) -->
    pce_message(Spec).
prolog:message(context_error(Goal, Context, What)) -->
    [ '~w: ~w '-[Goal, What] ],
    pce_message_context(Context).
prolog:message(type_error(Goal, ArgN, Type, _Value)) -->
    [ '~w: argument ~w must be a ~w'-[Goal, ArgN, Type], nl ].
