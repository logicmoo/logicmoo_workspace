/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2001-2015, University of Amsterdam
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

:- module(gui_tracer,
          [ guitracer/0,
            noguitracer/0,              % Switch it off
            gtrace/0,                   % Start tracer and trace
            gtrace/1,                   % :Goal
            gspy/1,                     % Start tracer and set spypoint
            gdebug/0                    % Start tracer and debug
          ]).
:- use_module(library(pce)).
:- set_prolog_flag(generate_debug_info, false).
:- meta_predicate
    gtrace(0),
    gspy(:).

/** <module> Graphical debugger utilities

This module provides utilities that use   the  graphical debugger rather
than the conventional 4-port commandline debugger.  This library is part
of XPCE.

@see    library(threadutil) provides another set t* predicates that
        deal with threads.
*/

%!  guitracer is det.
%
%   Enable the graphical debugger.  A   subsequent  call  to trace/0
%   opens the de debugger window. The   tranditional debugger can be
%   re-enabled using noguitracer/0.

guitracer :-
    current_prolog_flag(gui_tracer, true),
    !.
guitracer :-
    current_prolog_flag(gui_tracer, _),
    !,
    set_prolog_flag(gui_tracer, true),
    visible(+cut_call),
    print_message(informational, gui_tracer(true)).
guitracer :-
    in_pce_thread_sync(
        load_files([library('trace/trace')],
                   [ silent(true),
                     if(not_loaded)
                   ])),
    set_prolog_flag(gui_tracer, true),
    visible(+cut_call),
    print_message(informational, gui_tracer(true)).

%!  noguitracer is det.
%
%   Disable the graphical debugger.
%
%   @see guitracer/0

noguitracer :-
    current_prolog_flag(gui_tracer, true),
    !,
    set_prolog_flag(gui_tracer, false),
    visible(-cut_call),
    print_message(informational, gui_tracer(false)).
noguitracer.

%!  gtrace is det.
%
%   Like trace/0, but uses the graphical tracer.

:- '$hide'(gtrace/0).                   % don't trace it

gtrace :-
    guitracer,
    trace.

%!  gtrace(:Goal) is det.
%
%   Trace Goal in a separate thread,  such that the toplevel remains
%   free for user interaction.

gtrace(Goal) :-
    guitracer,
    thread_create(trace_goal(Goal), Id, [detached(true)]),
    print_message(informational, gui_tracer(in_thread(Id, Goal))).

:- meta_predicate trace_goal(0).

trace_goal(Goal) :-
    catch(trace_goal_2(Goal), _, true),
    !.
trace_goal(_).

trace_goal_2(Goal) :-
    setup_call_catcher_cleanup(
        trace,
        Goal,
        Catcher,
        finished(Catcher, Det)),
    notrace,
    (   Det == true
    ->  true
    ;   in_pce_thread_sync(send(@(display), confirm, 'Retry goal?'))
    ->  trace, fail
    ;   !
    ).

:- '$hide'(finished/2).

finished(Reason, Det) :-
    notrace,
    print_message(informational, gui_tracer(completed(Reason))),
    (   Reason == exit
    ->  Det = true
    ;   Det = false
    ).

%!  gspy(:Spec) is det.
%
%   Same as spy/1, but uses the graphical debugger.

gspy(Predicate) :-
    guitracer,
    spy(Predicate).

%!  gdebug is det.
%
%   Same as debug/0, but uses the graphical tracer.

gdebug :-
    guitracer,
    debug.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(gui_tracer(true)) -->
    [ 'The graphical front-end will be used for subsequent tracing' ].
prolog:message(gui_tracer(false)) -->
    [ 'Subsequent tracing uses the commandline tracer' ].
prolog:message(gui_tracer(in_thread(Id, _Goal))) -->
    [ 'Debugging goal in new thread ~q'-[Id] ].
prolog:message(gui_tracer(completed(Reason))) -->
    [ 'Goal completed: ~q~n'-[Reason] ].
