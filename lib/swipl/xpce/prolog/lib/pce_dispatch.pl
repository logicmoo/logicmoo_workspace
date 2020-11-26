/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2009-2015, University of Amsterdam
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

:- module(pce_dispatch,
          [ pce_dispatch/1,             % +Options
            pce_end_dispatch/0,
            pce_call/1                  % :Goal
          ]).
:- use_module(library(pce)).

:- meta_predicate
    pce_call(0).

/** <module> Run XPCE in a separate thread

This module allows one to run XPCE in   a separate thread =pce=. This is
especially  nice  if  xpce  is  only  used  to  support  the  SWI-Prolog
development tools because it ensures that   the  tools remain responsive
while the main thread executes long-running goals.

This module can be deactivated by setting the flag `xpce_threaded`:

  ==
  :- set_prolog_flag(xpce_threaded, false).
  ==
*/

:- predicate_options(pce_dispatch/1, 1,
                     [ pass_to(system:thread_create/3, 3)
                     ]).

%!  pce_dispatch(+Options) is det.
%
%   Create a new thread =pce= that takes   care  of the XPCE message
%   loop. This predicate has no effect  if dispatching is already on
%   another thread than the =main=.  The   loop  can  be ended using
%   pce_end_dispatch/0.

pce_dispatch(Options) :-
    with_mutex(pce_dispatch, pce_dispatch_(Options)).

pce_dispatch_(_) :-
    pce_thread(pce),
    !.
pce_dispatch_(Options) :-
    thread_self(Me),
    thread_create(pce_dispatcher(Me), _,
                  [ alias(pce),
                    debug(false)
                  | Options
                  ]),
    thread_get_message(pce_dispatch).

:- dynamic
    end_pce_dispatcher/1.

pce_dispatcher(Origin) :-
    set_pce_thread,
    thread_self(Me),
    retractall(pce:pce_thread(_)),
    assert(pce:pce_thread(Me)),
    thread_send_message(Origin, pce_dispatch),
    set_prolog_flag(debug_on_error, false),         % avoid the debugger
    set_prolog_flag(generate_debug_info, true),     % Started with false
    repeat,
        catch(pce_dispatch, E, true),
        (   var(E)
        ->  true
        ;   print_message(error, E)
        ),
    retract(end_pce_dispatcher(Sender)),
    !,
    thread_send_message(Sender, end_pce_dispatcher).

end(Requester) :-
    assert(end_pce_dispatcher(Requester)).

%!  pce_end_dispatch is det.
%
%   End the XPCE dispatcher loop started with pce_dispatch/1.

pce_end_dispatch :-
    thread_self(Me),
    in_pce_thread(end(Me)),
    thread_get_message(end_pce_dispatcher),
    set_pce_thread,
    thread_self(Me),
    retractall(pce:pce_thread(_)),
    assert(pce:pce_thread(Me)).

%!  pce_call(:Goal) is det.
%
%   Run Goal in the XPCE thread.
%
%   @deprecated New code should used in_pce_thread/1.

pce_call(Goal) :-
    in_pce_thread(Goal).
