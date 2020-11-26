/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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

:- module(pce_main,
          [ pce_loop/2,                 % :Goal, +Argv
            pce_loop/1,                 % :Goal
            pce_main_loop/1,            % :Goal
            dispatch_for_frames/1       % +FrameList
          ]).

:- meta_predicate
    pce_loop(:),
    pce_loop(:, +),
    pce_main_loop(:).

:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- require([ append/3
           , call/2
           , ignore/1
           , unix/1
           , chain_list/2
           ]).

%!  pce_main_loop(+Goal)
%
%   Simple XPCE runtime toplevel loop.  This goal extracts the command
%   line arguments, calls `call(Goal, CmdLineArgs)' and waits for all
%   frames created by this call to be invisible.  Then it will halt/0.

pce_main_loop(Goal) :-
    setup_runtime,
    current_prolog_flag(argv, Argv),
    pce_loop(Goal, Argv),
    halt.

%!  pce_loop(+Goal).
%!  pce_loop(+Goal, +Argv:list).
%
%   Runs `Goal', finds all toplevel frames created and then dispatches
%   events untill the last frame is destroyed.

pce_loop(Goal) :-
    pce_loop(Goal, []).
pce_loop(Goal, Argv) :-
    get(@display?frames, find_all, @arg1?kind == toplevel, FramesOld),
    call(Goal, Argv),
    get(@display?frames, find_all, @arg1?kind == toplevel, FramesNew),
    get(FramesNew, copy, FrameChain),
    send(FrameChain, subtract, FramesOld),
    chain_list(FrameChain, Frames),
    dispatch_for_frames(Frames).

dispatch_for_frames([]) :- !.
dispatch_for_frames(Frames) :-
    (   catch(send(@display, dispatch), E,
              (   message_to_string(E, Msg),
                  send(@display, inform, Msg),
                  (   E == '$aborted'
                  ->  throw(E)
                  ;   true
                  )
              ))
    ->  true
    ;   true
    ),
    existing_frames(Frames, Existing),
    dispatch_for_frames(Existing).

existing_frames([], []).
existing_frames([H|T0], [H|T]) :-
    object(H),
    send(H, instance_of, frame),
    get(H, status, Status),
    Status \== unmapped,
    !,
    existing_frames(T0, T).
existing_frames([_|T0], T) :-
    existing_frames(T0, T).

setup_runtime :-
    (   get(@pce, is_runtime_system, @on)
    ->  true
    ;   send(@pce, trap_errors, @off)
    ),
    catch(set_prolog_flag(debug_on_error, false), _, true).


