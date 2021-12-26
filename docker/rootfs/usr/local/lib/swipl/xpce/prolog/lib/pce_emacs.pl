/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.nl/projects/xpce/
    Copyright (c)  1985-2011, University of Amsterdam
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

:- module(start_emacs,
          [ emacs/0
          , emacs/1                             % x File
          , start_emacs/0
          , emacs_server/0
          , emacs_toplevel/0
          ]).
:- use_module(library(pce)).
:- require([ append/3
           , maplist/3
           , unix/1
           ]).

:- pce_autoload(emacs,      library('emacs/emacs')).
:- pce_autoload(emacs_view, library('emacs/emacs')).

:- pce_global(@emacs_buffers, new(dict)).
:- pce_global(@emacs, new(emacs(@emacs_buffers))).


/** <module> PceEmacs toplevel

This module provides predicates to start  PceEmacs. PceEmacs is an clone
of GNU-Emacs written in  XPCE.  Modes  are   XPCE  classes  that  can be
extended in Prolog.

@see    Set Prolog flag editor to pce_emacs to make PceEmacs the default
        for edit/1.
*/

%!  start_emacs is det.
%
%   Create PceEmacs, but no buffers nor windows.

start_emacs :-
    register_emacs,
    (   object(@emacs)
    ->  true
    ;   in_pce_thread_sync(send(@emacs, start))
    ).


%!  register_emacs is det.
%
%   If the user has not specified a specific editor and has started
%   PceEmacs, make it the default editor.

register_emacs :-
    (   current_prolog_flag(editor, '$EDITOR')
    ->  set_prolog_flag(editor, pce_emacs)
    ;   true
    ).


%!  emacs_server is det.
%
%   Create a PceEmacs, ready to run as an unattended background
%   server.

emacs_server :-
    start_emacs,
    send(@pce, trap_errors, @off),
    send(@pce, console_label, 'PceEmacs Server').

%!  emacs is det.
%
%   Create PceEmacs and open the *scratch* buffer.

emacs :-
    start_emacs,
    in_pce_thread((new(Scratch, emacs_buffer(@nil, '*scratch*')),
                   send(Scratch, open, tab))).

%!  emacs(+Location) is det.
%
%   Create PceEmacs and edit  Location.  Location   is  one  of  the
%   following, where File must be an atom   and Line and LinePos are
%   integers.
%
%     - File:Line:LinePos
%     - File:Line
%     - File

emacs(File:Line:LinePos) :-
    integer(Line),
    integer(LinePos),
    atom(File),
    !,
    start_emacs,
    new(Loc, source_location(File, Line)),
    send(Loc, attribute, linepos, LinePos),
    in_pce_thread(send(@emacs, goto_source_location, Loc, tab)).
emacs(File:Line) :-
    integer(Line),
    atom(File),
    !,
    start_emacs,
    in_pce_thread(send(@emacs, goto_source_location,
                       source_location(File, Line), tab)).
emacs(File) :-
    atom(File),
    !,
    start_emacs,
    in_pce_thread(send(@emacs, goto_source_location,
                       source_location(File), tab)).
emacs(File) :-
    domain_error(location, File).


%!  emacs_toplevel is det.
%
%   Prepare to run PceEmacs as a stand-alone executable.

emacs_toplevel :-
    send(@pce, trap_errors, @off),
    current_prolog_flag(argv, Files),
    (   Files = [_|_]
    ->  start_emacs,
        maplist(make_buffer, Files, [B0|_]),
        send(B0, open)
    ;   emacs
    ).

make_buffer(File, Buffer) :-
    new(Buffer, emacs_buffer(File)).
