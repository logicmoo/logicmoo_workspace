/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2006-2020, University of Amsterdam
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

:- module(emacs_dde_server,
          [ start_emacs_dde_server/1,   % +Force
            win_register_emacs/0        % +Externsion
          ]).
:- use_module(library(pce)).
:- require([debug/3]).

/** <module> Register PceEmacs with the Windows DDE services

This module registers the DDE service   =PceEmacs= that allows accessing
PceEmacs from the Windows shell. The access   points  are dummy calls if
DDE is nor provided.
*/

%!  start_emacs_dde_server(+Force) is det.
%
%   If there is no DDE server, register it as =PceEmacs= using the
%   topic =control=.

:- if(current_predicate(open_dde_conversation/3)).
:- use_module(library(dde)).

start_emacs_dde_server(_) :-
    dde_current_service('PceEmacs', control),
    debug(emacs(server), 'PceEmacs DDE server is already running', []),
    !.
start_emacs_dde_server(true) :-
    catch(close_other_dde_server, _, fail),
    fail.
start_emacs_dde_server(false) :-
    get_time(T0),
    (   catch(ping_other_dde_server, _, fail)
    ->  Alive = alive
    ;   Alive = dead
    ),
    !,
    get_time(T1),
    _0T is T1-T0,
    debug(emacs(server), 'Remote DDE server is ~w (~3f sec)', [Alive, _0T]),
    Alive == alive,
    ignore(send(@emacs, report, status, 'Server on other PceEmacs')).
start_emacs_dde_server(_) :-
    dde_register_service('PceEmacs'(control, Item),
                         handle_request(Item)).

close_other_dde_server :-
    setup_call_cleanup(open_dde_conversation('PceEmacs', control, Handle),
                       dde_execute(Handle, 'close-server'),
                       close_dde_conversation(Handle)),
    send(@emacs, report, status, 'Closed server on other PceEmacs').

ping_other_dde_server :-
    open_dde_conversation('PceEmacs', control, Handle),
    !,
    close_dde_conversation(Handle).


handle_request(Item) :-
    atom_concat('edit ', WinFile, Item),
    !,
    prolog_to_os_filename(File, WinFile),
    new(B, emacs_buffer(File)),
    send(B, open, tab),
    send(B, check_modified_file).
handle_request('close-server') :-
    dde_unregister_service('PceEmacs'),
    send(@emacs, report, status, 'Closed DDE server').
handle_request(Item) :-
    format(user_error, 'PceEmacs DDE server: unknown request: ~q', [Item]),
    fail.

:- else.

start_emacs_dde_server(_).

:- endif.

:- if(current_predicate(shell_register_dde/6)).

win_register_emacs :-
    current_prolog_flag(executable, Me),
    shell_register_dde('prolog.type', edit,
                       'PceEmacs', control, 'edit %1', Me).

:- else.

win_register_emacs.

:- endif.
