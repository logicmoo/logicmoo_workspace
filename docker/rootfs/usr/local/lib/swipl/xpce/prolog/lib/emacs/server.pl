/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2012, University of Amsterdam
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

:- module(emacs_server,
          [
          ]).
:- use_module(library(pce)).
:- require([ file_base_name/2
           , file_directory_name/2
           , term_to_atom/2
           ]).

/** <module> Remote control for PceEmacs

This module allows for controlling PceEmacs   from  the (Unix) shell. It
creates a Unix domain socket  in  the   user's  home  directory. A shell
script =|edit.sh|= is available from library('emacs/edit.sh').
*/


:- pce_global(@emacs_server, make_emacs_server).
:- pce_global(@emacs_server_address, make_emacs_server_address).
:- pce_global(@emacs_server_method,
              new(chain(send_method(unlink_to, new(vector),
                                    and(message(@receiver?from, free),
                                        message(@receiver, free))),
                        send_method(unlink_from, new(vector),
                                    and(message(@receiver?to, free),
                                        message(@receiver, free)))))).

make_emacs_server_address(F) :-
    (   get(@pce, environment_variable, 'DISPLAY', Display),
        atom_codes(Display, Codes),
        phrase(local_display(Local), Codes, _)
    ->  true
    ;   get(@pce, hostname, Local)
    ),
    atom_concat('emacs_server.', Local, ServerRel),
    get(@pce, application_data, Dir),
    get(Dir, file, ServerRel, FA),
    get(FA, name, FileName),
    new(F, file(FileName)).

local_display(N) -->
    ":", digits(D),
    !,
    { number_codes(N, D) }.

digits([H|T]) --> [H], { between(0'0, 0'9, H) }, !, digits(T).
digits([]) --> "".


make_emacs_server(Socket) :-
    new(Socket, socket(@emacs_server_address)),
    send(Socket, input_message,
         message(@prolog, server_action_atom, @receiver, @arg1)),
    send(Socket, send_method,
         send_method(end_of_file, new(vector),
                     message(@receiver, free))).


server_action_atom(Socket, Action) :-
    get(Action, value, Atom),
    term_to_atom(Term, Atom),
    (   server_action(Term, Socket)
    ->  true
    ;   send(Socket, format, 'Request failed: %s\n', Action),
        send(Socket, free)
    ).


server_action((A,B), Socket) :-
    !,
    server_action(A, Socket),
    server_action(B, Socket).
server_action(edit(File), Socket) :-
    !,
    server_action(edit(File, [], [], wait), Socket).
server_action(edit(File, Line), Socket) :-
    !,
    server_action(edit(File, Line, [], wait), Socket).
server_action(edit(File, Line, CharPos), Socket) :-
    !,
    server_action(edit(File, Line, CharPos, wait), Socket).
server_action(edit(File, Line, CharPos, Wait), Socket) :-
    !,
    new(B, emacs_buffer(File)),
    get(B, open, tab, Frame),
    send(Frame, expose),
    get(Frame, editor, Editor),
    (   Wait == wait
    ->  new(H, hyper(Socket, Editor, editor, server)),
        send(H, send_method, @emacs_server_method)
    ;   true
    ),
    send(B, check_modified_file),
    (   Line == []
    ->  true
    ;   send(Editor, goto_line, Line)
    ),
    (   CharPos == []
    ->  true
    ;   send(Editor, column, CharPos)
    ).
server_action(gdb(File, Pid), Socket) :-
    !,
    file_directory_name(File, Dir),
    file_base_name(File, Exe),
    new(X, emacs_gdb_buffer(Exe, Pid)),
    get(X, process, Process),
    (   Process \== @nil,
        get(Process, status, inactive)
    ->  send(Process, directory, Dir),
        send(X, directory, Dir)
    ;   true
    ),
    new(W, emacs_frame(X)),
    get(W, editor, Editor),
    new(H, hyper(Socket, Editor, editor, server)),
    send(H, send_method, @emacs_server_method),
    send(X, start_process),
    send(X, open, tab).
server_action(gdb(File), Socket) :-
    !,
    server_action(gdb(File, @default), Socket).

server_action(Cmd, Socket) :-
    Cmd =.. [Sel|Args],
    Msg =.. [send, Socket, hyper_send, editor, Sel | Args],
    Msg.
