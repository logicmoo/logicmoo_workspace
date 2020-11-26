/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, CWI Amsterdam
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

:- module(emacs_chrome_server,
          [ emacs_chrome_server/0
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(pce)).

/** <module> Emacs Chrome Server

@see https://github.com/stsquad/emacs_chrome/tree/master/servers
*/

%!  emacs_chrome_server
%
%   Start a PceEmacs server for  the   _Edit  With Emacs_ Chrome/Firefox
%   extension.

emacs_chrome_server :-
    start_emacs,
    http_server(http_dispatch,
                [ port(localhost:9292)
                ]).


:- http_handler(root(edit),   edit,   [method(post), spawn]).
:- http_handler(root(status), status, [method(get)]).

edit(Request) :-
    http_read_data(Request, Data, [to(string)]),
    edit_text(Data, NewData),
    format('Content-type: text/plain~n~n'),
    format('~s', [NewData]).

status(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Ready~n').

edit_text(From, To) :-
    tmp_file_stream(utf8, FileName, Stream),
    format(Stream, '~s', [From]),
    close(Stream),
    edit_file(FileName),
    setup_call_cleanup(
        open(FileName, read, In),
        read_string(In, _, To),
        close(In)),
    delete_file(FileName).

:- pce_global(@emacs_chrome_server_method,
              new(chain(send_method(unlink_to, new(vector),
                                    and(message(@receiver?from, free),
                                        message(@receiver, free))),
                        send_method(unlink_from, new(vector),
                                    and(message(@receiver?to, free),
                                        message(@receiver, free)))))).

:- pce_begin_class(emacs_chrome_proxy, object,
                   "Track emacs").

variable(client, int, get, "Client thread").

initialise(P, Client:int) :->
    "Create from thread id"::
    send_super(P, initialise),
    send(P, slot, client, Client).

unlink(P) :->
    get(P, client, Id),
    thread_send_message(Id, emacs_chrome_server_done),
    send_super(P, unlink).

:- pce_end_class(emacs_chrome_proxy).


%!  edit_file(+File)
%
%   Edit a file using PceEmacs,  waiting  for   the  user  to  close the
%   editor.

edit_file(File) :-
    thread_self(Me),
    thread_property(Me, id(MeID)),
    in_pce_thread(create_editor(File, MeID)),
    thread_get_message(emacs_chrome_server_done).

create_editor(File, MeID) :-
    new(Proxy, emacs_chrome_proxy(MeID)),
    new(B, emacs_buffer(File)),
    get(B, open, tab, Frame),
    send(Frame, expose),
    get(Frame, editor, Editor),
    new(H, hyper(Proxy, Editor, editor, server)),
    send(H, send_method, @emacs_chrome_server_method).
