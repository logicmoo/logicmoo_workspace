/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(stress_server,
          []).
:- use_module(library(stomp)).
:- use_module(library(main)).
:- use_module(library(debug)).

:- initialization(main, main).

/** <module> The stress test server

This  is  a  simple  echo  server   with  acknowledgement  enabled.  See
stress_client.pl for a description of the test scenario.
*/

% configure debugging levels
:- set_prolog_flag(message_context, [thread,time]),
   debug(stomp(connection)).

main(_) :-
    connect(_),
    thread_get_message(_).

connect(Connection) :-
    stomp_connection('127.0.0.1':32772,
                     '/',
                     _{'heart-beat': '5000,5000',
                       login: guest,
                       passcode: guest
                      },
                     on_frame, Connection,
                     [ reconnect(true)
                     ]),
    stomp_connect(Connection).

on_frame(connected, Connection, _Header, _Body) :-
    debug(pong(connection), 'Establised connection', []),
    stomp_subscribe(Connection, '/queue/stress.requests', 0,
                    _{ack:'client-individual'}).
on_frame(disconnected, _Connection, _Header, _Body) :-
    debug(pong(connection), 'Lost connection', []).
on_frame(heartbeat_timeout, _Connection, _Header, _Body) :-
    debug(pong(connection), 'heartbeat timeout', []).
on_frame(heartbeat, _Connection, _Header, _Body) :-
    debug(pong(heartbeat), 'heartbeat', []).
on_frame(message, Connection, Header, Message) :-
    debug(pong, 'Got ~p', [Message]),
    stomp_send_json(Connection, '/queue/stress.replies', _{}, Message),
    stomp_ack(Connection, Header).


