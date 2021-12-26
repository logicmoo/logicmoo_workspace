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

:- module(stress_client,
          []).
:- use_module(library(stomp)).
:- use_module(library(main)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(thread)).

:- initialization(main, main).

/** <module> Stress testing client

This  library,  together  with   stress_server.pl  and  ./server-loop.sh
implements stress testing reliable communication.

The server is a simple echo   server.  The script `server-loop.sh` kills
the server every 0.2 seconds. The test   starts one or more instances of
`server-loop.sh`, simulating redundant frequently crashing servers. This
clients creates one or more threads that  connect to the broker and send
a series of messages to the `stress.requests`   queue. It listens on the
`stress.replies` queue and clears every message  for which it receives a
reply. At the end it reports teh  number   of  messages  it sent and for
which it got no reply (this should not happen).

Running this occasionally prints  "Unexpected   (multiple?)  replies for
..".  That  is  fine.  If  a  server  is  killed  between  replying  and
acknowledging the reply the request is   redelivered  and a second reply
comes in.

To run the stress test:

  - Make sure a STOMP server (tested RabbitMQ) runs at port 32772 on
    `localhost` and is accessible as user `guest`, password `guest`.
  - In three terminals start `./server-loop.sh`
  - In a fourth terminal run e.g.

	swipl stress_client.pl --threads=8 --count=100000
*/

% configure debugging levels
:- set_prolog_flag(message_context, [thread,time]),
%  debug(stress(send)),
%  debug(stress(receive)),
   debug(stress(progress)),
   debug(stress(wait)),
   debug(stomp(connection)),
   true.

:- dynamic
    sent/3.

%!  main(+Argv)
%
%   Run the test client.  Options processed:
%
%     - ``--threads=N`` <br>
%       Create N client threads.
%     - ``--count=N`` <br>
%       Each thread sends N messages.
%     - ``--tmon`` <br>
%       Run the XPCE _thread monitor_ during the test and fall back to
%       the toplevel after completion.
%     - ``--profile`` <br>
%       Run profile/1 for the first client.

main(Argv) :-
    argv_options(Argv, _Positional, Options),
    (   option(tmon(true), Options)
    ->  prolog_ide(thread_monitor)
    ;   true
    ),
    option(threads(NThreads), Options, 1),
    length(Done, NThreads),
    set_flag(received, 0),
    set_flag(client, 0),
    concurrent_maplist(test_client_(Options), Done),
    (   option(tmon(true), Options)
    ->  cli_enable_development_system
    ;   true
    ).

test_client_(Options, Pass) :-
    option(profile(true), Options),
    flag(client, 0, 1),
    !,
    profile(test_client(Pass, Options)).
test_client_(Options, Pass) :-
    test_client(Pass, Options).

test_client(Pass, Options) :-
    connect(Connection),
    option(count(Count), Options, 10),
    thread_id(Id),
    requests(Connection, Id, Count),
    debug(stress(progress), 'All sent, waiting for final replies', []),
    (   wait_receive(Id)
    ->  Pass = true
    ;   aggregate_all(count, sent(Id, _, _), Missed),
        format('Thread ~p: no answer: for ~D requests~n', [Id, Missed]),
        Pass = false
    ),
    debug(stress(progress), 'All done', []),
    stomp_disconnect(Connection, _{}),
    stomp_destroy_connection(Connection).

requests(Connection, Id, N) :-
    forall(between(1, N, I),
           send_msg(Connection, Id, I)).

send_msg(Connection, Id, Content) :-
    stomp_send_json(Connection, '/queue/stress.requests',
                    _{}, Tag{sender:Id, content:Content}),
    get_time(Now),
    debug(stress(send), 'Sent ~p', [Tag{sender:Id, content:Content}]),
    assertz(sent(Id, Content, Now)).

%!  wait_receive(+Id) is semidet.
%
%   Wait for all replies for client Id to  be received and succeed if we
%   got all of them. This uses  thread_wait/2   to  watch  for sent/3 to
%   drain. After a 10 seconds timeout we  check whether any messages are
%   still being received and continue waiting if this is the case.
%
%   In other words, if not all  replies   have  been received and no new
%   reply was received for 10 seconds we assume the reply was lost.

wait_receive(Id) :-
    get_flag(received, R0),
    (   thread_wait(\+ sent(Id,_,_),
                    [ wait_preds([sent/3]),
                      timeout(10)
                    ])
    ->  debug(stress(wait), '[~w]: received all', [Id])
    ;   get_flag(received, R1),
        R1 > R0
    ->  Rec is R1-R0,
        debug(stress(wait), '[~w]: received ~D~n', [Id, Rec]),
        wait_receive(Id)
    ;   debug(stress(wait), '[~w]: received none~n', [Id]),
        fail
    ).

connect(Connection) :-
    stomp_connection('127.0.0.1':32772,
                     '/',
                     _{'heart-beat': '5000,5000',
                       login: guest,
                       passcode: guest
                      },
                     on_frame, Connection,
                     [ reconnect(true),
                       json_options([ value_string_as(atom)
                                    ])
                     ]),
    stomp_connect(Connection).

on_frame(connected, Connection, _Header, _Body) :-
    debug(pong(connection), 'Extablised connection', []),
    stomp_subscribe(Connection, '/queue/stress.replies', 0,
                    _{ack: 'client-individual'}).
on_frame(disconnected, _Connection, _Header, _Body) :-
    debug(pong(connection), 'Lost connection', []).
on_frame(heartbeat_timeout, _Connection, _Header, _Body) :-
    debug(pong(connection), 'heartbeat timeout', []).
on_frame(heartbeat, _Connection, _Header, _Body) :-
    debug(pong(heartbeat), 'heartbeat', []).
on_frame(error, _Connection, Header, Body) :-
    print_message(error, stomp_error(Header, Body)).
on_frame(message, Connection, Header, _{sender:Id, content:Content}) :-
    get_time(Now),
    received(Id, Content, Now),
    stomp_ack(Connection, Header).

received(Id, Content, Received) :-
    flag(received, N, N+1),
    (   N mod 1000 =:= 0
    ->  thread_id(Me),
        format(user_error, '\r[~w] Received ~D', [Me, N])
    ;   true
    ),
    (   retract(sent(Id, Content, Sent))
    ->  Time is Received-Sent,
        debug(stress(received), 'Got ~p in ~6f sec',
              [#{sender:Id, content:Content}, Time])
    ;   print_message(warning, not_sent(Id, Content))
    ).

thread_id(Id) :-
    thread_self(Me),
    (   atom(Me)
    ->  Id = Me
    ;   thread_property(Me, id(Id))
    ).

:- multifile prolog:message//1.

prolog:message(not_sent(Id, Content)) -->
    [ 'Unexpected (multiple?) replies for ~p ~p'-[Id, Content] ].
