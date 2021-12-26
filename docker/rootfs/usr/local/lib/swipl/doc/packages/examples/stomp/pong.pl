:- module(pong,
          []).
:- use_module(library(stomp)).
:- use_module(library(main)).
:- use_module(library(debug)).

:- initialization(main, main).

/** <module> The Pong client

This client is the Pong end of the PingPong. It listens on `/queue/pong`
and echos all each `pong:Count` with a `ping:Count` on `/queue/ping`.

This client is setup to automatically   reconnect.  This implies that if
the connection to the server dies it will try to reconnect.

@license This code is in the public domain
*/

% configure debugging levels
:- set_prolog_flag(message_context, [thread,time]),
%  prolog_ide(thread_monitor),
   debug(pong(connection)),
%  debug(pong(heartbeat)),
%  debug(stomp(heartbeat)),
%  debug(stomp(receive)),
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
                     [ reconnect(true),
                       connect_timeout(infinite)
                     ]),
    stomp_connect(Connection).

on_frame(connected, Connection, _Header, _Body) :-
    debug(pong(connection), 'Extablised connection', []),
    stomp_subscribe(Connection, '/queue/pong', 0, _{}).
on_frame(disconnected, _Connection, _Header, _Body) :-
    debug(pong(connection), 'Lost connection', []).
on_frame(heartbeat_timeout, _Connection, _Header, _Body) :-
    debug(pong(connection), 'heartbeat timeout', []).
on_frame(heartbeat, _Connection, _Header, _Body) :-
    debug(pong(heartbeat), 'heartbeat', []).
on_frame(message, Connection, _Header, _{pong:Count}) :-
    debug(pong, 'Got ~D', [Count]),
    stomp_send_json(Connection, '/queue/ping', _{}, _{ping:Count}).


