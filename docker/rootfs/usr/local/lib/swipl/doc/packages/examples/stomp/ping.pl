:- module(ping,
          []).
:- use_module(library(stomp)).
:- use_module(library(main)).
:- use_module(library(option)).

/** <module> A simple demo and performance evaluation

To run, update the connection details below in this file and in pong.pl.
Now run in one terminal

    swipl pong.pl

and in another one e.g.,

    time swipl ping.pl --count=1000

Timing on Ubuntu 20.04  on  an  AMD3950X   system  is  about  0.4ms  per
iteration. Note that each iteration implies   two  exchanges as the pong
part just echos without decrementing. This   settles  a send and receive
and about 0.2ms. The server  is  RabbitMQ   on  Ubuntu  using  the STOMP
plugin.

Note    that    this     compare     fairly      poorly     to     e.g.,
[rclswi](https://github.com/SWI-Prolog/rclswi),    using    the     ROS2
middleware which performs roughly 7 times better.

@license This code is in the public domain
*/

:- initialization(main, main).

main(Argv) :-
    connect(Connection),
    argv_options(Argv, _, Options),
    option(count(Count), Options, 10),
    stomp_send_json(Connection, '/queue/pong', _{}, _{pong:Count}),
    thread_get_message(_).


connect(Connection) :-
    stomp_connection('127.0.0.1':32772,
                     '/',
                     _{'heart-beat': '5000,5000',
                       login: guest,
                       passcode: guest
                      },
                     on_frame, Connection),
    stomp_connect(Connection).

on_frame(connected, Connection, _Header, _Body) :-
    stomp_subscribe(Connection, '/queue/ping', 0, _{}).
on_frame(message, Connection, _Header, _{ping:Count}) :-
    debug(ping, 'Got ~D', [Count]),
    (   Count == 0
    ->  thread_send_message(main, done)
    ;   Count1 is Count-1,
        stomp_send_json(Connection, '/queue/pong', _{}, _{pong:Count1})
    ).


