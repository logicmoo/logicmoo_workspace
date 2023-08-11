/*  Part of SWI-Prolog

    Author:        Jan Wielemaker & Steve Prior
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2022, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(prolog_server,
          [ prolog_server/2             % +Port, +Options
          ]).

:- autoload(library(lists),[member/2]).
:- autoload(library(socket),
	    [ tcp_socket/1,
	      tcp_setopt/2,
	      tcp_bind/2,
	      tcp_listen/2,
	      tcp_accept/3,
	      tcp_open_socket/3,
	      tcp_host_to_address/2
	    ]).


%!  prolog_server(?Port, +Options)
%
%   Create a TCP/IP based server  on  the   given  Port,  so you can
%   telnet into Prolog and run an  interactive session. This library
%   is intended to provide access for   debugging  and management of
%   embedded servers.
%
%   Currently defined options are:
%
%           * allow(IP)
%           Allow access from IP, a term of the format ip(A,B,C,D).
%           Multiple of such terms can exist and access is granted
%           if the peer IP address unifies to one of them.  If no
%           allow option is provided access is only granted from
%           ip(127,0,0,1) (localhost).
%
%   For example:
%
%           ==
%           ?- prolog_server(4000, []).
%
%           % netcat -N localhost 4000
%           Welcome to the SWI-Prolog server on thread 3
%
%           1 ?-
%           ==
%
%   @bug  As  the connection  does  not  involve a  terminal,  command
%   history and  completion are  not provided. Neither  are interrupts
%   (Control-C).  The Prolog shell can be terminated if `netcat` shuts
%   down the socket on ^D (using the `-N` option).  Otherwise one must
%   enter the command "end_of_file."
%   @see The  add-on `libssh` provides  an embedded SSH  server.  This
%   provides encryption  as well as  a _pseudo terminal_ for  a better
%   user experience.

prolog_server(Port, Options) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    thread_create(server_loop(ServerSocket, Options), _,
                  [ alias(prolog_server)
                  ]).

server_loop(ServerSocket, Options) :-
    tcp_accept(ServerSocket, Slave, Peer),
    tcp_open_socket(Slave, InStream, OutStream),
    set_stream(InStream, close_on_abort(false)),
    set_stream(OutStream, close_on_abort(false)),
    tcp_host_to_address(Host, Peer),
    (   Postfix = []
    ;   between(2, 1000, Num),
        Postfix = [-, Num]
    ),
    atomic_list_concat(['client@', Host | Postfix], Alias),
    catch(thread_create(
              service_client(InStream, OutStream, Peer, Options),
              _,
              [ alias(Alias),
                detached(true)
              ]),
          error(permission_error(create, thread, Alias), _),
          fail),
    !,
    server_loop(ServerSocket, Options).

service_client(InStream, OutStream, Peer, Options) :-
    allow(Peer, Options),
    !,
    thread_self(Id),
    set_prolog_IO(InStream, OutStream, OutStream),
    set_stream(InStream, tty(true)),
    set_prolog_flag(tty_control, false),
    current_prolog_flag(encoding, Enc),
    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),
    format(user_error,
           'Welcome to the SWI-Prolog server on thread ~w~n~n',
           [Id]),
    call_cleanup(prolog,
                 ( close(InStream, [force(true)]),
                   close(OutStream, [force(true)]))).
service_client(InStream, OutStream, _, _):-
    thread_self(Id),
    format(OutStream, 'Go away!!~n', []),
    close(InStream),
    close(OutStream),
    thread_detach(Id).


allow(Peer, Options) :-
    (   member(allow(Allow), Options)
    *-> Peer = Allow,
        !
    ;   Peer = ip(127,0,0,1)
    ).
