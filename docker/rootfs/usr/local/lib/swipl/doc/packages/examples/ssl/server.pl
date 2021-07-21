/*  Part of SWI-Prolog

    Author:        Jan van der Steen and Jan Wielemaker
    E-mail:        J.van.der.Steen@diff.nl and jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, SWI-Prolog Foundation
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

:- module(server,
          [ server/0
          ]).
:- autoload(library(debug),[debug/1,debug/3]).
:- autoload(library(readutil),[read_line_to_codes/2]).
:- autoload(library(socket),
	    [ tcp_socket/1,
	      tcp_setopt/2,
	      tcp_bind/2,
	      tcp_listen/2,
	      tcp_accept/3,
	      tcp_open_socket/3,
	      tcp_close_socket/1
	    ]).
:- autoload(library(ssl),[ssl_context/3,ssl_negotiate/5]).

:- debug(connection).

server :-
    ssl_context(server, SSL,
                [ peer_cert(true),
                  cacert_file('etc/demoCA/cacert.pem'),
                  certificate_file('etc/server/server-cert.pem'),
                  key_file('etc/server/server-key.pem'),
                  cert_verify_hook(get_cert_verify),
                  close_notify(true),
                  % password('apenoot1'),
                  pem_password_hook(get_server_pwd)
                ]),
    Port = 1111,
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, localhost:Port),
    tcp_listen(Socket, 5),
    thread_create(server_loop(SSL, Socket), _, []).

server_loop(SSL, Server) :-
    tcp_accept(Server, Socket, Peer),
    debug(connection, 'Connection from ~p', [Peer]),
    setup_call_cleanup(tcp_open_socket(Socket, Read, Write),
                       handle_client(SSL, Read, Write),
                       tcp_close_socket(Socket)),
    server_loop(SSL, Server).

handle_client(SSL, Read, Write) :-
    catch(ssl_negotiate(SSL, Read, Write, SSLRead, SSLWrite),
          Exception,
          true),
    (   nonvar(Exception)
    ->  format("Exception during negotation: ~w~n", [Exception])
    ;   copy_client(SSLRead, SSLWrite),
        call_cleanup(close(SSLRead), close(SSLWrite))
    ).

copy_client(In, Out) :-
    read_line_to_codes(In, Line),
    (   Line == end_of_file
    ->  true
    ;   format('Got ~s~n', [Line]),
        format(Out, '~s~n', [Line]),
        flush_output(Out),
        copy_client(In, Out)
    ).

get_server_pwd(_SSL, apenoot1) :-
    format('Returning password from server passwd hook~n').

get_cert_verify(_SSL, Certificate, _AllCerts, _FirstCert, Error) :-
    format('Handling detailed certificate verification~n'),
    format('Certificate: ~w, error: ~w~n', [Certificate, Error]),
    format('Server accepts the client certificate~n').

