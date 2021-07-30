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

:- module(client,
          [ client/0
          ]).
:- autoload(library(readutil),[read_line_to_codes/2]).
:- autoload(library(socket),[tcp_connect/3]).
:- autoload(library(ssl),[ssl_context/3,ssl_negotiate/5]).

client :-
    ssl_context(client, SSL,
             [ host('localhost'),
               cert_verify_hook(cert_verify),
               cacert_file('etc/demoCA/cacert.pem'),
               certificate_file('etc/client/client-cert.pem'),
               key_file('etc/client/client-key.pem'),
               close_parent(true),
               close_notify(true),
%                  password('apenoot2'),
               pem_password_hook(get_client_pwd)
             ]),
    Port = 1111,
    tcp_connect(localhost:Port, StreamPair, []),
    stream_pair(StreamPair, Read, Write),
    catch(ssl_negotiate(SSL, Read, Write, SSLRead, SSLWrite),
          E,
          ( close(StreamPair), throw(E))),
    client_loop(SSLRead, SSLWrite).

client_loop(In, Out) :-
    write_server(In, Out),
    write_server(In, Out),
    write_server(In, Out),
    call_cleanup(close(In), close(Out)).

write_server(In, Out) :-
    format(Out, 'Hello~n', ''),
    flush_output(Out),
    read_line_to_codes(In, Line),
    (   Line == end_of_file
    ->  true
    ;   format('Got ~s~n', [Line])
    ).

user:get_client_pwd(_SSL, apenoot2) :-
    format('Returning password from client passwd hook~n').

cert_verify(_SSL, Certificate, _AllCerts, _FirstCert, Error) :-
    format('Handling detailed certificate verification~n'),
    format('Certificate: ~w, error: ~w~n', [Certificate, Error]),
    format('Client accepts the server certificate~n').
