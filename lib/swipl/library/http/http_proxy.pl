/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Matt Lilley
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
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

:- module(http_proxy, []).
:- use_module(library(http/http_header)).
:- use_module(library(socket)).

/** <module> Use HTTP network proxies

This  module  provides  a  plugin  for   tcp_connect/3  to  realise  TCP
connections through an HTTP proxy that   supports the HTTP 1.1 =CONNECT=
method.
*/

                 /*******************************
                 *            PROXY             *
                 *******************************/

%!  socket:try_proxy(+Proxy, +Address, -Socket, -StreamPair)
%
%   Connection is via an HTTP proxy   for  socket: Use HTTP CONNECT.
%   Note that most proxies will only  support this for connecting on
%   port 443
%
%   @arg Proxy is of the form proxy(Host, Port)
%   @error proxy_error(Message) if the proxy connection is
%   not successful.

socket:try_proxy(proxy(Host, Port), Address, Socket, StreamPair) :-
    !,
    tcp_socket(Socket),
    tcp_connect(Socket, Host:Port, StreamPair),
    catch(negotiate_http_connect(StreamPair, Address),
          Error,
          ( close(StreamPair, [force(true)]),
            throw(Error)
          )).

negotiate_http_connect(StreamPair, Address) :-
    format(StreamPair, 'CONNECT ~w HTTP/1.1\r\n\r\n', [Address]),
    flush_output(StreamPair),
    http_read_reply_header(StreamPair, Header),
    memberchk(status(_, Status, Message), Header),
    (   Status == ok
    ->  true
    ;   throw(error(proxy_error(Message), _))
    ).

