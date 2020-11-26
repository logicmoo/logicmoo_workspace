/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2016, University of Amsterdam,
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

:- module(http_host,
          [ http_public_url/2,          % +Request, -PublicURL
            http_public_host_url/2,     % +Request, -HostURL
            http_public_host/4,         % +Request, -Host, -Port, +Options
                                        % deprecated
            http_current_host/4         % +Request, -Host, -Port, +Options
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- setting(http:public_host, atom, '',
           'Name the outside world can use to contact me').
:- setting(http:public_port, integer, 80,
           'Port on the public server').
:- setting(http:public_scheme, oneof([http,https]), http,
           'Default URL scheme to use').

:- predicate_options(http_public_host/4, 4, [global(boolean)]).
:- predicate_options(http_current_host/4, 4,
                     [pass_to(http_public_host/4, 4)]).

/** <module> Obtain public server location

This library finds the public address of the running server. This can be
used to construct URLs that are visible   from anywhere on the internet.
This module was introduced to  deal  with  OpenID,  where  a  request is
redirected to the OpenID server, which in   turn redirects to our server
(see http_openid.pl).

The address is  established  from   the  settings `http:public_host` and
`http:public_port` if provided. Otherwise it is deduced from the request.
*/

%!  http_public_url(+Request, -URL) is det.
%
%   True when URL is  an  absolute   URL  for  the  current request.
%   Typically, the login page should redirect   to this URL to avoid
%   losing the session.

http_public_url(Request, URL) :-
    http_public_host_url(Request, HostURL),
    option(request_uri(RequestURI), Request),
    atomic_list_concat([HostURL, RequestURI], URL).


%!  http_public_host_url(+Request, -URL) is det.
%
%   True when URL is the public  URL   at  which  this server can be
%   contacted.   This   value   is   not   easy   to   obtain.   See
%   http_public_host/4 for the hardest  part:   find  the  host and
%   port.

http_public_host_url(Request, URL) :-
    http_public_host(Request, Host, Port,
                      [ global(true)
                      ]),
    setting(http:public_scheme, Scheme),
    (   scheme_port(Scheme, Port)
    ->  format(atom(URL), '~w://~w', [Scheme, Host])
    ;   format(atom(URL), '~w://~w:~w', [Scheme, Host, Port])
    ).

scheme_port(http, 80).
scheme_port(https, 443).

%!  http_public_host(?Request, -Hostname, -Port, +Options) is det.
%
%   Current global host and port of the HTTP server.  This is the
%   basis to form absolute address, which we need for redirection
%   based interaction such as the OpenID protocol.  Options are:
%
%     * global(+Bool)
%     If =true= (default =false=), try to replace a local hostname
%     by a world-wide accessible name.
%
%   This predicate performs the following steps to find the host and
%   port:
%
%     1. Use the settings =http:public_host= and =http:public_port=
%     2. Use =X-Forwarded-Host= header, which applies if this server
%        runs behind a proxy.
%     3. Use the =Host= header, which applies for HTTP 1.1 if we are
%        contacted directly.
%     4. Use gethostname/1 to find the host and
%        http_current_server/2 to find the port.
%
%   @param  Request is the current request.  If it is left unbound,
%           and the request is needed, it is obtained with
%           http_current_request/1.

http_public_host(_Request, Host, Port, _) :-
    setting(http:public_host, PublicHost), PublicHost \== '',
    !,
    Host = PublicHost,
    setting(http:public_port, Port).
http_public_host(Request, Host, Port, Options) :-
    (   var(Request)
    ->  http_current_request(Request)
    ;   true
    ),
    (   memberchk(x_forwarded_host(Forwarded), Request)
    ->  Port = 80,
        primary_forwarded_host(Forwarded, Host)
    ;   memberchk(host(Host0), Request),
        (   option(global(true), Options, false)
        ->  global_host(Host0, Host)
        ;   Host = Host0
        ),
        option(port(Port), Request, 80)
    ),
    !.
http_public_host(_Request, Host, Port, _Options) :-
    gethostname(Host),
    http_current_server(_:_Pred, Port).

%!  http_current_host(?Request, -Hostname, -Port, +Options) is det.
%
%   @deprecated     Use http_public_host/4 (same semantics)

http_current_host(Request, Hostname, Port, Options) :-
    http_public_host(Request, Hostname, Port, Options).

%!  primary_forwarded_host(+Spec, -Host) is det.
%
%   x_forwarded host contains multiple hosts separated   by  ', ' if
%   there are multiple proxy servers in   between.  The first one is
%   the one the user's browser knows about.

primary_forwarded_host(Spec, Host) :-
    sub_atom(Spec, B, _, _, ','),
    !,
    sub_atom(Spec, 0, B, _, Host).
primary_forwarded_host(Host, Host).


%!  global_host(+HostIn, -Host)
%
%   Globalize a hostname. Used if we need  to pass our hostname to a
%   client and expect the client to be   able to contact us. In this
%   case we cannot use a  name  such   as  `localhost'  or the plain
%   hostname of the machine. We assume   (possibly  wrongly) that if
%   the host contains a '.', it is globally accessible.
%
%   If the heuristics used by  this   predicate  do not suffice, the
%   setting http:public_host can be used to override.

global_host(_, Host) :-
    setting(http:public_host, PublicHost), PublicHost \== '',
    !,
    Host = PublicHost.
global_host(localhost, Host) :-
    !,
    gethostname(Host).
global_host(Local, Host) :-
    sub_atom(Local, _, _, _, '.'),
    !,
    Host = Local.
global_host(Local, Host) :-
    tcp_host_to_address(Local, IP),
    tcp_host_to_address(Host, IP).


